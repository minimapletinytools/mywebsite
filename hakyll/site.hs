--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char             (toLower)
import           Data.List             (sort)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text             as T
import           Hakyll
import           Network.URI           (escapeURIString, isUnescapedInURI)
import           System.Directory      (findExecutable)
import           System.FilePath       (joinPath, splitDirectories,
                                        takeExtension)


--------------------------------------------------------------------------------
main :: IO ()
main = do
    -- ImageMagick is used to shrink oversized photos and strip their metadata
    -- (EXIF GPS etc). If it isn't installed, images are copied as-is.
    magick <- findImageMagick
    case magick of
        Nothing -> putStrLn "WARNING: ImageMagick not found, images will not be resized"
        Just _  -> return ()

    hakyllWith config $ do

        match "templates/newsite/*" $ compile templateCompiler

        match "css/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "favicon.ico" $ do
            route   idRoute
            compile copyFileCompiler

        -- photos get resized + stripped, everything else is copied untouched
        match ("images/**" .&&. resizable) $ do
            route   idRoute
            compile $ imageCompiler magick

        -- max 500px wide copies used by auto galleries, under gallery-500/
        match ("images/**" .&&. resizable) $ version "gallery" $ do
            route   $ customRoute (galleryPath . toFilePath)
            compile $ galleryImageCompiler magick

        match ("images/**" .&&. complement resizable) $ do
            route   idRoute
            compile copyFileCompiler

        -- posts
        match "posts/**" $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= autoGallery
                >>= loadAndApplyTemplate "templates/newsite/item-project.html" postCtx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/newsite/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- loadAll "posts/writing/**"
                makeItem ""
                    >>= loadAndApplyTemplate "templates/newsite/archive.html" (listCtx posts)
                    >>= loadAndApplyTemplate "templates/newsite/default.html" (listCtx posts)
                    >>= relativizeUrls

        create ["index.html"] $ do
            route idRoute
            compile $ do
                posts <- loadAll "posts/portfolio/**"
                makeItem ""
                    >>= loadAndApplyTemplate "templates/newsite/project-list.html" (listCtx posts)
                    >>= loadAndApplyTemplate "templates/newsite/default.html" (listCtx posts)
                    >>= relativizeUrls

        -- special pages
        match (fromList ["about.markdown", "cv.markdown", "microblog.markdown"]) $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/newsite/item-post.html" postCtx
                >>= loadAndApplyTemplate "templates/newsite/default.html" postCtx
                >>= relativizeUrls

        -- GitHub Pages serves 404.html for *any* missing path, so its urls
        -- must stay absolute (relative urls would break on nested paths).
        match "404.markdown" $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/newsite/item-post.html" postCtx
                >>= loadAndApplyTemplate "templates/newsite/default.html" postCtx

        create ["rss.xml"] $ do
            route idRoute
            compile $ do
                posts <- fmap (take 10) . recentFirst =<<
                    loadAllSnapshots "posts/**" "content"
                renderRss feedConfiguration feedContext posts


config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "_site"
    }


--------------------------------------------------------------------------------
-- Contexts

postCtx :: Context String
postCtx =
    listField "art"      defaultContext (recentFirst =<< stubs "posts/portfolio/art/**")      <>
    listField "games"    defaultContext (recentFirst =<< stubs "posts/portfolio/games/**")    <>
    listField "teaching" defaultContext (recentFirst =<< stubs "posts/teaching/**")           <>
    dateField "date" "%B %e, %Y"                                                             <>
    galleryContext                                                                           <>
    defaultContext

-- context for pages that list a set of posts (index, archive)
listCtx :: [Item String] -> Context String
listCtx posts =
    constField "title" "Archives"                    <>
    postCtx                                          <>
    listField "posts" postCtx (recentFirst posts)

-- empty items for each post matching the pattern; enough for title/url/date
stubs :: Pattern -> Compiler [Item String]
stubs pat = do
    ids <- getMatches pat
    return [Item i "" | i <- ids]

feedContext :: Context String
feedContext =
    constField "description" "" <>
    dateField "date" "%B %e, %Y" <>
    defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "_kitchen"
    , feedDescription = "_kitchen"
    , feedAuthorName  = "Peter Lu"
    , feedAuthorEmail = ""
    , feedRoot        = "https://pdlla.org"
    }


--------------------------------------------------------------------------------
-- Galleries
--
-- Posts can set `gallery:` in their metadata to either
--   * a whitespace separated list of paths relative to images/
--   * `auto <dir>` to include every image in <dir> (computed at build time)
--
-- Auto galleries are inserted into the post's `<div id="gallery">` if there is
-- one, otherwise they are appended to the end of the post.

galleryContext :: Context String
galleryContext = field "gallery" $ \item -> do
    g <- getMetadataField (itemIdentifier item) "gallery"
    case words <$> g of
        Just ("auto" : _) -> noResult "auto gallery is rendered into the body"
        Just ws@(_ : _)   -> return $ unlines [imgTag ("images/" ++ w) | w <- ws]
        _                 -> noResult "no gallery"

autoGallery :: Item String -> Compiler (Item String)
autoGallery item = do
    g <- getMetadataField (itemIdentifier item) "gallery"
    case words <$> g of
        Just ("auto" : dir : _) -> do
            ids <- getMatches (fromGlob (normalizeDir dir ++ "/*") .&&. hasNoVersion)
            let imgs  = sort [toFilePath i | i <- ids, isImage (toFilePath i)]
                html  = T.pack $ "\n" ++ unlines (map (imgTag . galleryVersion) imgs)
                body  = T.pack (itemBody item)
                (before, after) = T.breakOn "<div id=\"gallery\">" body
                body' = if T.null after
                    then body <> html
                    else before <> "<div id=\"gallery\">" <> html
                        <> T.drop (T.length "<div id=\"gallery\">") after
            return $ itemSetBody (T.unpack body') item
        _ -> return item

-- "../../../../images/foo/" -> "images/foo"
normalizeDir :: String -> String
normalizeDir = joinPath . dropWhile (`elem` ["..", ".", "/"]) . splitDirectories
             . reverse . dropWhile (== '/') . reverse

imgTag :: FilePath -> String
imgTag path = "<img src=\"" ++ escapeHtml (escapeURIString isUnescapedInURI ('/' : path))
           ++ "\" loading=\"lazy\" />"

-- auto galleries show the max-500px-wide copy if one is generated
galleryVersion :: FilePath -> FilePath
galleryVersion f
    | matches resizable (fromFilePath f) = galleryPath f
    | otherwise                          = f

-- images/foo/bar.jpg -> gallery-500/foo/bar.jpg
galleryPath :: FilePath -> FilePath
galleryPath = ("gallery-500/" ++) . drop (length ("images/" :: String))

isImage :: FilePath -> Bool
isImage f = map toLower (takeExtension f) `elem`
    [".gif", ".png", ".jpg", ".jpeg", ".bmp", ".webp"]


--------------------------------------------------------------------------------
-- Image processing

-- files that get run through ImageMagick
resizable :: Pattern
resizable = fromRegex "\\.([jJ][pP][eE]?[gG]|[pP][nN][gG])$"

findImageMagick :: IO (Maybe FilePath)
findImageMagick = do
    m <- findExecutable "magick"   -- ImageMagick 7
    case m of
        Just _  -> return m
        Nothing -> findExecutable "convert"  -- ImageMagick 6

-- Shrink anything bigger than 1600px on its longest side, apply EXIF rotation,
-- and strip all metadata except the color profile.
imageCompiler :: Maybe FilePath -> Compiler (Item LBS.ByteString)
imageCompiler = resizeCompiler "1600x1600>"

-- Same, but at most 500px wide (what auto galleries display)
galleryImageCompiler :: Maybe FilePath -> Compiler (Item LBS.ByteString)
galleryImageCompiler = resizeCompiler "500x>"

resizeCompiler :: String -> Maybe FilePath -> Compiler (Item LBS.ByteString)
resizeCompiler _    Nothing   = getResourceLBS
resizeCompiler size (Just im) = do
    ext <- getUnderlying >>= \i -> return (map toLower (takeExtension (toFilePath i)))
    let fmt = if ext == ".png" then "png" else "jpg"
        args = [ "-", "-auto-orient", "-resize", size, "+profile", "!icc,*" ]
            ++ (if fmt == "jpg" then ["-quality", "85"] else [])
            ++ [ fmt ++ ":-" ]
    getResourceLBS >>= withItemBody (unixFilterLBS im args)
