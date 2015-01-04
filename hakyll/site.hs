--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_, zipWithM_, liftM)
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import           Text.Pandoc (writerReferenceLinks)
import           Data.Char (toLower)
import           System.Locale (defaultTimeLocale)
import           Data.List (sortBy, intercalate, length)
import           Data.List.Split (chunksOf)
import           Data.Time.Clock (UTCTime)
import           System.FilePath (takeBaseName, takeFileName)
import           Data.Time.Format (parseTime)
import           Data.List (isPrefixOf, tails, findIndex)


import Debug.Trace

-- Allow for reference style links in markdown
pandocWriteOptions = defaultHakyllWriterOptions{ 
    writerReferenceLinks = True
}

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateCompiler
    match "templates/stanley/*" $ compile templateCompiler

    match "templates/stanley/assets/**" $ do
        route   idRoute
        compile copyFileCompiler


    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/portfolio/**" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/stanley/item-project.html" noTagsPostCtx
            >>= loadAndApplyTemplate "templates/stanley/default.html" noTagsPostCtx
            >>= relativizeUrls

    forM_ ["posts/blog/**","posts/*"] $ \p ->
        match p $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/stanley/item-post.html" noTagsPostCtx
                >>= loadAndApplyTemplate "templates/stanley/default.html" noTagsPostCtx
                >>= relativizeUrls


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            allPosts <- loadAll "posts/writing/*"
            makeItem ""
                >>= loadAndApplyTemplate "templates/stanley/archive.html" (archiveCtx allPosts)
                >>= loadAndApplyTemplate "templates/stanley/default.html" (archiveCtx allPosts)
                >>= relativizeUrls

    
    create ["work.html","index.html"] $ do
        route idRoute
        compile $ do
            allPosts <- loadAll "posts/portfolio/**"
            makeItem ""
                >>= loadAndApplyTemplate "templates/stanley/project-list.html" (archiveCtx allPosts)
                >>= loadAndApplyTemplate "templates/stanley/default.html" (archiveCtx allPosts)
                >>= relativizeUrls           

    -- Build special pages
    forM_ ["404.markdown","about.markdown"] $ \p ->
        match p $ do
            route   $ setExtension "html"
            compile $ do
                pandocCompilerWith defaultHakyllReaderOptions pandocWriteOptions
                    >>= loadAndApplyTemplate "templates/stanley/item-post.html" noTagsPostCtx
                    >>= loadAndApplyTemplate "templates/stanley/default.html" noTagsPostCtx
                    >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let indexCtx =
                    listField "posts" noTagsPostCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    noTagsPostCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/stanley/default.html" indexCtx
                >>= relativizeUrls


    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedContext posts


--------------------------------------------------------------------------------
noTagsPostCtx :: Context String
noTagsPostCtx =
    field "recent" (\_ -> recentPostList) `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

-- variant of map that passes each element's index as a second argument to f
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

archiveCtx :: [Item String] -> Context String
archiveCtx posts = 
    let 
        size = length posts
        splitPosts = chunksOf ((size+2) `quot` 3) posts
        start = constField "title" "Archives" `mappend` 
                noTagsPostCtx `mappend`
                listField "posts" noTagsPostCtx (return posts)
        map1 = map return splitPosts
        map2fn a x = listField ("posts" ++ (show (x+1))) noTagsPostCtx $ a
        map2 = mapInd map2fn map1
    in 
        foldr mappend start map2
        --he above code is equivalent to the below, except it can do N collumns (hardcoded to 3)
        --listField "posts1" noTagsPostCtx (return $ splitPosts !! 0) `mappend`
        --listField "posts2" noTagsPostCtx (return $ splitPosts !! 1) `mappend`
        --listField "posts3" noTagsPostCtx (return $ splitPosts !! 2) `mappend`
        

--------------------------------------------------------------------------------
feedContext :: Context String
feedContext = 
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "_kitchen"
    , feedDescription = "TODO"
    , feedAuthorName  = "Peter Lu"
    , feedAuthorEmail = "poop"
    , feedRoot        = "http://5fkitchen.com"
    }


--------------------------------------------------------------------------------
recentPosts :: Compiler [Item String]
recentPosts = do
    identifiers <- getMatches "posts/*"
    return [Item identifier "" | identifier <- identifiers]

recentPostList :: Compiler String
recentPostList = do
    posts   <- fmap (take 10) . recentFirst =<< recentPosts
    itemTpl <- loadBody "templates/indexpostitem.html"
    list    <- applyTemplateList itemTpl defaultContext posts
    return list
