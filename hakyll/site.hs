--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_, zipWithM_, liftM)
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import           Text.Pandoc (writerReferenceLinks)
import           Data.Char (toLower)
import           System.Locale (defaultTimeLocale)
import           Data.List (sortBy, intercalate, length, isPrefixOf, tails, findIndex)
import           Data.List.Split (chunksOf)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (parseTime)
import           Data.Maybe (fromMaybe)
import qualified Data.Map as Map


import Debug.Trace

-- Allow for reference style links in markdown
pandocWriteOptions = defaultHakyllWriterOptions{ 
    writerReferenceLinks = True
}

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    --copy template stuff
    match "templates/**" $ do
        route   idRoute
        compile templateCompiler

    --copy images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    --generate posts
    match "posts/portfolio/**" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/newsite/item-project.html" postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/newsite/default.html" postCtx
            >>= relativizeUrls

    forM_ ["posts/writing/**","posts/*"] $ \p ->
        match p $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/newsite/item-post.html" postCtx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/newsite/default.html" postCtx
                >>= relativizeUrls

    --generate static pages
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            allPosts <- loadAll "posts/writing/**"
            makeItem ""
                >>= loadAndApplyTemplate "templates/newsite/archive.html" (archiveCtx allPosts)
                >>= loadAndApplyTemplate "templates/newsite/default.html" (archiveCtx allPosts)
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            allPosts <- loadAll "posts/portfolio/**"
            makeItem ""
                >>= loadAndApplyTemplate "templates/newsite/project-list.html" (archiveCtx allPosts)
                >>= loadAndApplyTemplate "templates/newsite/default.html" (archiveCtx allPosts)
                >>= relativizeUrls           

    -- Build special pages
    forM_ ["404.markdown","about.markdown"] $ \p ->
        match p $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/newsite/item-post.html" postCtx
                >>= loadAndApplyTemplate "templates/newsite/default.html" postCtx
                >>= relativizeUrls


    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/**" "content"
            renderRss feedConfiguration feedContext posts



--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    field "work" (\_ -> workList)           `mappend` 
    field "recent" (\_ -> recentPostList)   `mappend` 
    dateField "date" "%B %e, %Y"            `mappend` 
    defaultContext                        

--TODO
galleryContext :: Context a
galleryContext = field "gallery" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    let 
        gallery = fromMaybe "" $ Map.lookup "gallery" metadata    
    return gallery
        

-- variant of map that passes each element's index as a second argument to f
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

archiveCtx :: [Item String] -> Context String
archiveCtx posts = 
    let 
        size = length posts
        splitPosts = chunksOf ((size+2) `quot` 3) posts
        start = constField "title" "Archives" `mappend` 
                postCtx `mappend`
                listField "posts" postCtx (return posts)
        map1 = map return splitPosts
        map2fn a x = listField ("posts" ++ (show (x+1))) postCtx $ a
        map2 = mapInd map2fn map1
    in 
        foldr mappend start map2
        --the above code is equivalent to the below, except it can do N collumns (hardcoded to 3)
        --listField "posts1" postCtx (return $ splitPosts !! 0) `mappend`
        --listField "posts2" postCtx (return $ splitPosts !! 1) `mappend`
        --listField "posts3" postCtx (return $ splitPosts !! 2) `mappend`
        

--------------------------------------------------------------------------------
feedContext :: Context String
feedContext = 
    constField "description" "" `mappend` --TODO replace with something useful
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

workList :: Compiler String
workList = do
    identifiers <- getMatches "posts/portfolio/**"
    posts   <- fmap (take 10) . recentFirst $ [Item identifier "" | identifier <- identifiers] --TODO get rid of the fmap???
    itemTpl <- loadBody "templates/indexpostitem.html"
    list    <- applyTemplateList itemTpl defaultContext posts
    return list
