--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_, zipWithM_, liftM)
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import           Text.Pandoc (writerReferenceLinks)
import           Data.Char (toLower)
import           System.Locale (defaultTimeLocale)
import           Data.List (sortBy, intercalate, length, isPrefixOf, tails, findIndex)
import           Data.List.Split (chunksOf, splitOn)
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
    match "templates/newsite/*" $ do
        route   idRoute
        compile templateCompiler

    match "css/*" $ do
        route   idRoute
        compile copyFileCompiler

    --copy images
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    --generate posts
    match "posts/portfolio/**" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/newsite/item-project.html" newPostCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/newsite/default.html" newPostCtx
            >>= relativizeUrls

    forM_ ["posts/writing/**","posts/*"] $ \p ->
        match p $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/newsite/item-post.html" newPostCtx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/newsite/default.html" newPostCtx
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
                >>= loadAndApplyTemplate "templates/newsite/item-post.html" newPostCtx
                >>= loadAndApplyTemplate "templates/newsite/default.html" newPostCtx
                >>= relativizeUrls


    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/**" "content"
            renderRss feedConfiguration feedContext posts



--------------------------------------------------------------------------------

--CAN DELETE
postCtx :: Context String
postCtx =
    field "work" (\_ -> workList)           `mappend` 
    field "recent" (\_ -> recentPostList)   `mappend` 
    dateField "date" "%B %e, %Y"            `mappend` 
    defaultContext  

 
newPostCtx :: Context String
newPostCtx =
    listField "work" defaultContext (recentFirst =<< recentWork)  `mappend` 
    listField "art" defaultContext (recentFirst =<< recentArt)  `mappend` 
    listField "teaching" defaultContext (recentFirst =<< recentTeaching)  `mappend` 
    listField "recent" defaultContext (fmap (take 10) . recentFirst =<< recentPosts)   `mappend` 
    dateField "date" "%B %e, %Y"            `mappend` 
    galleryContext `mappend`
    defaultContext  

--TODO



galleryContext :: Context a
galleryContext = field "gallery" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    let 
        process s = if (trim s) == "" 
            then "" 
            else "<img src=\"" ++ (toUrl "images/" ++ s ++ "\"") ++ " />"
        gallery = fromMaybe "" $ Map.lookup "gallery" metadata    
        splitted = fmap process (words gallery)
    return $ unlines splitted 

        

-- variant of map that passes each element's index as a second argument to f
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]


--this fancy thing will split posts into 3 groups, not that I use this feature anymore...
--pass in what you want to list
--TODO DELETE This function and just use recent/posts/writing/work/art/games/teaching from below since we don't need to split into 3 collums anymore.
archiveCtx :: [Item String] -> Context String
archiveCtx posts = 
    let 
        size = length posts
        splitPosts = chunksOf ((size+2) `quot` 3) posts
        start = constField "title" "Archives" `mappend` 
                newPostCtx `mappend`
                listField "posts" newPostCtx (return posts)
        map1 = map return splitPosts
        map2fn a x = listField ("posts" ++ (show (x+1))) newPostCtx $ a
        map2 = mapInd map2fn map1
    in 
        foldr mappend start map2
        --the above code is equivalent to the below, except it can do N collumns (hardcoded to 3)
        --listField "posts1" newPostCtx (return $ splitPosts !! 0) `mappend`
        --listField "posts2" newPostCtx (return $ splitPosts !! 1) `mappend`
        --listField "posts3" newPostCtx (return $ splitPosts !! 2) `mappend`
        

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

recentWriting :: Compiler [Item String]
recentWriting = do
    identifiers <- getMatches "posts/writing/**"
    return [Item identifier "" | identifier <- identifiers]

recentWork :: Compiler [Item String]
recentWork = do
    identifiers <- getMatches "posts/portfolio/**"
    return [Item identifier "" | identifier <- identifiers]




recentArt :: Compiler [Item String]
recentArt = do
    identifiers <- getMatches $ fromList ["posts/portfolio/ceramics/**","posts/portfolio/mushrooms/**","posts/portfolio/other/**"]
    return [Item identifier "" | identifier <- identifiers]

recentGames :: Compiler [Item String]
recentGames = do
    identifiers <- getMatches "posts/portfolio/games/**"
    return [Item identifier "" | identifier <- identifiers]

recentTeaching :: Compiler [Item String]
recentTeaching = do
    identifiers <- getMatches "posts/teaching/**"
    return [Item identifier "" | identifier <- identifiers]




--THESE THINGS ARE BROKEN FOR SOME REASON...
recentPostList :: Compiler String
recentPostList = do
    posts   <- fmap (take 10) . recentFirst =<< recentPosts
    itemTpl <- loadBody "templates/indexpostitem.html"
    list    <- applyTemplateList itemTpl defaultContext posts
    return list

workList :: Compiler String
workList = do
    posts   <- recentFirst =<< recentWork
    itemTpl <- loadBody "templates/indexpostitem.html"
    list    <- applyTemplateList itemTpl defaultContext posts
    return list
