--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import           Debug.Trace                 (trace)
import           Data.Monoid                 (mappend)
import           Data.Maybe                  (fromMaybe)
import           Data.List                   (sortBy)
import           Data.List.Split             (splitOn)
import           Data.Ord                    (comparing)
import           Text.Regex                  (subRegex, mkRegex)
import           Text.Regex.Posix            ((=~))
import           Text.Read                   (readMaybe)
import           Control.Monad               (liftM, filterM)
import           Hakyll
import           Hakyll.Core.Metadata        (lookupString, lookupStringList)
import           System.FilePath.Posix       (takeBaseName,takeDirectory,takeExtension,(</>))

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "semantic/dist/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    create ["robots.txt", "favicon.ico"] $ do
        route   idRoute
        compile copyFileCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/meta/*" $ do
        route idRoute
        compile pandocCompiler

    match "posts/*" $ do
        route $ stripRouteDatePrefix
        compile $ do
            id' <- getUnderlying
            metadata <- getMetadata id'
            let relatedPostsNames = fromMaybe [] $ lookupStringList "relatedPosts" metadata
            let relatedPostsPaths = map (fromFilePath . ("posts/meta/"++)) relatedPostsNames
            posts <- loadAll $ fromList relatedPostsPaths
            let mHasRelatedPosts = (if null posts then mempty else constField "hasRelatedPosts" "True") 
            let taggedPostCtx =
                    mHasRelatedPosts                                 `mappend`
                    listField "posts" postCtx (return posts)         `mappend`
                    constField "postListingTitle" "Related Posts"    `mappend`
                    postCtxWithTags tags

            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    taggedPostCtx
                >>= loadAndApplyTemplate "templates/default.html" taggedPostCtx
                >>= relativizeUrls

    create ["guides.html"] $ do
        route $ cleanRoute
        compile $ do
            let description = "Guides and posts about software by WYC Technology."
            posts <- regularPosts
            let guidesCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Guides and Posts"    `mappend`
                    constField "description" description     `mappend`
                    constField "guides" "True"               `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/guides.html" guidesCtx
                >>= loadAndApplyTemplate "templates/default.html" guidesCtx
                >>= relativizeUrls

    match "index.html" $ do
        route   idRoute
        compile $ do
            let description = "Business-Driven Rapid Software Development"
            posts <- fmap (take 3) $ (byOrderMetadataField "featuredOrder") =<< regularPosts
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Rapid Software Development" `mappend`
                    constField "description" description     `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
-- | Item Contexts
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "guides" "True" `mappend`
	relatedPostsContext `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

--------------------------------------------------------------------------------
-- | Get regular posts
regularPosts = recentFirst =<< loadAll "posts/*"

--------------------------------------------------------------------------------
-- | Get posts by tag
postsByTag tag = recentFirst =<< keepTag tag =<< loadAll "posts/*"

--------------------------------------------------------------------------------
-- | Try to get a page's field as a Bool
getItemBool :: MonadMetadata m => String -> Identifier -> m Bool
getItemBool metaField id' = do
    metadata <- getMetadata id'
    return $ fromMaybe False $ readMaybe =<< lookupString metaField metadata
                                               
--------------------------------------------------------------------------------
-- | Try to get a page's field as an Integer
getItemInteger :: MonadMetadata m => String -> Identifier -> m Integer
getItemInteger metaField id' = do
    metadata <- getMetadata id'
    return $ fromMaybe 0 $ readMaybe =<< lookupString metaField metadata

--------------------------------------------------------------------------------
-- | Try to get a page's field as a String
getItemString :: MonadMetadata m => String -> Identifier -> m String
getItemString metaField id' = do
    metadata <- getMetadata id'
    return $ fromMaybe "" $ readMaybe =<< lookupString metaField metadata

--------------------------------------------------------------------------------
-- | Sort pages by a specified metadata field, ascending
byOrderMetadataField :: MonadMetadata m => String -> [Item a] -> m [Item a]
byOrderMetadataField field' =
    sortByM $ (getItemInteger field') . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

--------------------------------------------------------------------------------
-- | Sort pages by their order metadata field, ascending
byOrderMetadata :: MonadMetadata m => [Item a] -> m [Item a]
byOrderMetadata = byOrderMetadataField "order"

--------------------------------------------------------------------------------
-- | Generic function to filter items by their tags
filterTag :: MonadMetadata m => Bool -> String -> [Item a] -> m [Item a]
filterTag keep tagName =
    filterM filterFn
  where
    sign = if keep then id else not
    filterFn :: MonadMetadata m => Item a -> m Bool
    filterFn item = fmap (sign . elem tagName) $ getTags . itemIdentifier $ item

--------------------------------------------------------------------------------
-- | Keep items if they have a certain tag
keepTag :: MonadMetadata m => String -> [Item a] -> m [Item a]
keepTag = filterTag True

--------------------------------------------------------------------------------
-- | Exclude items if they have a certain tag
excludeTag :: MonadMetadata m => String -> [Item a] -> m [Item a]
excludeTag = filterTag False

--------------------------------------------------------------------------------
-- | Strip trailing extension (.html)
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
    where createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
            where p = toFilePath ident

--------------------------------------------------------------------------------
-- | Strip date prefix (2006-01-02-)
stripDatePrefix :: String -> String
stripDatePrefix s = subRegex pat s ""
   where pat = mkRegex "^[0-9]{4}-[0-9]{2}-[0-9]{2}-"

--------------------------------------------------------------------------------
-- | Strip date prefix from route (2006-01-02-)
stripRouteDatePrefix :: Routes
stripRouteDatePrefix = customRoute createStrippedRoute
    where createStrippedRoute ident = takeDirectory p </> strippedBaseName </> "index.html"
              where p = toFilePath ident
                    strippedBaseName = stripDatePrefix (takeBaseName p)


--------------------------------------------------------------------------------
-- | Generic function to filter items by their tags
splitRelatedPostPaths :: MonadMetadata m => String -> m [String]
splitRelatedPostPaths str =
    return $ map ("posts/"++) $ splitOn ", " str

--------------------------------------------------------------------------------
-- | 
relatedPostsContext :: Context a
relatedPostsContext = field "relatedPosts" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ lookupString "relatedPosts" metadata

