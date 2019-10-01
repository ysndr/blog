--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe                     ( fromJust )
import           Control.Applicative            ( empty ) 
import           System.Environment             ( lookupEnv )
import           Hakyll
import qualified Data.Text                     as T
import           Hakyll.Web.Sass                ( sassCompilerWith )
import           Text.Sass.Options              ( SassOptions(..)
                                                , defaultSassOptions
                                                , SassOutputStyle(..)
                                                )
--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration { destinationDirectory = "build/site"
                              , storeDirectory       = "build/_store"
                              , tmpDirectory         = "build/_tmp"
                              , providerDirectory    = "src"
                              }

sassOptions :: Maybe FilePath -> SassOptions
sassOptions distPath = defaultSassOptions
    { sassSourceMapEmbed = True
    , sassOutputStyle    = SassStyleCompressed
    , sassIncludePaths   = fmap (: []) distPath
    }

--------------------------------------------------------------------------------
main :: IO ()
main = do
    sassCompiler <- fmap (sassCompilerWith . sassOptions)
                         (lookupEnv "THIRDPARTY")
    hakyllWith config $ do

        tags <- buildTags "posts/*" (fromCapture "tags/*.html")

        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged \"" ++ tag ++ "\""
            let ctx   = postCtxWithTags tags

            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern

                let tagsCtx =
                        constField "title" title
                            <> listField "posts" ctx (return posts)
                            <> defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html"     tagsCtx
                    >>= loadAndApplyTemplate "templates/default.html" tagsCtx
                    >>= relativizeUrls


        match "assets/images/*" $ do
            route idRoute
            compile copyFileCompiler

        match (fromRegex "^css/[^_].*\\.scss") $ do
            route $ setExtension "css"
            compile sassCompiler

        match (fromList ["about.rst", "contact.markdown"]) $ do
            route $ setExtension "html"
            compile
                $   pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        match "posts/*" $ do
            let postCtx' = postCtxWithTags tags
            route $ setExtension "html"
            compile
                $   pandocCompiler
                >>= saveSnapshot "posts-content"
                >>= loadAndApplyTemplate "templates/post.html" postCtx'
                >>= saveSnapshot "posts-rendered"
                >>= loadAndApplyTemplate "templates/default.html" postCtx'
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let ctx = postCtxWithTags tags
                let archiveCtx =
                        listField "posts" ctx (return posts)
                            <> constField "title" "Archives"
                            <> defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let ctx = postCtxWithTags tags
                let indexCtx =
                        listField "posts" ctx (return posts)
                            <> constField "title" "Home"
                            <> defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

        match "html/*.html" $ do
            route idRoute
            compile copyFileCompiler
--------------------------------------------------------------------------------


postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = listFieldWith "tags" (tagCtx tags) mkPostTags <> postCtx 
 where
    
    tagCtx :: Tags -> Context String
    tagCtx tags = field "name" (return . itemBody) 
               <> field "url" mkTagUrl

    mkTagUrl    -- unwrap the maybe that gets returned when
                -- seaching the route of the id of the tag 
                -- (or something like that)
        :: Item String          -- ^ a tag name
        -> Compiler String    -- ^ corresponding tag page url
    mkTagUrl item = toUrl <$> ((<$>) fromJust . getRoute . tagsMakeId tags . itemBody $ item) 

    mkPostTags  -- resolve the item's tags
                -- if it has tags apply them to makeItem (phrasing?)
                -- else return empty to suppress rendering
        :: Item String               -- a post
        -> Compiler [Item String]    -- the tags for a given post
    mkPostTags item = (getTags . itemIdentifier $ item)
        >>= \tags' -> if null tags' then empty 
                      else (return tags') >>= (mapM makeItem)


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
        <> teaserField "teaser" "posts-content"
        <> peekField 50 "peek" "posts-content"
        <> defaultContext
-------------------------------------------------------------------------------
peekField
    :: Int              -- ^ length to peak
    -> String           -- ^ Key to use
    -> Snapshot         -- ^ Snapshot to load
    -> Context String   -- ^ Resulting context
peekField length key snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    return (peak body)
    where peak = T.unpack . T.unwords . take length . T.words . T.pack
