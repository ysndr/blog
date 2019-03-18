--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                    ( mappend )
import           System.Environment             ( lookupEnv )
import           Hakyll
import           Hakyll.Web.Sass                ( sassCompiler
                                                , sassCompilerWith
                                                )
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
    sassCompiler <- fmap (sassCompilerWith . sassOptions) (lookupEnv "THIRDPARTY")
    hakyllWith config $ do

        match "images/*" $ do
            route idRoute
            compile copyFileCompiler

        -- match "css/*.css" $ do
        --     route   idRoute
        --     compile compressCssCompiler

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
            route $ setExtension "html"
            compile
                $   pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts)
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
                let indexCtx =
                        listField "posts" postCtx (return posts)
                            <> constField "title" "Home"
                            <> defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = 
    dateField "date" "%B %e, %Y" 
    <> peekField 50 "peak" "posts-content"
    <> defaultContext
-------------------------------------------------------------------------------
peekField ::                Int           -- ^ length to peak
                         -> String           -- ^ Key to use
                         -> Snapshot         -- ^ Snapshot to load
                         -> Context String   -- ^ Resulting context
peekField length key snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    return (peak body)
    where peak = T.unpack . T.unwords . take length . T.words . T.pack
