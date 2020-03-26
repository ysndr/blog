--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import           Fields
import           System.Environment             ( lookupEnv )
import           System.FilePath.Posix          ( takeFileName )
import           Hakyll
import           Hakyll.Images                  ( loadImage
                                                , compressJpgCompiler
                                                )
import qualified Data.Text                      as T
import Data.Default
import Data.Char                                (isSpace)
import Data.List                                (dropWhileEnd, lookup, groupBy, isPrefixOf)
import           Hakyll.Web.Sass                ( sassCompilerWith )
import           Text.Sass.Options              ( SassOptions(..)
                                                , defaultSassOptions
                                                , SassOutputStyle(..)
                                                )
import           Text.Pandoc.Options            ( WriterOptions (..) )
import           Text.Pandoc.Definition         ( Pandoc(..), Block(..), Inline(..) )
import           Text.Pandoc.Walk


-- Configuration
--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration {
    destinationDirectory = "build/site"
    , storeDirectory       = "build/_store"
    , tmpDirectory         = "build/_tmp"
    , providerDirectory    = "src"
    , ignoreFile           = ignoreFile'
  } where
      ignoreFile' path
        | "."    `isPrefixOf` fileName = True
        | otherwise                    = False
        where
          fileName = takeFileName path

sassOptions :: Maybe FilePath -> SassOptions
sassOptions distPath = defaultSassOptions
    { sassSourceMapEmbed = True
    , sassOutputStyle    = SassStyleCompressed
    , sassIncludePaths   = fmap (: []) distPath
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "y|sndr blog - Alot of stuff about alot of stuff"
    , feedDescription = "Caution this feed might contain oxidized iron and functional ideas."
    , feedAuthorName  = "Yannik Sander"
    , feedAuthorEmail = "contact@ysndr.de"
    , feedRoot        = "https://blog.ysndr.de"
    }

-- Global Consts
--------------------------------------------------------------------------------
postsGlob = "posts/**.md"
jpgs = "**.jpg" .||. "**.jpeg"
svg = "**.svg"

domain :: String
domain = "blog.ysndr.de"

root :: String
root = "https://" ++ domain

-- Contexts
--------------------------------------------------------------------------------
customBaseContext :: Context String
customBaseContext = headVersionField "git-head-commit" Commit
                 <> headVersionField "git-head-commit-hash" Hash
                 <> headVersionField "git-head-commit-full" Full
                 <> constField "item-type" "default"
                 <> concatField "concat"
                 <> constField "root" root
                 <> defaultContext

postCtx :: Tags -> Tags -> Context String
postCtx tags category =  dateField "date" "%B %e, %Y"
        <> allTagsField "tags" tags
        <> allTagsField "category" category
        <> constField "item-type" "post"
        <> teaserField "teaser" "posts-content"
        <> peekField 50 "peek" "posts-content"
        <> readTimeField "read-time" "posts-content"
        <> tocField "toc" 4 (def {extraUlClasses = "uk-nav-default uk-list uk-nav-sub"}) "posts-content"
        <> plainTocField "toc-plain" 4 "posts-content"
        <> pathField "sourcefile"
        <> versionField "git-commit" Commit
        <> versionField "git-commit-hash" Hash
        <> customBaseContext

feedCtx :: Context String
feedCtx = bodyField "description"
        <> dateField "date" "%Y-%m-%d"
        <> customBaseContext

-- Main
-------------------------------------------------------------------------------
--------------------------------------------------------------------------------
main :: IO ()
main = do
    sassCompiler <- fmap (sassCompilerWith . sassOptions)
                         (lookupEnv "THIRDPARTY")
    hakyllWith config $ do

        tags <- buildTags postsGlob (fromCapture "tags/*.html")
        categories <- buildCategories postsGlob (fromCapture "posts/*.html")

        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged \"" ++ tag ++ "\""
            let ctx   = postCtx tags categories

            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern

                let tagsCtx =
                        constField "title" title
                            <> listField "posts" ctx (return posts)
                            <> constField "tag" tag
                            <> customBaseContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html"     tagsCtx
                    >>= loadAndApplyTemplate "templates/default.html" tagsCtx
                    >>= relativizeUrls

        -- compress images
        match jpgs $ do
            route idRoute
            compile $ loadImage
                >>= compressJpgCompiler 50

        -- copy assets (non images and non post files)
        match ("posts/**" .&&. complement postsGlob .&&. complement jpgs) $ do
            route idRoute
            compile $ copyFileCompiler

        match ("assets/images/**" .&&. complement jpgs) $ do
            route idRoute
            compile $ copyFileCompiler

        -- compile SASS/CSS
        depends <- makePatternDependency "assets/css/**.scss"
        rulesExtraDependencies [depends] $ do
            match (fromRegex "^assets/css/[^_].*.scss") $ do
                route $ setExtension "css"
                compile sassCompiler

        -- assemble static pages
        match (fromList ["about.md", "contact.md"]) $ do
            route $ setExtension "html"
            compile
                $   pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" customBaseContext
                >>= loadAndApplyTemplate "templates/default.html" customBaseContext
                >>= relativizeUrls

        -- assemble archive
        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsGlob
                let ctx = postCtx tags categories
                let archiveCtx = listField "posts" ctx (return posts)
                            <> publishedGroupField "years" posts ctx
                            <> constField "title" "Archive"
                            <> customBaseContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls

        -- assemble index page
        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsGlob
                let ctx = postCtx tags categories
                let indexCtx =
                        listField "posts" ctx (return $ take 6 posts)
                            <> constField "title" "Home"
                            <> customBaseContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls


         -- assemble posts
        match postsGlob $ do
            let postCtx' = postCtx tags categories
            route $ setExtension "html"
            compile
                $   pandocCompilerWithTransform defaultHakyllReaderOptions  defaultHakyllWriterOptions htmlFilter
                >>= saveSnapshot "posts-content"
                >>= loadAndApplyTemplate "templates/post.html" postCtx'
                >>= saveSnapshot "posts-rendered"
                >>= loadAndApplyTemplate "templates/default.html" postCtx'
                >>= relativizeUrls

        -- compile templates
        match "templates/**" $ compile templateBodyCompiler

        -- include static html pages
        match "html/*.html" $ do
            route idRoute
            compile copyFileCompiler

        create ["rss.xml"] $ do
            route idRoute
            compile (feedCompiler renderRss)

        create ["atom.xml"] $ do
            route idRoute
            compile (feedCompiler renderAtom)


        create ["sitemap.xml"] $ do
            route idRoute
            compile $ do
                -- load and sort the posts
                posts <- recentFirst =<< loadAll "posts/*"

                -- load individual pages from a list (globs DO NOT work here)
                singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])

                -- mappend the posts and singlePages together
                let pages = posts <> singlePages
                    -- create the `pages` field with the postCtx
                    -- and return the `pages` value for it
                    sitemapCtx =
                        constField "root" root <>
                        listField "pages" (postCtx tags categories) (return pages)

                -- make the item and apply our sitemap template
                makeItem ""
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

        create ["CNAME"] $ do
            route idRoute
            compile $ makeItem domain

htmlFilter :: Pandoc -> Pandoc
htmlFilter = walk replaceElements where
    replaceElements (Div (id, classes, kv) blocks) =
        Div (id, classes', kv) (prepend ++ blocks)
        where
            classes'
                | any (== "info") classes = "uk-alert" : classes
                | any (== "note") classes = ["uk-alert-primary" , "uk-alert"] ++ classes
                | any (== "warning") classes = ["uk-alert-warning", "uk-alert"] ++ classes
                | otherwise = classes
            prepend = case lookup "message" kv of
                Just message -> [(Header 1 ("",[],[]) [Str message])]
                Nothing -> []
    replaceElements block = block


type FeedRenderer =
    FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
    renderer feedConfiguration feedCtx
        =<< fmap (take 10 ) . recentFirst
        =<< loadAllSnapshots postsGlob "posts-content"
