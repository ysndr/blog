--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import           Fields
import           System.Environment             ( lookupEnv )
import           System.FilePath.Posix          ( takeFileName
                                                , takeDirectory
                                                , takeBaseName
                                                , takeExtension
                                                , (</>)
                                                )
import           Hakyll
import           Hakyll.Images                  ( loadImage
                                                , compressJpgCompiler
                                                , ensureFitCompiler
                                                )
import qualified Data.Text                      as T
import Data.Default
import           Data.List                      ( isPrefixOf
                                                , isSuffixOf
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Hakyll.Web.Sass                ( sassCompilerWith )
import           Text.Sass.Options              ( SassOptions(..)
                                                , defaultSassOptions
                                                , SassOutputStyle(..)
                                                )
import           Text.Pandoc.Options            ( WriterOptions (..) )
import           Text.Pandoc.Definition         ( Pandoc(..), Block(..), Inline(..) )
import           Text.Pandoc.Walk
import Text.Pandoc (runPure)
import Text.Pandoc.Readers (readMarkdown)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)


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
        -- <> plainTocField "toc-plain" 4 "posts-content"
        <> pathField "sourcefile"
        <> versionField "git-commit" Commit
        <> versionField "git-commit-hash" Hash
        <> customBaseContext

feedCtx :: Context String
feedCtx = bodyField "description"
        <> dateField "date" "%Y-%m-%d"
        <> customBaseContext

postCssCompiler:: Compiler (Item String)
postCssCompiler = do
    file <- getResourceFilePath
    compiled <-  unsafeCompiler $ runPostCss file
    makeItem compiled

runPostCss :: FilePath -> IO (String)
runPostCss file = do
    (status, stdout, _) <- readProcessWithExitCode "postcss" [ file ] ""

    return $ case status  of
        ExitSuccess -> stdout
        _           -> error "could not compile css"


-- Main
-------------------------------------------------------------------------------
--------------------------------------------------------------------------------
main :: IO ()
main = do
    sassCompiler <- fmap (sassCompilerWith . sassOptions)
                         (lookupEnv "THIRDPARTY")
    compilerEnv <- lookupEnv "HAKYLL_ENV"
    let isDevelopment = compilerEnv == Just "development"

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
                    >>= cleanIndexUrls

        -- compress images
        match jpgs $ version "large" $ do
            route idRoute
            compile $ loadImage
                >>= compressJpgCompiler 50

        match jpgs $ version "small" $ do
            route $ fileSuffixRoute "small"
            compile $ loadImage
                >>= ensureFitCompiler 1200 600
                >>= compressJpgCompiler 90

        -- copy assets (non images and non post files)
        match ("posts/**" .&&. complement postsGlob .&&. complement jpgs) $ do
            route idRoute
            compile $ copyFileCompiler

        match ("assets/images/**" .&&. complement jpgs) $ do
            route idRoute
            compile $ copyFileCompiler

        -- compile SASS/CSS
        depends <- makePatternDependency "assets/css/**.css"
        rulesExtraDependencies [depends] $ do
            match (fromRegex "^assets/css/[^_].*\\.css") $ do
                route $ setExtension "css"
                compile postCssCompiler

        -- assemble static pages
        match (fromList ["about.md", "contact.md"]) $ do
            route $ cleanRoute
            compile
                $   pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" customBaseContext
                >>= loadAndApplyTemplate "templates/default.html" customBaseContext
                >>= relativizeUrls
                >>= cleanIndexUrls

        -- assemble archive
        create ["archive.html"] $ do
            route cleanRoute
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
                    >>= cleanIndexUrls

        -- assemble index page
        match "index.html" $ do
            route cleanRoute
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
                    >>= cleanIndexUrls


         -- assemble posts
        matchMetadata postsGlob (\m -> isDevelopment || lookupString "status" m == Just "published") $ do
            let postCtx' = postCtx tags categories
            route $ composeRoutes ensureDateRoute $ composeRoutes cleanRoute idRoute
            compile
                $   pandocCompilerWithTransform defaultHakyllReaderOptions  defaultHakyllWriterOptions htmlFilter
                >>= saveSnapshot "posts-content"
                >>= loadAndApplyTemplate "templates/post.html" postCtx'
                >>= saveSnapshot "posts-rendered"
                >>= loadAndApplyTemplate "templates/default.html" postCtx'
                >>= relativizeUrls
                >>= cleanIndexUrls
                >>= localAssetsUrls


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
                posts <- recentFirst =<< loadAll postsGlob

                -- load individual pages from a list (globs DO NOT work here)
                singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])

                -- mappend the posts and singlePages together
                let pages = posts <> singlePages

                    postCtx' t c = dateField "date" "%Y-%m-%d" <> postCtx t c
                    -- create the `pages` field with the postCtx containing standard date
                    -- and return the `pages` value for it
                    sitemapCtx =
                        constField "root" root <>
                        listField "pages" (postCtx' tags categories) (return pages)

                -- make the item and apply our sitemap template
                makeItem ""
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

        create ["CNAME"] $ do
            route idRoute
            compile $ makeItem domain

parse body = case (runPure $ readMarkdown defaultHakyllReaderOptions body)
                 of
                    Left err    -> error $ "Could not parse"
                    Right (Pandoc meta blocks) -> blocks

htmlFilter :: Pandoc -> Pandoc
htmlFilter = walk replaceElements where
    replaceElements (Div (id, classes, kv) blocks) =
        Div (id, classes' ++ ["uk-alert y-fill-horizontal"], kv) (prependHeader ++ content ++ appendCaption)
        where
            (classes', icon, badge)
                | any (== "info") classes       = (classes, "info", "INFO")
                | any (== "note") classes       = (["uk-alert-primary"] ++ classes, "pencil", "NOTE")
                | any (== "warning") classes    = (["uk-alert-warning"] ++ classes, "warning", "WARN")
                | any (== "help")    classes    = (["uk-alert-success"] ++ classes, "lifesaver", "HELP")
                | any (== "danger")  classes    = (["uk-alert-danger"] ++ classes, "bolt", "MISTAKE")
                | otherwise = (classes, "", "")
            content = blocks

            iconElement = Div ("", [], []) [Div ("", [], [("uk-icon", icon)]) [], Div ("", ["y-badge"], []) [Plain [Str badge]]]
            prependHeader =
                case lookup "header" kv of
                    Just header -> [(Div ("", ["y-box-header"], []) (iconElement : parse header) )]
                    Nothing -> []

            appendCaption =
                case  lookup "caption" kv of
                    Just caption -> [(Div ("", ["y-box-caption"], []) (parse caption))]
                    Nothing -> []


    replaceElements (Para inlines) = Para (map addClasses inlines) where
        addClasses (Image (id, classes, kv) label target) = Image (
            id,
            classes ++ [
                "uk-border-rounded",
                "uk-box-shadow-large",
                T.append "uk-align-" align
            ],
            kv) label target where align = fromMaybe  "center" $ lookup "align" kv
        addClasses inline = inline

    replaceElements code @ (CodeBlock (_, [_], _) _) = Div ("", ["y-fill-horizontal"], []) [code]

    replaceElements (Header level (id, classes, kv) content ) = Header level (id, classes', kv) content' where
        classes' = classes ++ ["y-header"]
        content' = [(Span ("", [], []) content), Link ("", ["y-anchor"], [("uk-icon", "link")]) [] (T.pack $ "#"++ (T.unpack id), id) ] -- how else to do that?

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

fileSuffixRoute :: String -> Routes
fileSuffixRoute suffix = customRoute makeSuffixRoute
  where
    makeSuffixRoute ident = parentDir </> suffixed  where
        p = toFilePath ident
        parentDir = takeDirectory p
        baseName = takeBaseName p
        ext = takeExtension p
        suffixed = baseName ++ "-" ++ suffix ++ ext


ensureDateRoute :: Routes
ensureDateRoute = metadataRoute $ \m -> do
    case lookupString "date" m of
        Just date -> customRoute $ prefixDate date
        _ -> idRoute
    where
        prefixDate date ident = path where
            file = toFilePath ident
            dir = takeDirectory file
            name = takeFileName file
            path = if isPrefixOf date name then file
                   else dir </> (date ++ "-" ++ name)

-- adapted from https://www.rohanjain.in/hakyll-clean-urls/
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = parentDir </> index  where
        p = toFilePath ident
        parentDir = takeDirectory p
        baseName = takeBaseName p
        index = if baseName == "index"
                then "index.html"
                else baseName </> "index.html" -- do not create `index/index.html`

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = let
    cleanIndex :: String -> String
    cleanIndex url
        | isSuffixOf idx url = take (length url - length idx) url
        | otherwise            = url
        where idx = "index.html"
    in return . fmap (withUrls cleanIndex)


localAssetsUrls :: Item String -> Compiler (Item String)
localAssetsUrls item = let
    localAssets :: FilePath -> FilePath
    localAssets url
        | isPrefixOf "./"  url && local /= "index" = "../" </> local </> drop 2 url -- drop ./
        | otherwise            = url
    ident = itemIdentifier item
    file = toFilePath ident
    local = takeBaseName file
    in
        return $ fmap (withUrls $ localAssets ) item
