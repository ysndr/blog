--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe                     ( fromJust )
import           Control.Applicative            ( empty ) 
import           System.Environment             ( lookupEnv )
import           System.Process
import           Hakyll
import qualified Data.Text                     as T
import Data.Char                                (isSpace)
import Data.List                                (dropWhileEnd,groupBy)
import Data.Time.Calendar
import           Data.Time.Clock                (UTCTime (..))
import           Data.Time.Locale.Compat        (defaultTimeLocale)
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
postsGlob = "posts/**"

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


        match "assets/images/*" $ do
            route idRoute
            compile copyFileCompiler

        match (fromRegex "^assets/css/[^_].*\\.scss") $ do
            route $ setExtension "css"
            compile sassCompiler

        match (fromList ["about.rst", "contact.markdown"]) $ do
            route $ setExtension "html"
            compile
                $   pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" customBaseContext
                >>= relativizeUrls

        match postsGlob $ do
            let postCtx' = postCtx tags categories
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

        match "templates/*" $ compile templateBodyCompiler
        match "templates/includes/*" $ compile templateBodyCompiler
        

        match "html/*.html" $ do
            route idRoute
            compile copyFileCompiler
--------------------------------------------------------------------------------

customBaseContext :: Context String
customBaseContext = headVersionField "git-head-commit" False
                 <> headVersionField "git-head-commit-hash" True
                 <> defaultContext 

allTagsField :: String -> Tags -> Context String
allTagsField name tags = listFieldWith name (tagCtx tags) mkPostTags 
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

postCtx :: Tags -> Tags -> Context String
postCtx tags category =  dateField "date" "%B %e, %Y"
        <> allTagsField "tags" tags
        <> allTagsField "category" category
        <> boolField "isPost" (\_ -> True)
        <> teaserField "teaser" "posts-content"
        <> peekField 50 "peek" "posts-content"
        <> readTimeField "read-time" "posts-content"
        <> pathField "sourcefile"
        <> versionField "git-commit" False 
        <> versionField "git-commit-hash" True
        <> customBaseContext
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

-------------------------------------------------------------------------------
getGitVersion :: Bool -> FilePath -> IO String
getGitVersion hashOnly path = trim <$> readProcess "git" ["log", "-1", (if  hashOnly then "--format=%h" else "--format=%h (%ai) %s"), "--", path] ""
  where
    trim = dropWhileEnd isSpace

-- Field that contains the latest commit hash that hash touched the current item.
versionField :: String -> Bool -> Context String
versionField name hashOnly= field name $ \item -> unsafeCompiler $ do
    let path = toFilePath $ itemIdentifier item
    getGitVersion hashOnly path

-- Field that contains the commit hash of HEAD.
headVersionField :: String -> Bool -> Context String
headVersionField name hashOnly = field name $ \_ -> unsafeCompiler $ getGitVersion hashOnly "."

-- Field that naïvely determines the reading time 
-- by assuming an average of 200 words per minute of reading velocity and 
-- dividing the actual number of words by  this average
readTimeField :: String -> Snapshot -> Context String
readTimeField name snapshot = field name $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    let words = length (T.words . T.pack $ body)
    return $ show $ div words 200

publishedGroupField :: String           -- name
                    -> [Item String]    -- posts
                    -> Context String   -- Post context
                    -> Context String   -- output context
publishedGroupField name posts postContext = listField name groupCtx $ do
    tuples <- sequence $ fmap extractYear $ posts
    let grouped = groupByYear tuples 
    let merged = fmap merge $ grouped
    let itemized = fmap makeItem $ merged
    
    sequence itemized

    
    where groupCtx = field "year" (return . show . fst . itemBody) 
                  <> listFieldWith "posts" postContext (return . snd . itemBody)

          merge :: [(Integer, [Item String])]  -> (Integer, [Item String])
          merge gs = let conv (year, acc) (_, toAcc) = (year, toAcc ++ acc)
                      in  foldr conv (head gs) (tail gs)
        
          
          groupByYear = groupBy (\(y, _) (y', _) -> y == y')
          
          extractYear :: Item a -> Compiler (Integer,  [Item a])
          extractYear = \item -> do
             time <- getItemUTC defaultTimeLocale (itemIdentifier item)
             let    (year, _, _) = (toGregorian . utctDay) time             
             return (year, [item])
