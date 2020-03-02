module Fields (
  peekField
, GitVersionContent(..)
, versionField
, headVersionField
, readTimeField
, publishedGroupField
, concatField
, tocField
, allTagsField
) where

import           Control.Applicative            ( empty )
import qualified Data.Text                      as T
import           Data.Char                      (isSpace)
import           Data.List                      ( dropWhileEnd
                                                , groupBy
                                                , isPrefixOf )
import           Data.Maybe                     ( fromJust )

import           Data.Time.Calendar
import           Data.Time.Clock                (UTCTime (..))
import           Data.Time.Locale.Compat        (defaultTimeLocale)

import           Hakyll
import           System.Exit                    ( ExitCode(..) )
import           System.Process
import           Text.Pandoc                    ( runPure )
import           Text.Pandoc.Options            ( WriterOptions (..) )
import           Text.Pandoc.Readers            ( readHtml )
import           Text.Pandoc.Writers            ( writeHtml5String )


-- Peek Field
--------------------------------------------------------------------------------

peekField
    :: Int              -- ^ length to peak
    -> String           -- ^ Key to use
    -> Snapshot         -- ^ Snapshot to load
    -> Context String   -- ^ Resulting context
peekField length key snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    return (peak body)
    where peak = T.unpack . T.unwords . take length . T.words . T.pack


-- Git related fields
--------------------------------------------------------------------------------
data GitVersionContent = Hash | Commit | Full
     deriving (Eq, Read)

instance Show GitVersionContent where
    show content = case content of
        Hash -> "%h"
        Commit -> "%h: %s"
        Full -> "%h: %s (%ai)"

-- Query information of a given file tracked with git
getGitVersion :: GitVersionContent -- Kind of information
              -> FilePath          -- File to query information of
              -> IO String         --
getGitVersion content path = do
    (status, stdout, _) <- readProcessWithExitCode "git" [
        "log",
        "-1",
        "--format=" ++ (show content),
        "--",
        "src/"++path] ""

    return $ case status  of
        ExitSuccess -> trim stdout
        _           -> ""

    where trim = dropWhileEnd isSpace

-- Field that contains the latest commit hash that hash touched the current item.
versionField :: String -> GitVersionContent -> Context String
versionField name content = field name $ \item -> unsafeCompiler $ do
    let path = toFilePath $ itemIdentifier item
    getGitVersion content  path

-- Field that contains the commit hash of HEAD.
headVersionField :: String -> GitVersionContent -> Context String
headVersionField name content  = field name $ \_ -> unsafeCompiler $ getGitVersion content  "."

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
    tuples <- traverse extractYear posts
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
          extractYear item = do
             time <- getItemUTC defaultTimeLocale (itemIdentifier item)
             let    (year, _, _) = (toGregorian . utctDay) time
             return (year, [item])

concatField :: String -> Context String
concatField name = functionField name (\args item -> return $ concat args)

tocField :: String -> String -> Context String
tocField name snapshot = field name $ \item -> do
    body <- loadSnapshot (itemIdentifier item) snapshot

    let writerOptions = defaultHakyllWriterOptions
            {
              writerTableOfContents = True
            , writerTemplate = Just "$table-of-contents$"
            }
        toc = case (runPure $ readHtml defaultHakyllReaderOptions (T.pack $ itemBody body))
               >>= \pandoc -> runPure ( writeHtml5String writerOptions pandoc) of
                        Left err    -> fail $ ""
                        Right item' -> T.unpack item'

    return $ toc


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
