{-# LANGUAGE OverloadedStrings #-}

module Fields (
  peekField
, GitVersionContent(..)
, versionField
, headVersionField
, readTimeField
, publishedGroupField
, concatField
, ToCExtra(..)
, tocField
-- , plainTocField
, allTagsField
) where

import           Control.Applicative            ( empty )
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as TL
import           Data.Char                      ( isSpace )
import           Data.Default                   ( Default )
import           Data.List                      ( dropWhileEnd
                                                , groupBy
                                                , isPrefixOf )
import           Data.Maybe                     ( fromJust )
import           Data.String                    ( IsString, fromString )


import           Data.Time.Calendar
import           Data.Time.Clock                (UTCTime (..))
import           Data.Time.Locale.Compat        (defaultTimeLocale)

import           Hakyll
import           System.Exit                    ( ExitCode(..) )
import           System.Process
import           Text.Blaze.Internal            ( MarkupM( .. ), attribute, (!), getText, StaticString (..) )
import           Text.Blaze.Html                ( Html, Attribute,  )
import           Text.Blaze.XHtml5              ( ul, li, toHtml )
import           Text.Blaze.XHtml5.Attributes   ( class_ , alt)
import           Text.Blaze.Html.Renderer.Text  ( renderHtml )

import           Text.Pandoc                    hiding (trace)
import           Text.Pandoc.Options            ( WriterOptions (..) )
import           Text.Pandoc.Readers            ( readHtml )
import           Text.Pandoc.Writers            ( writeHtml5 )
import           Text.Pandoc.Writers.Shared     ( toTableOfContents )
import Data.Either (fromRight)

import Debug.Trace
import Data.Typeable
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

data ToCExtra =  ToCExtra { extraUlClasses :: String }
     deriving (Show)
instance Default ToCExtra where
    def = ToCExtra { extraUlClasses = "" }

tocField :: String -> Int -> ToCExtra -> String -> Context String
tocField name depth tocExtra snapshot = field name $ \item -> do
    body <- loadSnapshot (itemIdentifier item) snapshot

    let writerOptions = def { writerTOCDepth = depth }

        pandoc@(Pandoc _ blocks) = case (runPure $ readHtml defaultHakyllReaderOptions (T.pack $ itemBody body))
                 of
                    Left err    -> error $ "Could not parse"
                    Right pandoc -> pandoc

        toc = toTableOfContents writerOptions blocks

        ulAttributes ul' = ul'
            ! class_ (fromString $ extraUlClasses tocExtra)

    return . TL.unpack . renderHtml . modList writerOptions ulAttributes $ [toc]

modList :: WriterOptions -> ((Html -> Html)->(Html->Html))  -> [Block] -> Html
modList opts ulMod = makeBulletItem
  where
      -- This decomposes one item of a bullet list
      -- BulletList takes a  list of items each of which is a list of blocks
      -- respectively :: [[Block]]
      makeBulletItem :: [Block] -> Html
      makeBulletItem [] = Empty ()
      makeBulletItem ((BulletList elems):extra)
        = toHtml [makeList $ filter (not . null) elems, makeBulletItem extra]
      makeBulletItem (block:extra) = toHtml [makeItem block, makeBulletItem extra]

      makeItem :: Block -> Html
      makeItem block = fromRight (Empty ()) (runPure $ writeHtml5 opts (Pandoc nullMeta [block]))

      makeList:: [[Block]] -> Html
      makeList [] = Empty ()
      makeList listItems = (ulMod ul) $ (toHtml . map (li . makeBulletItem) $ listItems)

-- plainTocField :: String -> Int -> String -> Context String
-- plainTocField name depth snapshot = field name $ \item -> do

--     body <- loadSnapshot (itemIdentifier item) snapshot
--     template <- read "$toc$"
--     let writerOptions = def
--             {
--               writerTableOfContents = True
--             , writerTOCDepth = depth
--             , writerTemplate = Just template
--             }
--         toc = case runPure (readHtml defaultHakyllReaderOptions
--                                      (T.pack $ itemBody body))
--                >>= \pandoc -> runPure ( writeHtml5String writerOptions pandoc) of
--                    Left err    -> fail $ ""
--                    Right item' -> T.unpack item'

--     return toc

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
