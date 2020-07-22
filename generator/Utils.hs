{-# LANGUAGE OverloadedStrings #-}
module Utils () where
import           Hakyll

import           Text.Pandoc.Options            ( WriterOptions (..) )
import           System.Process
import           System.Exit                    ( ExitCode(..) )



-- runPandocFilter :: Item String -> String
-- runPandocFilter filter = do
--     let (status, stdout, _) = readProcessWithExitCode filter [
--         "log",
--         "-1",
--         "--format=",
--         "--"] ""

--     return $ case status  of
--         ExitSuccess -> stdout
--         _           -> ""
