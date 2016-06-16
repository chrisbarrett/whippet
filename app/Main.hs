{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad                (when)
import           Language.Whippet.AST         (TopLevel)
import qualified Language.Whippet.Parser      as Parser
import           Options.Applicative
import qualified System.Environment           as Environment
import qualified System.Exit                  as Exit
import           System.IO                    (stderr, stdout)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.Trifecta                as Trifecta

main :: IO ()
main = do
    Opts {..} <- execParser . options =<< Environment.getProgName

    let printDoc h d =
            let useColour = if optNoColour then PP.plain d else d
                settings = PP.renderPretty 1 100 useColour
            in PP.displayIO h settings

    ast <- Parser.parseFile optFile
    case ast of
        Trifecta.Success _  ->
            when optCheckOnly $ do
                printDoc stdout (PP.dullgreen "Parsed with no errors.")
                Exit.exitSuccess
        Trifecta.Failure errs -> do
            printDoc stderr errs
            printDoc stdout (PP.red "Aborted with errors.")
            Exit.exitFailure

    printDoc stdout (PP.dullgreen "Finished with no errors.")

-- * Option parsing

data Opts = Opts {
    optNoColour  :: Bool
  , optCheckOnly :: Bool
  , optFile      :: FilePath
  }

options :: String -> ParserInfo Opts
options prog = info (helper <*> opts) desc
    where
      desc = fullDesc
          <> header (prog ++ " - whippet language reference compiler")
          <> progDesc "Compiles whippet language files into executable code."
      opts =
          Opts <$> switch (long "plain" <> help "Remove ANSI colour codes from output.")
               <*> switch (long "check-only" <> help "Stop after type-checking")
               <*> argument str (metavar "FILES" <> help "Path to the file to compile")
