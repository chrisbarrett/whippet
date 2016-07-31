{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad                (forM_)
import qualified Errors                       as Errors
import qualified Language.Whippet.Parser      as Parser
import qualified Language.Whippet.Typecheck   as Typecheck
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

    Parser.parseFile optFile >>= \case
        Trifecta.Success ast ->
            case Typecheck.typecheck ast of

              Left errs -> do

                  forM_ errs $
                    printDoc stderr . Errors.pprint optFile

                  printDoc stdout (PP.red "Type checking failed.")
                  Exit.exitFailure

              Right _ ->
                  pure ()

        Trifecta.Failure errs -> do
            printDoc stderr errs
            printDoc stdout (PP.red "Parsing failed.")
            Exit.exitFailure

    if optCheckOnly
      then do
        printDoc stdout (PP.dullgreen "Finished.")
        Exit.exitSuccess
      else do
        printDoc stdout (PP.red "Code emission not implemented.")
        Exit.exitFailure


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
