{-# LANGUAGE PackageImports #-}

import Debug.Trace (trace)
import Text.Parsec (parse)
import PazLexer
import PazParser
import PazPrettify
import System.Environment
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

-- copy from the framework provided
die :: String -> IO ()
die err = do
    hPutStrLn stderr err
    exitFailure

-- the tracing code is copied from the provided framework
tokenise :: String -> IO ()
tokenise text =
    case
        trace
             "*** Lexical analysis"
             (parse PazLexer.parseStartSymbol "(file)" text)
        of
        Left error ->
            die ("Lexical error:\n" ++ show error)
        Right tokens ->
            case
                trace
                    "*** Syntax analysis"
                    (parse PazParser.parseStartSymbol "(file)" tokens)
                of
                Left error ->
                    die ("Syntax error:\n" ++ show error)
                Right ast ->
                    prettyPrint ast

parseArgs :: [String] -> IO ()
parseArgs ["-p", f] = readFile f >>= tokenise
parseArgs [f] = die "Sorry, cannot generate code yet"
parseArgs _ = die "undefined action"

main :: IO ()
main = getArgs >>= parseArgs
