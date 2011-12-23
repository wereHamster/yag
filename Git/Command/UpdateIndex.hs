module Git.Command.UpdateIndex (run) where

import System.IO
import System.Exit
import System.Console.GetOpt

import Data.List

data Flag = Add | Remove
    deriving (Eq, Ord, Show)

flags = [
    Option [] ["add"]       (NoArg Add)     "Add files to the index",
    Option [] ["remove"]    (NoArg Remove)  "Remove files from the index" ]

displayHelp errs = do
    hPutStrLn stderr (concat errs ++ usageInfo header flags) >> exitWith (ExitFailure 1)
    where header = "Usage: yag update-index [...]"

parse argv = case getOpt Permute flags argv of
    (args, xs, []) -> do
        return $ nub (concatMap (:[]) args)
 
    -- Errors while parsing the arguments.
    (_, _, errs) -> do displayHelp errs

run :: [String] -> IO ()
run argv = do
    args <- parse argv
    putStrLn $ show args
    if null args
        then exitWith (ExitFailure 1)
        else exitWith ExitSuccess
