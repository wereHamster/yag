
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Data.List
import Data.Maybe

import Git.Repository
import Git.Revision

data Flag = Help | Verbose
    deriving (Eq, Ord, Enum, Show, Bounded)

flags = [
    Option ['h'] [] (NoArg Help)    "Display help",
    Option ['v'] [] (NoArg Verbose) "Be verbose" ]

-- Display help and then exit with success.
displayHelp errs = do
    hPutStrLn stderr (concat errs ++ usageInfo header flags) >> exitWith ExitSuccess
    where header = "Usage: yagc [-v] <command> [...]"

-- Parse arguments, return something.. uhm.. useful.
parse argv = case getOpt Permute flags argv of
    -- No command specified.
    (args, [], []) -> do displayHelp []

    -- Have command and no errors.
    (args, (cmd:xs), []) -> do
        if Help `elem` args
            then do displayHelp []
            else return (nub (concatMap (:[]) args), cmd, xs)
 
    -- Errors while parsing the arguments.
    (_, _, errs) -> do displayHelp errs

main :: IO ()
main = do
    (args, cmd, xs) <- getArgs >>= parse
    runCommand cmd xs
    return ()

runCommand :: String -> [String] -> IO ()

-- Command: rev-parse
runCommand "rev-parse" []     = return ()
runCommand "rev-parse" (x:xs) = do
    repo <- (findRepository ".") >>= return . fromJust
    rev <- resolveRevision repo $ fromJust . parseRevision $ x
    putStrLn $ show $ fromJust rev
    return ()
