
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Data.List

import qualified Git.Command.Init           as Init
import qualified Git.Command.RevParse       as RevParse
import qualified Git.Command.UpdateIndex    as UpdateIndex
import qualified Git.Command.WriteTree      as WriteTree

data Flag = Help | Verbose | TemplateDirectory (Maybe String)
    deriving (Eq, Ord, Show)

flags =
    [ Option ['h'] [] (NoArg Help)    "Display help"
    , Option ['v'] [] (NoArg Verbose) "Be verbose"
    , Option [] ["template"] (OptArg TemplateDirectory "dir") "Be verbose"
    ]

-- | Display help and then exit with success.
displayHelp errs = do
    hPutStrLn stderr (concat errs ++ usageInfo header flags) >> exitWith (ExitFailure 1)
    where header = "Usage: yag [-v] <command> [...]"


-- The arguments to can be split into two parts: global args, and per-command
-- args. This methods splits the commands into the two parts and extracts the
-- command name.
--   - git <global args> <command> <command args>
splitArgs :: [String] -> IO ([String], String, [String])
splitArgs argv = do
    let ga = takeWhile (\x -> head x == '-') argv
    let gal = length ga

    if length argv <= gal
        then return (ga, "No Command Specified", [])
        else return (ga, argv !! gal, drop (gal + 1) argv)

-- Parse arguments, return something.. uhm.. useful.
parse (argv, cmd, rest) = case getOpt Permute flags argv of
    (args, xs, []) -> do
        if Help `elem` args
            then do displayHelp []
            else return $ (nub (concatMap (:[]) args), cmd, rest)

    -- Errors while parsing the arguments.
    (_, _, errs) -> do displayHelp errs

main :: IO ()
main = do
    (args, cmd, xs) <- parse =<< splitArgs =<< getArgs
    runCommand cmd xs


runCommand :: String -> [String] -> IO ()

runCommand "init"               = Init.run
runCommand "rev-parse"          = RevParse.run
runCommand "update-index"       = UpdateIndex.run
runCommand "write-tree"         = WriteTree.run

runCommand cmd = \_ -> do
    putStrLn $ "Unknown command: " ++ cmd
    exitWith (ExitFailure 1)
