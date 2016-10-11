import FileSystem
import Data.List.Split
import Control.Monad.State


stringToPath :: String -> Path
stringToPath s = splitOn "/" s

processInput :: StateT FileDir IO ()
processInput = do
    lift $ putStr "> "
    input <- (lift getLine)
    fd <- get
    put (snd (runState (processInput' input) fd))
    lift $ putStrLn (fst ((runState (processInput' input)) fd))
    processInput

-- take input(command) and return FileDir and output
processInput' :: String -> State FileDir String
processInput' input = 
    case head $ splitOn " " input of
        "cat" -> cat firstpath >>= return
        "rm" -> do
            f <- rm firstpath
            if f then return "Succeeded in remove." else return "Failed in remove"
        "mv" -> do
            f <- mv firstpath secondpath
            if f then return "Succeeded in mv." else return "Failed in mv"
        "cp" -> do
            f <- cp firstpath secondpath
            if f then return "Succeeded in cp." else return "Failed in cp"
        "rename" -> do
            f <- rename firstpath (head secondpath)
            if f then return "Succeeded in rename." else return "Failed in rename"
        "mkfile" -> do
            f <- mkFile firstpath (head secondpath) (head thirdpath)
            if f then return "Succeeded in mkfile." else return "Failed in mkfile"
        "mkdir" -> do
            f <- mkDir firstpath (head secondpath)
            if f then return "Succeeded in mkdir." else return "Failed in mkdir"
        "tree" -> tree >>= return
        _ -> do
            return "Cannot parse input. Please try again."
    where
        firstpath = stringToPath $ head $ tail $ (splitOn " " input)
        secondpath = stringToPath $ head $ tail $ tail $ (splitOn " " input)
        thirdpath = stringToPath $ head $ tail $ tail $ tail $ (splitOn " " input)

main = runStateT processInput root >> return ()
