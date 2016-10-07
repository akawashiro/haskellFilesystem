import FileSystem
import Data.List.Split
import Control.Monad.State


stringToPath :: String -> Path
stringToPath s = splitOn "/" s

-- take input(command) and return FileDir and output
processInput :: String -> State FileDir String
processInput input = do
    fd <- get
    put $ processInput' input
    

main = undefined
