import Data.List.Split
import Control.Monad.State

data FileDir =
      File {fileName::String,fileContent::String}
    | Dir {dirName::String,dirContent::[FileDir]} deriving Show

f = File "aa" "bb"
f1 = File "xx" "yy"
d = Dir  "cc" [f]


type Path = [String]

stringToPath :: String -> Path
stringToPath s = splitOn "/" s

-- Make a file or directory in the directroy assingned by the path
touch :: Path -> FileDir -> State FileDir Bool
touch p f = do
    fd <- get
    if isDirExist p fd 
        then do
            put $ touch' p f fd
            return True
        else return False
    
touch' (s:[]) f (Dir n fds) = if s==n then Dir n (f : fds) else Dir n fds
touch' (s:ss) f (Dir n fds) = if s==n then Dir n (map (touch' ss f) fds) else Dir n fds
touch' _ _ fd = fd

isDirExist [] _ = True
isDirExist (s:ss) (Dir n fds) = if s==n then any (isDirExist ss) fds else False
isDirExist _ _ = False

-- cat a file assingned by the path
cat :: Path -> State FileDir String
cat p = do
    fd <- get
    if isFileExist p fd then return (cat' p fd) else return ""

cat' [s] (File n c) = c
cat' (s:ss) (Dir n fds) = if s==n then concat (map (cat' ss) fds) else ""
cat' _ _ = ""

isFileExist [s] (File n _) = s==n
isFileExist (s:ss) (Dir n fds) = if s==n then any (isFileExist ss) fds else False
isFileExist _ _ = False

checkTouch = runState (touch ["cc"] f1) d


main = undefined
