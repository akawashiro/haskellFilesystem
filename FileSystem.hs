module FileSystem where

import Data.List.Split
import Control.Monad.State

data FileDir =
      File {fileName::String,fileContent::String}
    | Dir {dirName::String,dirContent::[FileDir]} deriving Show

f = File "aa" "bb"
root = Dir "" [File "hello" "HELLO",Dir "usr" [Dir "local" []],Dir "var" [File "http" "world"],Dir "proc" [],Dir "etc" []]
f1 = File "xx" "yy"
d = Dir  "cc" [f,Dir "f" []]

type Path = [String]

-- tree command on root
tree :: State FileDir String
tree = do
    fd <- get
    return (tree' "" fd)

tree' :: String -> FileDir -> String
tree' parentPath (File n c) = parentPath++n++"\n"
tree' parentPath (Dir n fds) = concat (map (tree' (parentPath++n++"/")) fds)++parentPath++n++"/"++"\n"

-- Make a file or directory in the directroy assingned by the path
mkFileDir :: Path -> FileDir -> State FileDir Bool
mkFileDir p f = do
    fd <- get
    if isDirExist p fd 
        then do
            put $ mkFileDir' p f fd
            return True
        else return False

mkFileDir' (s:[]) f (Dir n fds) = if s==n then Dir n (f : fds) else Dir n fds
mkFileDir' (s:ss) f (Dir n fds) = if s==n then Dir n (map (mkFileDir' ss f) fds) else Dir n fds
mkFileDir' _ _ fd = fd

isDirExist [] _ = True
isDirExist (s:ss) (Dir n fds) = if s==n then any (isDirExist ss) fds else False
isDirExist _ _ = False

-- Make a file in specified path
mkFile :: Path -> String -> String -> State FileDir Bool
mkFile p n c = mkFileDir p (File n c)

-- Make a directory in specified path
mkDir :: Path -> String -> State FileDir Bool
mkDir p n = mkFileDir p (Dir n [])

-- cat a file assingned by the path
cat :: Path -> State FileDir String
cat p = do
    fd <- get
    if isFileExist p fd then return (cat' p fd) else return "Cannot find such file."

cat' [s] (File n c) = c
cat' (s:ss) (Dir n fds) = if s==n then concat (map (cat' ss) fds) else ""
cat' _ _ = ""

isFileExist [s] (File n _) = s==n
isFileExist (s:ss) (Dir n fds) = if s==n then any (isFileExist ss) fds else False
isFileExist _ _ = False

-- rm and rm -r command
-- This command cannot deal with a filesystem which contain only one File at its root
rm :: Path -> State FileDir Bool
rm p = do
    fd <- get
    if isDirExist p fd || isFileExist p fd
    then do
        put $ rm' p fd
        return True
    else
        return False

rm' :: Path -> FileDir -> FileDir
rm' (s:t:[]) (Dir n fds) = if s==n 
                           then Dir n (filter (\x -> not (t==getFileDirName x)) fds)
                           else Dir n fds
rm' (s:ss) (Dir n fds) = if s==n then (Dir n (map (rm' ss) fds))
                                 else (Dir n fds)
rm' _ fd = fd


getFileDirName (Dir n _) = n
getFileDirName (File n _) = n

-- mv command
mv :: Path -> Path -> State FileDir Bool
mv p1 p2 = do
    fd <- get
    if (isDirExist p1 fd || isFileExist p1 fd) && isDirExist (pathToDirName p2) fd
    then do
        put $ mv' p1 p2 fd
        return True
    else return False

mv' :: Path -> Path -> FileDir -> FileDir
mv' p1 p2 fd = mkFileDir' p2 (getFileDir p1 fd) (rm' p1 fd)

-- Caution! This function must be use after checked by isDirExist or isFileExist.
getFileDir :: Path -> FileDir -> FileDir
getFileDir (s:t:[]) (Dir n fds) = head (filter (\x -> (t==getFileDirName x)) fds)
getFileDir (s:t:us) (Dir n fds) = getFileDir (t:us) $ head (filter (\x -> (t==getFileDirName x)) fds)

pathToDirName [s] = []
pathToDirName (s:ss) = s:pathToDirName ss

-- cp command
cp :: Path -> Path -> State FileDir Bool
cp p1 p2 = do
    fd <- get
    if (isDirExist p1 fd || isFileExist p1 fd) && isDirExist (pathToDirName p2) fd
    then do
        put $ cp' p1 p2 fd
        return True
    else return False

cp' :: Path -> Path -> FileDir -> FileDir
cp' p1 p2 fd = mkFileDir' p2 (getFileDir p1 fd) fd

-- rename command
rename :: Path -> String -> State FileDir Bool
rename p s = do
    fd <- get
    if isDirExist p fd || isFileExist p fd
    then do
        put $ rename' p s fd
        return True
    else 
        return False

rename' [t] s (Dir n fds) = if t==n then (Dir s fds) else (Dir n fds)
rename' [t] s (File n c) = if t==n then (File s c) else (File n c)
rename' (t:ts) s (Dir n fds) = if t==n then Dir n (map (rename' ts s) fds) else Dir n fds
rename' _ _ fd = fd

