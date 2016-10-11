#haskellで作る超簡易ファイルシステム

##概要
haskellで作られた超簡易ファイルシステム。ファイルの作成、削除、リネームができる。

##動作の様子
(https://github.com/akawashiro/haskell-filesystem/blob/master/haskell-filesystem-intro.gif)

##使えるコマンド
注意:このプログラムにはカレントディレクトリの概念が無いので常に絶対パスで指定する必要がある。

###mv
mv ファイル名orディレクトリ名 ディレクトリ名
の形でしか呼び出せない
つまりファイル名orディレクトリ名を変更することはできない

###cp
cp ファイル名orディレクトリ名 ディレクトリ名
の形でしか呼び出せない
つまりファイル名orディレクトリ名を変更することはできない

###mkfile
mkfile ファイル名 中身
の形で呼び出す。

###cat
ファイルの中身を表示する。

###rm
ディレクトリの削除もrmコマンドで出来る

###mkdir
mkdir ディレクトリ名

###rename
rename ファイル名orディレクトリ名(絶対パスで) 新しい名前

