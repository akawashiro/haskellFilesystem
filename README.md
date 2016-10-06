#haskellで作る超簡易ファイルシステム

##概要
ファイルシステムまがいのものを作る。
階層構造が表現でき、新しいファイルの追加、削除、リネームが可能なものを作ることが目標。

##実装するもの

###mv
実装済み。ただし
mv ファイル名orディレクトリ名 ディレクトリ名
の形でしか呼び出せない
つまりファイル名orディレクトリ名を変更することはできない

###cp
実装済み。
cp ファイル名orディレクトリ名 ディレクトリ名
の形でしか呼び出せない
つまりファイル名orディレクトリ名を変更することはできない

###touch
mkFileDirとして実装済み

###cat
実装済み

###rm
実装済み。ディレクトリの削除もrmコマンドで出来る

###mkdir
mkFileDirとして実装済み

###rename
