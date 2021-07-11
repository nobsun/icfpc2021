# 準備

以下の環境変数を設定してください。

```
export ICFP2021_API_TOKEN=<<team-token>>
```

トークンについては以下のリンク先にあります。

参考: https://sampou.slack.com/archives/C027JJJ6CE6/p1625914727171900?thread_ts=1625914647.171800&cid=C027JJJ6CE6

# 前回取得結果を一旦削除

これは wget で 1.1 などのドット付のファイルやフォルダが作られないようにするための措置です。
TODO: wget のオプションでもっとスマートに対応できそう。

```
$ seq 1 106 | xargs rm -rf
```

# 実行

上記の環境変数を設定したことを確認して、このディレクトリで以下コマンド実行してください。

```
$ ./get-submitted.sh
```

# git へ追加登録

```
$ seq 1 106 | xargs git add
```

ステージングしたら git commit して push すればおしまい。

