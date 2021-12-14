# OCaml-interpreter-in-OCaml

## Install OCaml

- Ubuntu

```
$ sudo apt install ocaml
```

- Mac

```
$ brew install ocaml
```

その他の場合は [公式ページ](https://ocaml.org/docs/install.html) を参照．

## How to use

- 準備

```
$ git clone https://github.com/mamizu-git/OCaml-interpreter-in-OCaml.git
$ cd OCaml-interpreter-in-OCaml
$ make
```

- 起動

```
$ ./main
```

- 終了

`Ctrl` + `C`

## What I implemented

- 整数の四則演算
- ブール値の演算
- 変数定義
- 関数定義・関数適用 (再帰・相互再帰に対応)
- 型検査・型推論 (let多相に対応)
- リスト型・タプル型
- パターンマッチング
- コメント・エラー処理
