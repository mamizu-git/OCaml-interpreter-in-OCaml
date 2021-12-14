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

ここで rlwrap をインストールし

```
$ rlwrap ./main
```

とするとインタプリタの操作が快適になる．

- 終了

`Ctrl` + `C`

## What I implemented

- 整数の四則演算
```
# 1 + 1;;
- : int = 2
# 5 * (3 - 1);;
- : int = 10
# (4 + 1) / 3;;
- : int = 1
```
- ブール値の演算
```
# true || false;;
- : bool = true
# true && false;;
- : bool = false
# 1 == 1;;
- : bool = true
# (1 == 1) && (2 == 1);;
- : bool = false
```
- 変数定義
```
# let x = 1 in x + 1;;
- : int = 2
# let x = 2;;
val x : int = 2
# x * 4;;  
- : int = 8
# let y = 2 in let z = 3 in y * z;;
- : int = 6
```
- 関数定義・関数適用 (再帰・相互再帰に対応)
```
# fun x -> x + 1;;
- : (int -> int) = <fun>
# (fun x -> x + 1) 10;;
- : int = 11
# let f x = x * 3;;
val f : (int -> int) = <fun>
# f 10;;
- : int = 30
# let rec fib n = if n = 1 || n = 2 then 1 else fib (n-1) + fib (n-2);;
val fib : (int -> int) = <fun>
# fib 10;;
- : int = 55
# let rec even n = if n = 0 then true else odd (n-1) and odd n = if n = 0 then false else even (n-1);;
val even : (int -> bool) = <fun>
val odd : (int -> bool) = <fun>
# even 10;;
- : bool = true
# odd 10;;
- : bool = false
```
- 型検査・型推論 (let多相に対応)
```
# 1 + true;;
Error: type error
# let f = fun x -> x + 1;;
val f : (int -> int) = <fun>
# f true;;
Error: type error
# let f x y z = x + y + z;;
val f : (int -> (int -> (int -> int))) = <fun>
# f 1 2 3;;
- : int = 6
# let id x = x;;
val id : ('a14 -> 'a14) = <fun>
# (id 0, id true);;
- : (int * bool) = (0, true)
```
- リスト型・タプル型
```
# (1,2);;
- : (int * int) = (1, 2)
# (fun x -> x + 1, (false, 3));;
- : ((int -> int) * (bool * int)) = (<fun>, (false, 3))
# let f x y = (x,y);;
val f : ('a2 -> ('a3 -> ('a2 * 'a3))) = <fun>
# f 2 false;;
- : (int * bool) = (2, false)
# [];;
- : 'a8 list = []
# 1::2::3::[];;
- : int list = [1; 2; 3]
# let rec f n = if n = 0 then [] else n::(f (n-1));;
val f : (int -> int list) = <fun>
# f 10;;
- : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]
# [1;2;3];;
- : int list = [1; 2; 3]
```
- パターンマッチング
```
# match 1 with
  | 1 -> 2
  | 2 -> 3;;
- : int = 2
# match (1,2) with
  | (2,x) -> 0
  | (x,2) -> 1
- : int = 1
# match 1 with
  | 0 -> 1
  | 2 -> 3
  | _ -> 0;;
# let rec f x =
    match x with
    | y::ys -> y + f ys
    | [] -> 0;;
val f : (int list -> int) = <fun>
# f [1;2;3;4];;
- : int = 10
```
- コメント・エラー処理
```
# 1 + (* comment *) 1;;
- : int = 2
# 1 + (* comment1 (* comment2 *)*) 2;;
- : int = 3
# if 2 then 3 else 4;;
Error: type error
# 1 ? 2;;
Error: Syntax error
# 1 + + 1;;
Error: cannot parse
# 3 / 0;;
Exception: Division_by_zero.
# let x = y;;
Error: Unbound value y
# x;;
Error: Unbound value x
```
