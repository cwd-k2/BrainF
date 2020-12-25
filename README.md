# BrainF

Haskell で BrainF\*\*k を実装してみる.

## Install

初回は色々ダウンロードしてくるので気をつける.

```sh
stack install
```

## Test

```sh
stack test
```

## Run

```sh
stack exec BrainF
```

または, `stack install` 済みであり, `PATH` の通ったところに置かれていれば次.

```sh
BrainF <filename>
```

## Example

`examples/` 以下に `helloworld.bf` などがあるので, それで試す.

```sh
stack exec BrainF examples/helloworld.bf
```

インストール済みならば次.

```sh
BrainF examples/echo.bf
```

## Optional

環境変数 `BF_NO_BUFFERING` に何らかの値がセットされていれば入出力に関してバッファリングをしないようにしてある. わざわざ入力を確定するのに Enter 押下しないようにできる.

```sh
BF_NO_BUFFERING=1 BrainF <foobar.bf>
```
