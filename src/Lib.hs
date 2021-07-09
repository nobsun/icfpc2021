-- # 雛形モジュール
-- このファイルは`stack new`コマンドで自動的に`src/`に挿入されます
-- 
-- ## 言語拡張と`module`宣言
-- 最低限の指定をしてある

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

-- ## `doctest`のための記述と定義本体
-- テストは失敗するように書いてある

{- | 
「なんか関数」を標準出力に印字する
>>> someFunc
なんか関数
-}
someFunc :: IO ()
someFunc = putStrLn "なんか函数"
