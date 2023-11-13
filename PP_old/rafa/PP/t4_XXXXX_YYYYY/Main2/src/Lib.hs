module Lib
    ( someFunc
    ) where

import Test.QuickCheck (quickCheck)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
