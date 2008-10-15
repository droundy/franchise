{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import Prelude
import Foreign

foreign import ccall "test.h tester" tester :: IO ()

main = tester >> putStrLn "Done."
