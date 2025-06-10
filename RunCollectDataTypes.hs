{-# LANGUAGE TemplateHaskell #-}
import CollectDataTypes
import Control.Monad

main :: IO ()
main =
  forM_ $(doTheMagicStuff "base") $ \(dt, result) ->
    putStrLn (haskellName dt ++ ": " ++ show result)
