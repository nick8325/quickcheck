module Chalmers.QuickCheck.Text
  ( Str(..)
  , ranges
  
  , number
  , short
  , showErr
  , bold
  
  , newTerminal
  , Terminal
  , putTemp
  , putPart
  , putLine
  )
 where

--------------------------------------------------------------------------
-- imports

import System.IO
  ( hFlush
  , hPutStr
  , stdout
  , stderr
  )

import Data.IORef

--------------------------------------------------------------------------
-- literal string

newtype Str = MkStr String

instance Show Str where
  show (MkStr s) = s

ranges :: Integral a => a -> a -> Str
ranges k n = MkStr (show n' ++ " -- " ++ show (n'+k-1))
 where
  n' = k * (n `div` k)

--------------------------------------------------------------------------
-- formatting

number :: Int -> String -> String
number n s = show n ++ " " ++ s ++ if n == 1 then "" else "s"

short :: Int -> String -> String
short n s
  | n < k     = take (n-2-i) s ++ ".." ++ drop (k-i) s
  | otherwise = s
 where
  k = length s
  i = if n >= 5 then 3 else 0

showErr :: Show a => a -> String
showErr = unwords . words . show

bold :: String -> String
-- not portable:
--bold s = "\ESC[1m" ++ s ++ "\ESC[0m"
bold s = s -- for now

--------------------------------------------------------------------------
-- putting strings

newtype Terminal
  = MkTerminal (IORef (IO ()))

newTerminal :: IO Terminal
newTerminal =
  do hFlush stdout
     hFlush stderr
     ref <- newIORef (return ())
     return (MkTerminal ref)

flush :: Terminal -> IO ()
flush (MkTerminal ref) =
  do io <- readIORef ref
     writeIORef ref (return ())
     io

postpone :: Terminal -> IO () -> IO ()
postpone (MkTerminal ref) io' =
  do io <- readIORef ref
     writeIORef ref (io >> io')

putPart, putTemp, putLine :: Terminal -> String -> IO ()
putPart tm s =
  do flush tm
     putStr s
     hFlush stdout
     
putTemp tm s =
  do flush tm
     hPutStr h s
     hPutStr h [ '\b' | _ <- s ]
     hFlush h
     postpone tm $
       do hPutStr h ( [ ' ' | _ <- s ]
                   ++ [ '\b' | _ <- s ]
                    )
 where
  --h = stdout
  h = stderr
     
putLine tm s =
  do flush tm
     putStrLn s
     hFlush stdout    

--------------------------------------------------------------------------
-- the end.
