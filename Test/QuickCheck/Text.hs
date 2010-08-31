module Test.QuickCheck.Text
  ( Str(..)
  , ranges
  
  , number
  , short
  , showErr
  , bold
  
  , newTerminal
  , newStdioTerminal
  , newNullTerminal
  , handle
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
  , Handle
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

data Terminal
  = MkTerminal (IORef (IO ())) (String -> IO ()) (String -> IO ())

newTerminal :: (String -> IO ()) -> (String -> IO ()) -> IO Terminal
newTerminal out err =
  do ref <- newIORef (return ())
     return (MkTerminal ref out err)

newStdioTerminal :: IO Terminal
newStdioTerminal = newTerminal (handle stdout) (handle stderr)

newNullTerminal :: IO Terminal
newNullTerminal = newTerminal (const (return ())) (const (return ()))

handle :: Handle -> String -> IO ()
handle h s = do
  hPutStr h s
  hFlush h

flush :: Terminal -> IO ()
flush (MkTerminal ref _ _) =
  do io <- readIORef ref
     writeIORef ref (return ())
     io

postpone :: Terminal -> IO () -> IO ()
postpone (MkTerminal ref _ _) io' =
  do io <- readIORef ref
     writeIORef ref (io >> io')

putPart, putTemp, putLine :: Terminal -> String -> IO ()
putPart tm@(MkTerminal _ out _) s =
  do flush tm
     out s
     
putTemp tm@(MkTerminal _ _ err) s =
  do flush tm
     err s
     err [ '\b' | _ <- s ]
     postpone tm $
       err ( [ ' ' | _ <- s ]
          ++ [ '\b' | _ <- s ]
           )
     
putLine tm@(MkTerminal _ out _) s =
  do flush tm
     out s
     out "\n"

--------------------------------------------------------------------------
-- the end.
