-- | Terminal control. Internal QuickCheck module.
module Test.QuickCheck.Text
  ( Str(..)
  , ranges

  , number
  , short
  , showErr
  , oneLine
  , isOneLine
  , bold

  , newTerminal
  , withStdioTerminal
  , withNullTerminal
  , terminalOutput
  , handle
  , Terminal
  , putTemp
  , putPart
  , putLine
  )
 where

--------------------------------------------------------------------------
-- imports

import Control.Applicative
import System.IO
  ( hFlush
  , hPutStr
  , stdout
  , stderr
  , Handle
  , BufferMode (..)
  , hGetBuffering
  , hSetBuffering
  )

import Data.IORef
import Test.QuickCheck.Exception

--------------------------------------------------------------------------
-- literal string

newtype Str = MkStr String

instance Show Str where
  show (MkStr s) = s

ranges :: (Show a, Integral a) => a -> a -> Str
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

oneLine :: String -> String
oneLine = unwords . words

isOneLine :: String -> Bool
isOneLine xs = '\n' `notElem` xs

bold :: String -> String
-- not portable:
--bold s = "\ESC[1m" ++ s ++ "\ESC[0m"
bold s = s -- for now

--------------------------------------------------------------------------
-- putting strings

data Terminal
  = MkTerminal (IORef (IO ())) Output Output

data Output
  = Output (String -> IO ()) (IORef String)

newTerminal :: Output -> Output -> IO Terminal
newTerminal out err =
  do ref <- newIORef (return ())
     return (MkTerminal ref out err)

withBuffering :: IO a -> IO a
withBuffering action = do
  mode <- hGetBuffering stderr
  -- By default stderr is unbuffered.  This is very slow, hence we explicitly
  -- enable line buffering.
  hSetBuffering stderr LineBuffering
  action `finally` hSetBuffering stderr mode

withStdioTerminal :: (Terminal -> IO a) -> IO a
withStdioTerminal action = do
  out <- output (handle stdout)
  err <- output (handle stderr)
  withBuffering (newTerminal out err >>= action)

withNullTerminal :: (Terminal -> IO a) -> IO a
withNullTerminal action = do
  out <- output (const (return ()))
  err <- output (const (return ()))
  newTerminal out err >>= action

terminalOutput :: Terminal -> IO String
terminalOutput (MkTerminal _ out _) = get out

handle :: Handle -> String -> IO ()
handle h s = do
  hPutStr h s
  hFlush h

output :: (String -> IO ()) -> IO Output
output f = do
  r <- newIORef ""
  return (Output f r)

put :: Output -> String -> IO ()
put (Output f r) s = do
  f s
  modifyIORef r (++ s)

get :: Output -> IO String
get (Output _ r) = readIORef r

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
     put out s

putTemp tm@(MkTerminal _ _ err) s =
  do flush tm
     put err (s ++ [ '\b' | _ <- s ])
     postpone tm $
       put err ( [ ' ' | _ <- s ]
              ++ [ '\b' | _ <- s ]
               )

putLine tm@(MkTerminal _ out _) s =
  do flush tm
     put out (s ++ "\n")

--------------------------------------------------------------------------
-- the end.
