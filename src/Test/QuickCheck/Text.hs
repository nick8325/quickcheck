{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
{-# OPTIONS_HADDOCK hide #-}
-- | Terminal control and text helper functions. Internal QuickCheck module.
module Test.QuickCheck.Text
  ( Str(..)
  , ranges

  , number
  , short
  , showErr
  , oneLine
  , isOneLine
  , bold
  , ljust, rjust, centre, lpercent, rpercent, lpercentage, rpercentage
  , drawTable, Cell(..)
  , paragraphs

  , newTerminal
  , withStdioTerminal
  , withHandleTerminal
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

import System.IO
  ( hFlush
  , hPutStr
  , stdout
  , stderr
  , Handle
  , BufferMode (..)
  , hGetBuffering
  , hSetBuffering
  , hIsTerminalDevice
  )

import Data.IORef
import Data.List (intersperse, transpose)
import Text.Printf
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

ljust n xs = xs ++ replicate (n - length xs) ' '
rjust n xs = replicate (n - length xs) ' ' ++ xs
centre n xs =
  ljust n $
  replicate ((n - length xs) `div` 2) ' ' ++ xs

lpercent, rpercent :: (Integral a, Integral b) => a -> b -> String
lpercent n k =
  lpercentage (fromIntegral n / fromIntegral k) k

rpercent n k =
  rpercentage (fromIntegral n / fromIntegral k) k

lpercentage, rpercentage :: Integral a => Double -> a -> String
lpercentage p n =
  printf "%.*f" places (100*p) ++ "%"
  where
    -- Show no decimal places if k <= 100,
    -- one decimal place if k <= 1000,
    -- two decimal places if k <= 10000, and so on.
    places :: Integer
    places =
      ceiling (logBase 10 (fromIntegral n) - 2 :: Double) `max` 0

rpercentage p n = padding ++ lpercentage p n
  where
    padding = if p < 0.1 then " " else ""

data Cell = LJust String | RJust String | Centred String deriving Show

text :: Cell -> String
text (LJust xs) = xs
text (RJust xs) = xs
text (Centred xs) = xs

-- Flatten a table into a list of rows
flattenRows :: [[Cell]] -> [String]
flattenRows rows = map row rows
  where
    cols = transpose rows
    widths = map (maximum . map (length . text)) cols

    row cells = concat (intersperse " " (zipWith cell widths cells))
    cell n (LJust xs) = ljust n xs
    cell n (RJust xs) = rjust n xs
    cell n (Centred xs) = centre n xs

-- Draw a table given a header and contents
drawTable :: [String] -> [[Cell]] -> [String]
drawTable headers table =
  [line] ++
  [border '|' ' ' header | header <- headers] ++
  [line | not (null headers) && not (null rows)] ++
  [border '|' ' ' row | row <- rows] ++
  [line]
  where
    rows = flattenRows table

    headerwidth = maximum (0:map length headers)
    bodywidth = maximum (0:map length rows)
    width = max headerwidth bodywidth

    line = border '+' '-' $ replicate width '-'
    border x y xs = [x, y] ++ centre width xs ++ [y, x]

paragraphs :: [[String]] -> [String]
paragraphs = concat . intersperse [""] . filter (not . null)

bold :: String -> String
-- not portable:
--bold s = "\ESC[1m" ++ s ++ "\ESC[0m"
bold s = s -- for now

--------------------------------------------------------------------------
-- putting strings

data Terminal
  = MkTerminal (IORef ShowS) (IORef Int) (String -> IO ()) (String -> IO ())

newTerminal :: (String -> IO ()) -> (String -> IO ()) -> IO Terminal
newTerminal out err =
  do res <- newIORef (showString "")
     tmp <- newIORef 0
     return (MkTerminal res tmp out err)

withBuffering :: IO a -> IO a
withBuffering action = do
  mode <- hGetBuffering stderr
  -- By default stderr is unbuffered.  This is very slow, hence we explicitly
  -- enable line buffering.
  hSetBuffering stderr LineBuffering
  action `finally` hSetBuffering stderr mode

withHandleTerminal :: Handle -> Maybe Handle -> (Terminal -> IO a) -> IO a
withHandleTerminal outh merrh action = do
  let
    err =
      case merrh of
        Nothing -> const (return ())
        Just errh -> handle errh
  newTerminal (handle outh) err >>= action

withStdioTerminal :: (Terminal -> IO a) -> IO a
withStdioTerminal action = do
  isatty <- hIsTerminalDevice stderr
  if isatty then
    withBuffering (withHandleTerminal stdout (Just stderr) action)
   else
    withBuffering (withHandleTerminal stdout Nothing action)

withNullTerminal :: (Terminal -> IO a) -> IO a
withNullTerminal action =
  newTerminal (const (return ())) (const (return ())) >>= action

terminalOutput :: Terminal -> IO String
terminalOutput (MkTerminal res _ _ _) = fmap ($ "") (readIORef res)

handle :: Handle -> String -> IO ()
handle h s = do
  hPutStr h s
  hFlush h

putPart, putTemp, putLine :: Terminal -> String -> IO ()
putPart tm@(MkTerminal res _ out _) s =
  do putTemp tm ""
     force s
     out s
     modifyIORef res (. showString s)
  where
    force :: [a] -> IO ()
    force = evaluate . seqList

    seqList :: [a] -> ()
    seqList [] = ()
    seqList (x:xs) = x `seq` seqList xs

putLine tm s = putPart tm (s ++ "\n")

putTemp tm@(MkTerminal _ tmp _ err) s =
  do n <- readIORef tmp
     err $
       replicate n ' ' ++ replicate n '\b' ++
       s ++ [ '\b' | _ <- s ]
     writeIORef tmp (length s)

--------------------------------------------------------------------------
-- the end.
