-- Check that the terminal output works correctly.
{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
import Test.QuickCheck
import Test.QuickCheck.Text
import System.Process
import System.IO
import Control.Exception
import GHC.Generics
import Control.DeepSeq

data Command =
    PutPart String
  | PutLine String
  | PutTemp String
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary Command where
  arbitrary =
    oneof [
      PutPart <$> line,
      PutLine <$> line,
      PutTemp <$> line]
    where
      line = filter (/= '\n') <$> arbitrary
  shrink = genericShrink

exec :: Terminal -> Command -> IO ()
exec tm (PutPart xs) = putPart tm xs
exec tm (PutLine xs) = putLine tm xs
exec tm (PutTemp xs) = putTemp tm xs

eval :: [Command] -> String
eval = concatMap eval1
  where
    eval1 (PutPart xs) = xs
    eval1 (PutLine xs) = xs ++ "\n"
    -- PutTemp only has an effect on stderr
    eval1 (PutTemp xs) = ""

-- Evaluate the result of printing a given string, taking backspace
-- characters into account.
format :: String -> String
format xs = format1 [] [] xs
  where
    -- Arguments: text before the cursor (in reverse order),
    -- text after the cursor, text to print
    format1 xs ys [] = line xs ys
    -- \n emits a new line
    format1 xs ys ('\n':zs) = line xs ys ++ "\n" ++ format1 [] [] zs
    -- \b moves the cursor to the left
    format1 (x:xs) ys ('\b':zs) = format1 xs (x:xs) zs
    -- beginning of line: \b ignored
    format1 [] ys ('\b':zs) = format1 [] ys zs
    -- Normal printing puts the character before the cursor,
    -- and overwrites the next character after the cursor
    format1 xs ys (z:zs) = format1 (z:xs) (drop 1 ys) zs

    line xs ys = reverse xs ++ ys

-- Check that the terminal satisfies the following properties:
-- * The text written to stdout matches what's returned by terminalOutput
-- * The output agrees with the model implementation 'eval'
-- * Anything written to stderr (presumably by putTemp) is erased
prop_terminal :: [Command] -> Property
prop_terminal cmds =
  withMaxSuccess 1000 $ ioProperty $
  withPipe $ \stdout_read stdout_write ->
  withPipe $ \stderr_read stderr_write -> do
    out <- withHandleTerminal stdout_write (Just stderr_write) $ \tm -> do
      mapM_ (exec tm) (cmds ++ [PutPart ""])
      terminalOutput tm
    stdout <- stdout_read
    stderr <- stderr_read
    return $ conjoin [
        counterexample "output == terminalOutput" $ stdout === out,
        counterexample "output == model" $ out === eval cmds,
        counterexample "putTemp erased" $ all (== ' ') (format stderr) ]
  where
    withPipe :: (IO String -> Handle -> IO a) -> IO a
    withPipe action = do
      (readh, writeh) <- createPipe
      hSetEncoding readh utf8
      hSetEncoding writeh utf8
      let
        read = do
          hClose writeh
          contents <- hGetContents readh
          return $!! contents
      action read writeh `finally` do
        hClose readh
        hClose writeh

return []
main = do True <- $quickCheckAll; return ()
