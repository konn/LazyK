{-# LANGUAGE OverlappingInstances #-}
module Main (module LazyK, module SExp
            ,runLazyK, main) where
import LazyK
import SExp
import Utils
import Control.Applicative
import Text.Parsec
import System.Environment
import System.IO

runLazyK :: String -> IO ()
runLazyK src = do
  input <- toExpr <$> getContents
  let ans = runParser (spaces *> skiP <* eof) () "" src
  case ans of
    Left err  -> print err
    Right ans -> do
      let !mout = fromExpr $ App ans input
      case mout of
        Nothing -> putStrLn "ERR: Nothing"
        Just a  -> mapM_ (\c -> putChar c >> hFlush stdout) a

main = do
  args <- getArgs
  case args of
    fname:_ -> do
      src <- readFile fname
      runLazyK src
    _ -> putStrLn "not enough arguments"
