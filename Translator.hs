module Main where
import SExp

main = do
  src <- getContents
  ans <- evalLispDefault src
  case ans of
    Nothing -> putStrLn "Conversion failed."
    Just ex -> print ex
