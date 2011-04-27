{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module SExp (lisp, scm, evalLispWithEnv, evalLispDefault) where
import Text.Parsec
import Text.Parsec.String hiding (Parser)
import Text.Parsec.Combinator
import Control.Applicative hiding (many, (<|>), empty)
import Data.Maybe
import Data.Map hiding (map)
import Control.Monad
import Prelude hiding (lookup)
import Control.Arrow hiding (app)
import Debug.Trace
import Language.Haskell.TH.Quote
import Data.Char
import Language.Haskell.TH hiding (Name, letE)

import LazyK

type Parser = Parsec String (Map String Int)

ident :: Bool -> Parser Name
ident incr = do
  base <- (:) <$> letter <*> many alphaNum
  col <- maybe 0 (if incr then succ else id) . lookup base <$> getState
  updateState (insert base col)
  return $ Name base col

lisp = val <|> try lambda <|> try letE <|> try cons <|> app

lambda :: Parser Lisp
lambda = parens $ do
  symbol "lambda" <* notFollowedBy alphaNum
  vars <- parens $ ident True `sepBy` spaces
  code <- lisp
  updateState (flip (foldr (uncurry insert)) (map (unName &&& pred . identifier) vars))
  return $ foldr Abs code vars

app :: Parser Lisp
app = parens $ lisp `chainl1` (AppL <$ spaces)

reserved :: String -> Parser String
reserved name = do
  num <- fromMaybe 0 . lookup name <$> getState
  when (num > 0) $ fail "reserved value is overwritten"
  string name <* notFollowedBy alphaNum

val :: Parser Lisp
val = try (Prim S <$ reserved "S")
  <|> try (Prim K <$ reserved "K")
  <|> try (Prim I <$ reserved "I")
  <|> Var <$> ident False

cons :: Parser Lisp
cons = parens $ do
  ls <- lisp `sepEndBy` spaces
  symbol "."
  l2 <- lisp
  return $ foldr (AppL . (AppL (Var "cons"))) l2 ls

defs :: Parser [(Name, Lisp)]
defs = many $ parens ((,) <$> ident True <* spaces <*> lisp)

letE :: Parser Lisp
letE = parens $ do
  symbol "let" <* notFollowedBy alphaNum
  spaces
  dic <- parens defs
  spaces
  body <- lisp
  let (names, decs) = unzip dic
      lam = foldr Abs body names
      app = foldl AppL lam decs
  return app

env :: Parser (Lisp -> Lisp)
env = do
  table <- defs
  return $ \body -> foldr (\(name, dec) b -> AppL (Abs name b) dec) body table

defDics :: Map String Int
defDics = fromList [("S", 0), ("K", 0), ("I", 0)]

enclose :: Name -> Lisp -> Lisp -> Lisp
enclose name dec body = AppL (Abs name dec) body

evalLispWithEnv :: String -> String -> Maybe Expr
evalLispWithEnv envSrc body =
  let (run, st) = either (error.show) id $ runParser ((,) <$> env <*> getState) empty "env" envSrc
      ans  = either (const Nothing) (Just  . run) $ runParser (spaces *> lisp <* eof) st "src" body
      ans' = flip (foldr (uncurry enclose))
               [(Name "S" 0, Prim S), (Name "K" 0, Prim K), (Name "I" 0, Prim I)] <$> ans
  in eval <$> (translate =<< ans)

evalLispDefault :: String -> IO (Maybe Expr)
evalLispDefault src = do
  env <- readFile "env.scm"
  return $ evalLispWithEnv env src

scm :: QuasiQuoter
scm = QuasiQuoter { quoteExp = parseScmExp
                  , quotePat = parseScmPat
                  , quoteDec = undefined
                  , quoteType = undefined
                  }

parseScmExp :: String -> ExpQ
parseScmExp src = do
  ans <- fromJust <$> runIO (evalLispDefault $ dropWhile isSpace src)
  dataToExpQ (const Nothing) ans

parseScmPat :: String -> PatQ
parseScmPat src = do
  ans <- fromJust <$> runIO (evalLispDefault $ dropWhile isSpace src)
  dataToPatQ (const Nothing) ans
