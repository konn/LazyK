{-# LANGUAGE PatternGuards, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module LazyK ( ski, skiP, eval, translate, symbol, lexeme
             , parens, Name(..), Lisp(..), normalize, Expr(..)) where
import Data.Maybe
import Control.Applicative hiding ((<|>))
import Control.Monad
import Control.Monad.Reader
import Data.String
import Numeric
import Data.Char
import Data.Data
import Text.Parsec
import Text.Parsec.String
import Data.Generics
import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.Quote
import Control.Arrow

ski :: QuasiQuoter
ski = QuasiQuoter { quoteExp = parseSKIExp
                  , quotePat = parseSKIPat
                  , quoteDec = undefined
                  , quoteType = undefined
                  }

parseSKI :: Monad m => (String, (Int, Int)) -> String -> m Expr
parseSKI (file, (line, col)) s =
  case runParser p () "" s of
    Left err -> fail $ show err
    Right ex -> return ex
  where
    p = do
      pos <- getPosition
      setPosition $ flip setSourceName file $
                    flip setSourceLine line $
                    flip setSourceColumn col $
                    pos
      spaces *> skiP <* eof

getLoc :: Q (String, (Int, Int))
getLoc = (loc_filename &&& loc_start) <$> location

parseSKIExp :: String -> ExpQ
parseSKIExp src = do
  expr <- flip parseSKI src =<< getLoc
  dataToExpQ (const Nothing) expr

parseSKIPat :: String -> PatQ
parseSKIPat src = do
  expr <- flip parseSKI src =<< getLoc
  dataToPatQ (const Nothing) expr

symbol :: Monad m => String -> ParsecT String u m String
symbol = lexeme . string

lexeme :: Monad m => ParsecT String u m a -> ParsecT String u m a
lexeme p = p <* skipMany space

parens :: Monad m => ParsecT String u m a -> ParsecT String u m a
parens p = symbol "(" *> p <* symbol ")"

skiP :: Parser Expr
skiP = term `chainl1` (App <$ spaces)

term :: Parser Expr
term = combinator
   <|> parens skiP

combinator :: Parser Expr
combinator = S <$ lexeme (oneOf "sS")
         <|> K <$ lexeme (oneOf "kK")
         <|> I <$ lexeme (oneOf "iI")

instance IsString Name where
  fromString = flip Name 0

instance Show Name where
  showsPrec _ (Name base col) = showString base . (if col == 0 then id else shows col)

data Name = Name { unName :: String, identifier :: Int }
          deriving (Eq, Ord, Typeable, Data)

data Lisp = Var Name
          | AppL Lisp Lisp
          | Abs Name Lisp
          | Prim Expr
            deriving (Eq, Ord, Data, Typeable)

data Expr = S
          | K
          | I
          | X Int
          | App Expr Expr
            deriving (Eq, Ord, Data, Typeable)

occurs :: Name -> Lisp -> Bool
occurs n (Var m)      = n == m
occurs n (AppL e1 e2) = n `occurs` e1 || n `occurs` e2
occurs n (Abs ix e)   = n `occurs` e
occurs n (Prim _)     = False

normalize :: Lisp -> Lisp
normalize (AppL e1 e2)   = AppL (normalize e1) (normalize e2)
normalize (Abs v (Abs u (Abs t (AppL (AppL (Var v') (Var t')) (AppL (Var u') (Var t''))))))
    | v == v' && u == u' && t == t' && t == t'' = Prim S
normalize (Abs v (Abs u (Var v'))) | v == v' = Prim K
normalize (Abs v (Var v')) | v == v' = Prim I
normalize (Abs v e)
    | not (v `occurs` e) = AppL (Prim K) (normalize e)
    | AppL e1 e2 <- e    = AppL (AppL (Prim S) (normalize (Abs v e1))) (normalize (Abs v e2))
    | Var u <- e, u == v = Prim I
    | otherwise          = normalize (Abs v (normalize e))
normalize l              = l

translate :: Lisp -> Maybe Expr
translate = trans . normalize
  where
    trans (AppL e1 e2) = App <$> trans e1 <*> trans e2
    trans (Prim e)     = pure e
    trans _            = Nothing

isAbs :: Lisp -> Bool
isAbs (Abs _ _) = True
isAbs _         = False

showsLisp :: Int -> Lisp -> ShowS
showsLisp d (Var  id)   = shows id
showsLisp d (AppL e e') = showParen (d > 10) $ showsLisp 10 e . showChar ' ' . showsLisp (11) e'
showsLisp d (Abs v e)   = showParen True $ showString "lambda" . showChar ' ' . showParen True (shows v) . showChar ' ' . showParen (not (isAbs e)) (showsLisp d e)
showsLisp d (Prim a)    = showParen (d > 10) $ showsPrec d a

eval :: Expr -> Expr
eval d@(X _)                   = d
eval (App (App (App S x) y) z) = eval $ App (App x z) (App y z)
eval (App I x)                 = eval x
eval (App (App K a) b)         = eval a
eval (App a b)                 =
    let a' = eval a
    in if a' /= a
       then eval (App a' b)
       else App a b
eval s                         = s

instance Show Expr where
  showsPrec d S          = showString "S"
  showsPrec d K          = showString "K"
  showsPrec d I          = showString "I"
  showsPrec d (X i)      = showInt i
  showsPrec d (App e e') = showParen (d > 10) $ showsPrec 10 e . (showsPrec 11 e')

instance Show Lisp where
  showsPrec = showsLisp
