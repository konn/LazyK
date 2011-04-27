{-# LANGUAGE QuasiQuotes, TypeSynonymInstances, ScopedTypeVariables, OverlappingInstances #-}
module Utils ( module SExp, module LazyK
             , swap, succL, even
             , ToExpr(..), FromExpr(..)) where
import SExp
import LazyK
import Data.List 
import Data.Word
import Control.Applicative
import Codec.Binary.UTF8.String
import Prelude hiding (even)
import Data.Maybe

fixL = [ski| S(K(SII))(S(S(KS)(S(KK)I))(K(SII))) |]
fixL2 = [scm| (lambda (f) (let ((sub (lambda (x) (f (x x))))) (sub sub))) |]

succL = [scm| (succ) |]

phi :: (b -> Bool) -> (b -> (a, b)) -> b -> Maybe (a, b)
phi p s x | p x       = Nothing
          | otherwise = Just $ s x

swap :: (a, b) -> (b, a)
swap = uncurry $ flip (,)

toBits :: Integral a => a -> [a]
toBits = unfoldr (phi (== 0) (swap . (`divMod` 2)))

fromBits :: Integral a => [a] -> a
fromBits = foldr ((. (*2)) . (+)) 0

even :: Expr
even = [scm| (fix (lambda (f n) (if (isZero n) (true) (not (f (pred n)))))) |]

class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr Word where
  toExpr =  foldr (\t s -> (if t == 1 then App succL else id) (App [scm| (double) |] s)) [ski| KI |] . toBits 

instance (ToExpr a, ToExpr b) => ToExpr (a, b) where
  toExpr (a, b) = App (App [scm| (cons) |] (toExpr a)) (toExpr b)

instance ToExpr a => ToExpr [a] where
  toExpr []     = [ski| KI |]
  toExpr (x:xs) = App (App [scm| (cons) |] (toExpr x)) (toExpr xs)

instance ToExpr String where
  toExpr = toExpr . (++[256 :: Word]) . map (toEnum . fromEnum) . encodeString

instance ToExpr Bool where
  toExpr True  = [scm| (lambda (t f) (t)) |]
  toExpr False = [scm| (lambda (t f) (f)) |]

class FromExpr a where
  fromExpr :: Expr -> Maybe a

instance FromExpr Word where
  fromExpr expr = sub $ eval (App (App expr $ X 1) $ X 0)
    where
      sub e@(App (X 1) a) = succ <$> sub (eval a)
      sub (X 0)           = pure 0
      sub _               = Nothing

instance FromExpr Bool where
  fromExpr ex =
    case eval (App (App ex (X 0)) (X 1)) of
      X 0 -> Just True
      X 1 -> Just False
      _   -> Nothing

instance (FromExpr a, FromExpr b) => FromExpr (a, b) where
  fromExpr ex =
    let (e1, e2) = (App [scm| (car) |] ex, App [scm| (cdr) |] ex)
    in (,) <$> fromExpr e1 <*> fromExpr e2
 
instance FromExpr a => FromExpr [a] where 
  fromExpr ex = (:) <$> fromExpr (App [scm| (car) |] ex)
                    <*> pure (fromJust $ fromExpr (App [scm| (cdr) |] ex))
           --  <|> if fromExpr ex == Just (0::Word) then Just [] else Nothing

instance FromExpr String where
  fromExpr ex = decodeString . map (toEnum . fromEnum) . takeWhile (< (256::Word))<$> fromExpr ex
