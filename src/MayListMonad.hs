{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module MayListMonad where

import Prelude hiding (zipWith)
import Data.Foldable
import Test.QuickCheck.Arbitrary
import qualified GHC.Exts as Exts
import qualified Test.Laws.IsMonad as IsMonad


data List a
  = a :> List a
  | Nil
  deriving (Eq, Ord, Foldable)

instance Exts.IsList (List a) where
  type Item (List a) = a
  fromList = foldr (:>) Nil
  toList = toList

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Exts.fromList <$> arbitrary

instance Show a => Show (List a) where
  show = show . toList

instance Functor List where
  fmap _ Nil       = Nil
  fmap f (x :> xs) = f x :> fmap f xs

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ Nil _ = Nil
zipWith _ _ Nil = Nil
zipWith f (x :> xs) (y :> ys) = f x y :> zipWith f xs ys

instance Applicative List where
  pure x = x :> pure x
  (<*>) = zipWith ($)

instance Monad List where
  m >>= f = join' $ fmap f m
    where
      join' = diag 0

      diag _ Nil = Nil
      diag n (xs :> xss) = case diagStep n xs of
        Nothing -> Nil
        Just x  -> x :> diag (n + 1) xss

      diagStep _ Nil       = Nothing
      diagStep 0 (x :> _)  = Just x
      diagStep n (_ :> xs) = diagStep (n - 1) xs

monadLawCounterExample :: Bool
monadLawCounterExample = IsMonad.propAssociative l
  where
    l :: List (List (List ()))
    l = [[[()]],[[],[(),()]]]
