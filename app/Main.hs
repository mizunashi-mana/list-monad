{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Data.Proxy
import           MayListMonad
import qualified Test.Laws.IsMonad as IsMonad
import           Test.QuickCheck

main :: IO ()
main = do
  let listMonadLawCheck :: (Eq a, Arbitrary a, Show a) => Proxy a -> IO ()
      listMonadLawCheck = monadLawCheck (Proxy :: Proxy [])
  listMonadLawCheck (Proxy :: Proxy ())
  listMonadLawCheck (Proxy :: Proxy Bool)
  listMonadLawCheck (Proxy :: Proxy Int)

  let ilistMonadLawCheck :: (Eq a, Arbitrary a, Show a) => Proxy a -> IO ()
      ilistMonadLawCheck = monadLawCheck (Proxy :: Proxy List)
  ilistMonadLawCheck (Proxy :: Proxy ())
  ilistMonadLawCheck (Proxy :: Proxy Bool)
  ilistMonadLawCheck (Proxy :: Proxy Int)


monadLawCheck :: forall m a.
  ( Eq (m a), Arbitrary (m a), Show (m a)
  , Eq (m a), Arbitrary (m (m (m a))), Show (m (m (m a)))
  , Monad m
  ) => Proxy m -> Proxy a -> IO ()
monadLawCheck _ _ = do
  quickCheck (IsMonad.propLeftUnitary @m @a)
  quickCheck (IsMonad.propRightUnitary @m @a)
  quickCheck (IsMonad.propAssociative @m @a)
