{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Data.Proxy
import           MayListMonad
import qualified Test.Laws.IsMonad as IsMonad
import           Test.QuickCheck

main :: IO ()
main = do
  listMonadLawCheck (Proxy :: Proxy ())
  listMonadLawCheck (Proxy :: Proxy Bool)
  listMonadLawCheck (Proxy :: Proxy Int)


listMonadLawCheck :: forall a. (Eq a, Arbitrary a, Show a) => Proxy a -> IO ()
listMonadLawCheck _ = do
  quickCheck (IsMonad.propLeftUnitary @List @a)
  quickCheck (IsMonad.propRightUnitary @List @a)
  quickCheck (IsMonad.propAssociative @List @a)
