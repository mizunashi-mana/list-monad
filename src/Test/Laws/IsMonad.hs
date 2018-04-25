module Test.Laws.IsMonad where

import           Control.Monad


propLeftUnitary :: (Eq (m a), Monad m) => m a -> Bool
propLeftUnitary m = join (return m) == m

propRightUnitary :: (Eq (m a), Monad m) => m a -> Bool
propRightUnitary m = join (fmap return m) == m

propAssociative :: (Eq (m a), Monad m) => m (m (m a)) -> Bool
propAssociative m = join (join m) == join (fmap join m)
