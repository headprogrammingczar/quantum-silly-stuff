{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
import QIO.QExamples
import QIO.QArith
import QIO.Shor
import QIO.Qio
import QIO.QioSyn
import QIO.QIORandom
import QIO.Vec
import Control.Monad
import Data.List

deriving instance Eq a => Eq (Vec RR a)
deriving instance Ord a => Ord (Vec RR a)

deriving instance Eq a => Eq (Prob a)
deriving instance Ord a => Ord (Prob a)

main = do
  factors <- forM [1..100] $ \n -> do
    let v = sim $ do
              bit1 <- mkQbit False
              bit2 <- mkQbit False
              applyU (hadamards [bit1])
              applyU (cnot bit1 bit2)
              b1 <- measQbit bit1
              b2 <- measQbit bit1
              pure (b1, b2)
    pure v
  let factors' = group (sort factors)
  let factors'' = map (\fs -> (head fs, length fs)) factors'
  print factors''
