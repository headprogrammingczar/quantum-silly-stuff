{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
import QIO.Heap
import QIO.QArith
import QIO.QExamples
import QIO.QIORandom
import QIO.Qdata
import QIO.Qft
import QIO.Qio
import QIO.QioClass
import QIO.QioSyn
import QIO.Shor
import QIO.Vec
import QIO.VecEq
import Control.Monad
import Control.Monad.State
import Data.List

deriving instance Eq a => Eq (Vec RR a)
deriving instance Ord a => Ord (Vec RR a)

deriving instance Eq a => Eq (Prob a)
deriving instance Ord a => Ord (Prob a)

deriving instance Show StateQ

main = do
  print pureQFTTest

-- looks nicer than evalWith @Prob
probEvalWith :: QIO a -> State StateQ (Prob a)
probEvalWith = evalWith

-- when measured, bit1 and bit2 are independent and 50-50 chance of being False or True
-- but they aren't measured, instead you can see how the final quantum state is different
-- for different initial conditions
-- fun fact: if the qubits are measured, the final quantum state becomes worthless
pureQFTTest :: StateQ
pureQFTTest = do
  let s = probEvalWith $ do
            bit1 <- mkQbit True
            bit2 <- mkQbit True
            applyU (qft [bit1, bit2])
            pure ()
            -- b1 <- measQbit bit1
            -- b2 <- measQbit bit2
            -- pure (b1, b2)
  let (action, finalstate) = runState s initialStateQ
  finalstate

-- when measured, bit1 and bit2 are independent and 50-50 chance of being False or True
-- a second quantum fourier transform returns them to being always their initial values
probTest = do
  factors <- forM [1..100] $ \n -> do
    let v = sim $ do
              bit1 <- mkQbit False
              bit2 <- mkQbit True
              applyU (qft [bit1, bit2])
              -- applyU (qft [bit1, bit2])
              b1 <- measQbit bit1
              b2 <- measQbit bit2
              pure (b1, b2)
    pure v
  let factors' = group (sort factors)
  let factors'' = map (\fs -> (head fs, length fs)) factors'
  print factors''

