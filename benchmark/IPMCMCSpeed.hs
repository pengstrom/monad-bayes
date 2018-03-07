module Main where

import           Control.Monad.Bayes.Class
import           Control.Monad.Bayes.Enumerator
import           Control.Monad.Bayes.Inference.IPMCMC4
import           Control.Monad.Bayes.Inference.SMC
import           Control.Monad.Bayes.Population
import           Control.Monad.Bayes.Sampler
import           Control.Monad.Bayes.Weighted

--import           NonlinearSSM
import           Control.Exception
import           System.CPUTime
import           Text.Printf

two35 :: MonadInfer m => m Double
two35 = do
  x <- random
  score 0.2
  y <- random
  score 0.3
  z <- random
  score 0.5
  return (x + y + z)

dice :: MonadSample m => m Int
dice = uniformD [1 .. 6]

dices :: MonadInfer m => m Int
dices = do
  x <- dice
  y <- dice
  score $
    if x + y > 5
      then 0.7
      else 0.5
  return $ x * y

truth = enumerate dices

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10 ^ 12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v

main = do
  let samples = 100
      nodes = 32
      iters = 10
  putStrLn "iPMCMC"
  res <- time $ sampleIO $ ipmcmc samples nodes iters dices
  print $ length res
  {-
  let mysmcs = [time $ sampleIO $ smc (10 * x) (ssm ys) | x <- [20 .. 30]]
      bismcs =
        [ time $ sampleIO $ runPopulation $ smcMultinomial 50 (10 * x) (ssm ys)
        | x <- [1 .. 20]
        ]
  --sequence $ mysmcs
  --sequence $ bismcs
  --time $ sampleIO $ smc 300 $ ssm ys
  time $ sampleIO $ runPopulation $ smcMultinomial 50 300 $ ssm ys
  -}
