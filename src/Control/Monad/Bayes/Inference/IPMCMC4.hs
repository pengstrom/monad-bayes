module Control.Monad.Bayes.Inference.IPMCMC4 where

import           Control.Monad.Bayes.Class
import           Control.Monad.Bayes.Weighted
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors

import           Control.Monad.Trans

import           Numeric.Log

import           Debug.Trace

import           Data.Either
import qualified Data.Vector                                as V

type Trace m a = (Either (CSMC m a) a, Log Double)

type Trajectory m a = [Trace m a]

type SMCState m a = (V.Vector (Trajectory m a), Log Double)

type CSMC m a = Coroutine (Await ()) (Weighted m) a

instance MonadSample m => MonadSample (Coroutine (Await ()) m) where
  random = lift random

instance MonadCond m => MonadCond (Coroutine (Await ()) m) where
  score w = lift (score w) >> await

instance MonadInfer m => MonadInfer (Coroutine (Await ()) m)

smc :: MonadSample m => Int -> CSMC m a -> m (SMCState m a)
smc n model = smcHelper (V.replicate n [(Left model, 1)], 1)

smcHelper :: MonadSample m => SMCState m a -> m (SMCState m a)
smcHelper state
  | finished state = return state
  | otherwise = stepPop state >>= smcHelper

step :: MonadSample m => CSMC m a -> m (Trace m a)
step c = do
  (cont, w) <- runWeighted $ resume c
  traceM $ "Step: w = " ++ show w
  let c' = either (\(Await f) -> Left $ f ()) Right cont
  return (c', w)

-- Assumes execution not completed (head of traces not right)
stepPop :: MonadSample m => SMCState m a -> m (SMCState m a)
stepPop (t, z) = do
  traceM $ "-------\nStepPop: length t = " ++ show (V.length t)
  let resample = logCategorical $ normalizedw t -- normalisation maybe not neccesary
  t' <-
    V.forM t $ \xs -> do
      a <- resample
      traceM $ "StepPop: a = " ++ show a
      let (Left c, w) = head $ t V.! a
      traceM $ "StepPop: w = " ++ show w
      (: xs) <$> step c
      --traceM $ "StepPop: w' * w = " ++ show (w' * w)
      --return $ (c', w * w') : xs
  traceM $ "StepPop: meanw t' = " ++ show (meanw t')
  let z' = z * meanw t'
  traceM $ "StepPop: z' = " ++ show z'
  return (t', z')

finished :: SMCState m a -> Bool
finished = isRight . fst . head . V.head . fst

normalizedw :: V.Vector (Trajectory m a) -> V.Vector (Log Double)
normalizedw t = V.map (\((_, w):_) -> w / s) t
  where
    s = sumw t

sumw :: V.Vector (Trajectory m a) -> Log Double
sumw = V.foldl (\acc ((_, w):_) -> acc + w) 0

meanw :: V.Vector (Trajectory m a) -> Log Double
meanw t = sumw t / fromIntegral (V.length t)
