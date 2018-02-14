module Control.Monad.Bayes.Inference.IPMCMC4
  ( smc
  , CSMC
  , SMCState
  , toPop
  ) where

import           Control.Monad.Bayes.Class
import qualified Control.Monad.Bayes.Population             as P
import           Control.Monad.Bayes.Weighted
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors

import           Control.Monad.Trans

import           Numeric.Log

import           Debug.Trace

import           Data.Either
import qualified Data.Vector                                as V

type Trace m a = (Either (CSMC m a) a, Log Double)

-- List for O(1) cons and head
type Trajectory m a = [Trace m a]

-- Vector for O(1) lookup and update
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
  let c' = either (\(Await f) -> Left $ f ()) Right cont
  return (c', w)

-- Assumes execution not completed (head of traces not right)
stepPop :: MonadSample m => SMCState m a -> m (SMCState m a)
stepPop (t, z)
  --traceM $ "--- StepPop, step = " ++ show (length $ V.head t)
  --traceM $ "z = " ++ show z
  --traceM $ "length t = " ++ show (V.length t)
  -- TODO: Possibly replace with mutation in ST
 = do
  t' <-
    V.replicateM (V.length t) $
    --V.forM (V.zip t (V.enumFromTo 0 (V.length t - 1))) $ \(_, i)
      --traceM $ "------ Loop = " ++ show i
     -- -> do
     do
      ancestor <- sampleAncestor t
      --traceM $ "a = " ++ show a
      let (Left c, _) = head ancestor
      --traceM $ "w = " ++ show w
      (: ancestor) <$> step c
  --traceM $ "------"
  --traceM $ "meanw t' = " ++ show (meanw t')
  let z' = z * meanw t'
  --traceM $ "z' = " ++ show z'
  --traceM $ "---"
  return (t', z')

-- Only checks first trace for finished.
-- Assumes the same number of observations on every execution path
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

-- Assumes last trace is a value
toPop :: Monad m => m (SMCState m a) -> P.Population m a
toPop state = P.fromWeightedList weights
  where
    weights = fmap (V.toList . V.map f . fst) state
    f xs = (value xs, total xs)
    value ((Right x, _):_) = x
    value _                = error "Last trace not Right"
    total = product . map snd

sampleAncestor ::
     MonadSample m => V.Vector (Trajectory m a) -> m (Trajectory m a)
sampleAncestor t = do
  b <- logCategorical $ normalizedw t
  return $ t V.! b
