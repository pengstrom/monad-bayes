module Control.Monad.Bayes.Inference.IPMCMC4
  ( smc
  , csmc
  , ipmcmc
  , IPMCMCResult
  , CSMC
  , SMCState
  , Trajectory
  , ipmcmcAvg
  , toPopulation
  , sampleAncestor
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
import           Data.Maybe
import qualified Data.Vector                                as V

type Trace m a = (Either (CSMC m a) a, Log Double)

-- List for O(1) cons and head
type Trajectory m a = [Trace m a]

-- Vector for O(1) lookup
type SMCState m a = (V.Vector (Trajectory m a), Log Double)

type CSMC m a = Coroutine (Await ()) (Weighted m) a

data IPMCMCState m a = IPMCMCState
  { numnodes :: Int
  , numcond :: Int
  , nummcmc :: Int
  --, condIdxs :: V.Vector Int
  , conds :: V.Vector (Trajectory m a)
  , result :: [V.Vector a]
  , smcnode :: m (SMCState m a)
  , csmcnode :: Trajectory m a -> m (SMCState m a)
  }

type IPMCMCResult a = [V.Vector a]

instance MonadSample m => MonadSample (Coroutine (Await ()) m) where
  random = lift random

instance MonadCond m => MonadCond (Coroutine (Await ()) m) where
  score w = lift (score w) >> await

instance MonadInfer m => MonadInfer (Coroutine (Await ()) m)

ipmcmc :: MonadSample m => Int -> Int -> Int -> CSMC m a -> m (IPMCMCResult a)
ipmcmc n m r model = do
  let p = m `div` 2
      --c = V.enumFromTo 1 p
      smcnode' = smc n model
      csmcnode' t = csmc t n model
  pnodes <- V.replicateM p smcnode'
  mnodes <- V.replicateM (m-p) smcnode'
  x' <- mcmcStep pnodes mnodes
  let res = V.map (fromRight . fst . head) x'
      state = IPMCMCState m p r x' [res]  smcnode' csmcnode'
  ipmcmcHelper state

ipmcmcHelper :: MonadSample m => IPMCMCState m a -> m (IPMCMCResult a)
ipmcmcHelper state
  | nummcmc state <= 0 = return $ result state
  | otherwise = do
      --traceM $ "MCMC iteration = " ++ show (nummcmc state)
      pnodes <- V.forM (conds state) $ csmcnode state
      mnodes <- V.replicateM (numnodes state - numcond state) $ smcnode state
      x' <- mcmcStep pnodes mnodes
      let res = V.map (fromRight . fst . head) x' : result state
          nextr = nummcmc state - 1
          state' = state {nummcmc = nextr, conds = x', result = res}
      ipmcmcHelper state'

mcmcStep :: MonadSample m => V.Vector (SMCState m a) -> V.Vector (SMCState m a) -> m (V.Vector (Trajectory m a))
mcmcStep pnodes mnodes = do
  let sumzm = V.sum $ V.map snd mnodes
      weightsm = V.map (flip (/) sumzm . snd) mnodes
  V.forM pnodes $ \(t,z) -> do
    let weights = V.cons (z / (z + sumzm)) $ V.map (\x -> recip $ recip x + z / (x * sumzm)) weightsm
    ksi <- logCategorical weights
    let trajectory = maybe t fst $ mnodes V.!? ksi
    b <- logCategorical $ normalizedw trajectory
    return $ trajectory V.! b

csmc :: MonadSample m => Trajectory m a -> Int -> CSMC m a -> m (SMCState m a)
csmc x' n model =
  csmcHelper (Just ([], reverse x')) (V.replicate n [(Left model, 1)], 1)

csmcHelper ::
     MonadSample m
  => Maybe (Trajectory m a, Trajectory m a)
  -> SMCState m a
  -> m (SMCState m a)
csmcHelper x state
  | finished state = return state
  | otherwise = stepPop (fmap fst nextx) state >>= csmcHelper nextx
  where
    nextx = do
      (current, rh:rest) <- x
      return (rh : current, rest)

smc :: MonadSample m => Int -> CSMC m a -> m (SMCState m a)
smc n model = csmcHelper Nothing (V.replicate n [(Left model, 1)], 1)

step :: MonadSample m => CSMC m a -> m (Trace m a)
step c = do
  (cont, w) <- runWeighted $ resume c
  --traceM $ "Step: w = " ++ show w
  let c' = either (\(Await f) -> Left $ f ()) Right cont
  return (c', w)

-- Assumes execution not completed (head of traces not right)
stepPop ::
     MonadSample m => Maybe (Trajectory m a) -> SMCState m a -> m (SMCState m a)
stepPop x' (t, z) = do
  let n = V.length t
  t' <-
    V.replicateM n $ do
      ancestor <- sampleAncestorWith x' t
      let Left c = fst $ head ancestor
      (c', w') <- step c
      let nextw = w' * sumw t / fromIntegral n
      --traceM $ "StepPop: w' = " ++ show w'
      --traceM $ "StepPop: w' * sumw t / n = " ++ show nextw
      return $ (c', nextw) : ancestor
  let z' = z * meanw t'
  return (t', z')

-- Only checks first trace for finished.
-- Assumes the same number of observations on every execution path
finished :: SMCState m a -> Bool
finished = isRight . fst . head . V.head . fst

normalizedw :: V.Vector (Trajectory m a) -> V.Vector (Log Double)
normalizedw = normalizedwWith Nothing

normalizedwWith ::
     Maybe (Trajectory m a)
  -> V.Vector (Trajectory m a)
  -> V.Vector (Log Double)
normalizedwWith x' t = maybe a (flip V.cons a . flip (/) s . snd . head) x'
  where
    s = sumw t + maybe 0 (snd . head) x'
    a = V.map (\((_, w):_) -> w / s) t

sumw :: V.Vector (Trajectory m a) -> Log Double
sumw = V.foldl (\acc ((_, w):_) -> acc + w) 0

meanw :: V.Vector (Trajectory m a) -> Log Double
meanw t = sumw t / fromIntegral (V.length t)

-- Assumes last trace is a value
toPopulation :: Monad m => m (SMCState m a) -> P.Population m a
toPopulation state = P.fromWeightedList weights
  where
    weights = fmap (V.toList . V.map f . fst) state
    f xs = (value xs, total xs)
    value ((Right x, _):_) = x
    value _                = error "Last trace not Right"
    total = product . map snd

sampleAncestorWith ::
     MonadSample m
  => Maybe (Trajectory m a)
  -> V.Vector (Trajectory m a)
  -> m (Trajectory m a)
sampleAncestorWith x' t = do
  let weights = normalizedwWith x' t
  b <- logCategorical weights
  return $ maybe t (`V.cons` t) x' V.! b

sampleAncestor ::
     MonadSample m => V.Vector (Trajectory m a) -> m (Trajectory m a)
sampleAncestor = sampleAncestorWith Nothing

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Coroutine not finished (not Right)"

ipmcmcAvg :: (a -> Double) -> IPMCMCResult a -> Double
ipmcmcAvg f res = flip (/) (r * p) $ foldl (\acc v -> acc + V.foldl (\acc' x -> acc' + f x) 0 v) 0 res
  where
    r = fromIntegral $ length res
    p = fromIntegral $ V.length $ head res
