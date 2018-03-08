module Control.Monad.Bayes.Inference.IPMCMC4
  ( CSMC
  , Trace
  , Trajectory
  , IPMCMCResult
  , SMCState
  , ipmcmc
  , smc
  , csmc
  , ipmcmcAvg
  , toPopulation
  ) where

import           Control.Monad.Bayes.Class
import qualified Control.Monad.Bayes.Population             as P
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors

import           Control.Monad.Trans

import           Numeric.Log

import           Data.Either
import qualified Data.Vector                                as V

import Control.Monad.Parallel as MP

import Debug.Trace
import Data.Time.Clock.System

import Control.Monad (when)

-- | Snapshot of program execution in time.
-- Pair of rest of program (or result) and weight at this time.
type Trace m a =
  ( Either (CSMC m a) a
  , Log Double)

traceCont :: Trace m a -> Either (CSMC m a) a
traceCont = fst

traceWeight :: Trace m a -> Log Double
traceWeight = snd

-- | The particle trajectory
type Trajectory m a = [Trace m a]

-- | SMC state.
-- Pair of all trajectories and estimated marginal likelihood.
type SMCState m a = 
  ( V.Vector (Trajectory m a)
  , Log Double) 

-- | Suspendable execution yielding the score.
type CSMC m a = Coroutine (Yield (Log Double))  m a

-- | Internals of iPMCMC
data IPMCMCState m a = IPMCMCState
  { numnodes :: Int -- ^ Number of total nodes M
  , numcond :: Int -- ^ Number of conditional nodes P
  , nummcmc :: Int -- ^ Number of (remaining) MCMC iterations
  , conds :: V.Vector (Trajectory m a) -- ^ Collected conditional trajectories of last iteration
  , result :: IPMCMCResult a -- ^ Accumulated conditional trajectories
  , smcnode :: m (SMCState m a) -- ^ Alias for running unconditional SMC on model
  , csmcnode :: Trajectory m a -> m (SMCState m a) -- ^ Alias for running conditional SMC on model
  }

-- | All conditional trajectories from all iterations
type IPMCMCResult a = [V.Vector a]

instance MonadSample m => MonadSample (Coroutine (Yield (Log Double)) m) where
  random = lift random

instance Monad m => MonadCond (Coroutine (Yield (Log Double)) m) where
  score = yield

instance MonadSample m => MonadInfer (Coroutine (Yield (Log Double)) m)

-- | Interacting Particle Markov Chain Monte Carlo sampler.
-- Assumes all execution paths of the model contains the same number of scorings.
-- Results are (at least explicitly) unweighted.
-- The value M/2 is used for the number of conditional nodes.
-- No Rao-Blackwell normalization.
ipmcmc :: (MonadParallel m, MonadSample m, MonadIO m)
  => Int -- ^ Number of SMC particles per node N
  -> Int -- ^ Number of nodes M
  -> Int -- ^ Number of MCMC iterations
  -> CSMC m a -- ^ Model
  -> m (IPMCMCResult a)
ipmcmc n m r model = do
  let p = m `div` 2
      smcnode' = smc n model
      csmcnode' t = csmc t n model
  logtime
  pnodes <- V.fromList <$> MP.replicateM p     smcnode'
  mnodes <- V.fromList <$> MP.replicateM (m-p) smcnode'
  x' <- mcmcStep pnodes mnodes -- 0 []
  let res = V.map (fromRight . fst . head) x'
      state = IPMCMCState m p r x' [res]  smcnode' csmcnode'
  ipmcmcHelper state

ipmcmcHelper :: (MonadIO m, MonadParallel m, MonadSample m) => IPMCMCState m a -> m (IPMCMCResult a)
ipmcmcHelper state
    | nummcmc state <= 0 = return $ result state
    | otherwise = do
      let p = numcond state
          nonp = numnodes state - p
      logtime
      pnodes <- V.fromList <$> MP.forM (V.toList $ conds state) (csmcnode state)
      mnodes <- V.fromList <$> MP.replicateM nonp (smcnode state)
      traceM $ "MCMC step = " ++ show (nummcmc state)
      x' <- mcmcStep pnodes mnodes -- 0 []
      let res = V.map (fromRight . fst . head) x' : result state
          nextr = nummcmc state - 1
          state' = state {nummcmc = nextr, conds = x', result = res}
      ipmcmcHelper state'

mcmcStep :: MonadSample m => V.Vector (SMCState m a) -> V.Vector (SMCState m a) {- -> Int -> [Trajectory m a] -} -> m (V.Vector (Trajectory m a))
mcmcStep pnodes mnodes -- idx acc
    {-
  | idx == V.length pnodes = return $ V.fromList $ reverse acc
  | otherwise = do
    let ct = pnodes V.! idx
    (node, mnodes') <- sampleNode ct mnodes
    x <- sampleCond node
    let idx' = idx + 1
        acc' = x : acc
    mcmcStep pnodes mnodes' idx' acc'
      -}
      = do
    let sumzm = V.sum $ V.map snd mnodes
        weightsm = V.map (flip (/) sumzm . snd) mnodes
    V.forM pnodes $ \(t,z) -> do
      let weights = V.snoc (V.map (\x -> recip $ recip x + z / (x * sumzm)) weightsm) (z / (z + sumzm))
      ksi <- logCategorical weights
      let trajectory = maybe t fst $ mnodes V.!? ksi
      b <- logCategorical $ normalizedw trajectory
      return $ trajectory V.! b

sampleCond :: MonadSample m => SMCState m a -> m (Trajectory m a)
sampleCond (t, _) = do
  let ws = fmap (snd . head . tail) t
      sumWs = V.sum ws
      normWs = fmap (/sumWs) ws
  b <- logCategorical normWs
  return $ t V.! b

sampleNode :: MonadSample m => SMCState m a -> V.Vector (SMCState m a) -> m (SMCState m a, V.Vector (SMCState m a))
sampleNode ct mnodes = do
  let ts = V.cons ct mnodes
      zs = fmap snd ts
      sumz = V.sum zs
      normZs = fmap (/sumz) zs
  ksi <- logCategorical normZs
  let isCond = ksi == 0
      node = ts V.! ksi
      mnodes' = if isCond then mnodes else mnodes V.// [(ksi-1,node)]
  return (node, mnodes')

-- | Conditional Sequential Markov Chain sampler.
-- Uses multinomial resampling.
-- Retains all trajectories unlike 'smcMultinomial' and the like.
-- Conditional on given trajectory
csmc :: MonadSample m
  => Trajectory m a -- ^ Conditional trajectory
  -> Int -- ^ Number of particles N
  -> CSMC m a -- ^ Model
  -> m (SMCState m a)
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

-- | Sequential Markov Chain sampler.
-- Uses multinomial resampling.
-- Retains all trajectories unlike 'smcMultinomial' and the like.
smc :: MonadSample m
  => Int -- ^ Number of particles N
  -> CSMC m a -- ^ Model
  -> m (SMCState m a)
smc n model = csmcHelper Nothing (V.replicate n [(Left model, 1)], 1)

step :: MonadSample m => CSMC m a -> m (Trace m a)
step c = do
  cont <- resume c
  return $ either (\(Yield w c') -> (Left c', w)) (\x -> (Right x, 1)) cont

-- Assumes execution not completed (head of traces not right).
stepPop ::
     MonadSample m => Maybe (Trajectory m a) -> SMCState m a -> m (SMCState m a)
stepPop x' (t, z) = do
  let n = V.length t
      w = sumw t / fromIntegral n
  t' <-
    V.forM (V.enumFromTo 0 (n-1)) $ \i -> do
      let sampler = if nonzero t then sampleAncestorWith x' t else return (t V.! i)
      ancestor <- sampler
      let (Left c, _) = head ancestor
      (c', w') <- step c
      let nextw = w' * w
      return $ (c', nextw) : ancestor
  let z' = z * meanw t'
  return (t', z')

nonzero :: V.Vector (Trajectory m a) -> Bool
nonzero = not . V.all (0 ==) . getWeights
  where getWeights = V.map (traceWeight . head)

-- Only checks first trace for finished.
-- Assumes the same number of observations on every execution path
finished :: SMCState m a -> Bool
finished = isRight . fst . head . V.head . fst

meanw :: V.Vector (Trajectory m a) -> Log Double
meanw t = sumw t / fromIntegral (V.length t)

-- | Convert the result from 'ipmcmc' to a 'Population'
toPopulation :: Monad m => m (SMCState m a) -> P.Population m a
toPopulation state = P.fromWeightedList weights
  where
    weights = fmap (V.toList . V.map f . fst) state
    f xs = (value xs, total xs)
    value ((Right x, _):_) = x
    value _                = error "Last trace not Right"
    total = snd . head . tail

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

normalizedw :: V.Vector (Trajectory m a) -> V.Vector (Log Double)
normalizedw = normalizedwWith Nothing

normalizedwWith ::
     Maybe (Trajectory m a)
  -> V.Vector (Trajectory m a)
  -> V.Vector (Log Double)
normalizedwWith x' t = maybe a (flip V.cons a . flip (/) s . getWeight) x'
  where
    getWeight = traceWeight . head
    s = sumw t + maybe 0 getWeight x'
    a = V.map (flip (/) s . getWeight) t

sumw :: V.Vector (Trajectory m a) -> Log Double
sumw = V.foldl (\acc ((_, w):_) -> acc + w) 0

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Coroutine not finished (not Right)"

-- | Estimate the expected value of a function 'f' using the result of 'ipmcmc'.
ipmcmcAvg :: (a -> Double) -> IPMCMCResult a -> Double
ipmcmcAvg f res = flip (/) (r * p) $ foldl (\acc v -> acc + V.foldl (\acc' x -> acc' + f x) 0 v) 0 res
  where
    r = fromIntegral $ length res
    p = fromIntegral $ V.length $ head res

logtime :: MonadIO m => m ()
logtime = do
  (MkSystemTime secs msecs) <- liftIO getSystemTime
  return ()
  --liftIO $ putStrLn $ show secs ++ "." ++ show msecs
