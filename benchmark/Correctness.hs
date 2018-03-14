{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Bayes.Class
import           Control.Monad.Bayes.Enumerator
import           Control.Monad.Bayes.Inference.IPMCMC4
import           Control.Monad.Bayes.Inference.SMC
import           Control.Monad.Bayes.Population
import           Control.Monad.Bayes.Sampler
import           Control.Monad.Bayes.Weighted

import           Control.Monad
import           Control.Monad.IO.Class

import           Graphics.EasyPlot

import           Debug.Trace

import qualified Data.Vector                           as V
import           Dice

import qualified Data.Map.Strict                       as M

import qualified Data.ByteString.Lazy                  as B
import           Data.Csv
import           Data.Time
import           Text.Printf

import qualified Control.Monad.Parallel                as MP

import           Criterion.Measurement

model :: MonadInfer m => m Int
model = dice_soft

truth = enumerate model

weightedToMap :: [(Int, Double)] -> M.Map Int Double
weightedToMap = foldr f M.empty
  where
    f (x, w) = M.insertWithKey g x w
    g _ new old = new + old

kl :: M.Map Int Double -> Double
kl = M.foldrWithKey f 0
  where
    f x w acc = acc - w * log (q x / w)
    q x = q' x truth
    q' _ [] = error "Erroneous value"
    q' x ((y, w):ys) =
      if x == y
        then w
        else q' x ys

ipmcmcToWeighted :: [V.Vector Int] -> M.Map Int Double
ipmcmcToWeighted rows = V.foldr f M.empty pop
  where
    pop = V.concat rows
    n = V.length pop
    piece = 1 / fromIntegral n
    f x = M.insertWithKey g x piece
    g _ = (+)

itersAccum :: ([a] -> b) -> [a] -> [b]
itersAccum f [x]    = [f [x]]
itersAccum f (x:xs) = f (x : xs) : itersAccum f xs

{-
simulate ::
     (MonadSample n, MonadInfer m)
  => [Int]
  -> (Int -> m Int -> Population n Int)
  -> n [Double]
  -}
simulate particles alg =
  map (kl . weightedToMap) <$>
  forM
    particles
    (\p -> do
       traceM $ "N = " ++ show p
       pop <- explicitPopulation $ normalize $ alg p model
       logtime
       return pop)

benchsmc :: MonadSample m => Int -> Int -> m [Double]
benchsmc n k = do
  let sampler = explicitPopulation $ normalize $ smcMultinomial 2 n model
      samples = replicateM k sampler
  map (kl . weightedToMap) <$> samples

benchsmcs :: (MonadSample m, MonadIO m) => Int -> [Int] -> m [[Double]]
benchsmcs k particles =
  forM particles $ \n -> do
    traceM $ "Benchmarking n = " ++ show n
    (x, b) <- timebench (benchsmc n k)
    let p = fromIntegral n
        a = b / fromIntegral k
    return (p : a : x)

timebench :: MonadIO m => m a -> m (a, Double)
timebench m = do
  start <- liftIO getTime
  x <- m
  end <- liftIO getTime
  return (x, end - start)

onlysmc ::
     (MP.MonadParallel m, MonadSample m) => Int -> Int -> m [(Int, Double)]
onlysmc n m =
  MP.replicateM m $ do
    pop <- explicitPopulation $ normalize $ smcMultinomial 2 n model
    idx <- categorical $ V.fromList $ map snd pop
    let (x, _) = pop !! idx
    return (x, 1 / fromIntegral m)

smciters ::
     (MonadIO m, MonadSample m, MP.MonadParallel m)
  => Int
  -> Int
  -> Int
  -> m [[(Int, Double)]]
smciters n m r =
  forM [1 .. r] $ \i -> do
    traceM $ "MCMC = " ++ show i
    logtime
    onlysmc n m

logtime :: MonadIO m => m ()
logtime = do
  wtime <- liftIO $ show <$> getTime
  ctime <- liftIO $ show <$> getCPUTime
  liftIO $ putStrLn $ wtime ++ "," ++ ctime
  return ()

concatNorm :: [[(Int, Double)]] -> [(Int, Double)]
concatNorm xs = map (\(x, w) -> (x, w / fromIntegral (length xs))) $ concat xs

mean :: [Double] -> Double
mean xs = sum $ map (/ n) xs
  where
    n = fromIntegral $ length xs

transpose :: [[a]] -> [[a]]
transpose xs
  | (null . head) xs = []
  | otherwise = map head xs : transpose (map tail xs)

main :: IO ()
main
  --now <- show <$> getCurrentTime
 = do
  initializeTime
  now <- show <$> getCurrentTime
  let particles = [2 ^ x | x <- [0 .. pmax]]
      pmax = floor $ logBase 2 10000
      --nodes = 32
      --iters = 1000
      samples = 10
      filename = printf "smc-particles-%d_%s.csv" samples now
  traceM $ show pmax
  kls <- sampleIO $ benchsmcs samples particles
  B.writeFile filename $ encode kls
  {-
      filename =
        printf
          "ipmcmc-correctness-iters%d-%d-%d-%d_%s"
          samples
          particles
          nodes
          iters
          now
  (kl, kls) <-
    sampleIO $ do
      res <- MP.replicateM samples $ ipmcmc particles nodes iters model
      let x = transpose $ map (reverse . itersAccum (kl . ipmcmcToWeighted)) res
      return (map mean x, x)
      filename = printf "memsmash-ipmcmc_%s" now
  samples <-
    sampleIO $ do
      let pop = ipmcmc particles nodes iters model
      MP.replicateM 1 ((kl . ipmcmcToWeighted) <$> pop)
  res <- sampleIO $ smciters particles nodes iters
  let kls = reverse $ itersAccum (kl . weightedToMap . concatNorm) res
  let datapoints =
        zip [2,4 .. fromIntegral nodes] $
        map (log . kl truth . ipmcmcToWeighted) res
  plot X11 $
    Data2D
      [ Title (printf "iPMCMC, M=%d, N=1-%d for R=%d" nodes particles iters)
      , Style Lines
      ]
      []
      datapoints
 = do
  let particles = [50 * x | x <- [1 .. 50]]
  (ismc, mbsmc) <-
    sampleIO $ do
      traceM "Sampling iPMCMC"
      ismc <- simulate particles (\p model -> toPopulation $ smc p model)
      traceM "Sampling MB"
      mbsmc <- simulate particles (smcMultinomial 2)
      return (ismc, mbsmc)
  now <- show <$> getCurrentTime
  let filename = printf "smc_50-2500_kl_dice_soft_%s" now
      hd = header ["particles", "ipmcmc", "monad-bayes"]
  B.writeFile filename $ encodeByName hd $ zip3 particles ismc mbsmc
  let idata = zip (map fromIntegral particles) (map log ismc)
      mbdata = zip (map fromIntegral particles) (map log mbsmc)
  traceM $ "Plotting"
  plot
    X11
    [ Data2D [Title "iPMCMC SMC", Style Lines] [] idata
    , Data2D [Title "Monad-Bayes SMC", Style Lines] [] mbdata
    ]
  -}
  return ()

instance ToNamedRecord (Int, Double, Double) where
  toNamedRecord (p, i, mb) =
    namedRecord ["particles" .= p, "ipmcmc" .= i, "monad-bayes" .= mb]
