module MySprinkler where

import           Control.Arrow                     ((***))
import           Control.Monad.Bayes.Class
import           Control.Monad.Bayes.Enumerator
import           Control.Monad.Bayes.Inference.SMC
import           Control.Monad.Bayes.Population
import           Control.Monad.Bayes.Sampler

--import           Graphics.Rendering.Chart.Backend.Cairo
--import           Graphics.Rendering.Chart.Easy
import           Control.Monad.IO.Class

ssprinkler :: MonadSample m => m Bool
ssprinkler = do
  cloudy <- bernoulli 0.5
  sprinkler <-
    bernoulli $
    if cloudy
      then 0.1
      else 0.5
  rain <-
    bernoulli $
    if cloudy
      then 0.8
      else 0.2
  bernoulli $
    case (sprinkler, rain) of
      (True, True)   -> 0.99
      (True, _)      -> 0.9
      (_, True)      -> 0.9
      (False, False) -> 0.0

isprinkler :: MonadInfer m => m Bool
isprinkler = do
  cloudy <- bernoulli 0.5
  sprinkler <-
    bernoulli $
    if cloudy
      then 0.1
      else 0.5
  rain <-
    bernoulli $
    if cloudy
      then 0.8
      else 0.2
  wet <-
    bernoulli $
    case (sprinkler, rain) of
      (True, True)   -> 0.99
      (True, _)      -> 0.9
      (_, True)      -> 0.9
      (False, False) -> 0.0
  condition rain
  return wet

dices :: MonadInfer m => m Int
dices = do
  x <- uniformD [1 .. 6 :: Int]
  y <- uniformD [1 .. 6]
  condition (x + y > 7)
  return x

nlssm :: MonadSample m => Int -> m ([Double], [Double])
nlssm t = do
  x1 <- normal 0 5
  y1 <- observe x1
  nlssmHelper ([x1], [y1]) 2 t

nlssmHelper ::
     MonadSample m
  => ([Double], [Double])
  -> Int
  -> Int
  -> m ([Double], [Double])
nlssmHelper s@(x:xs, ys) t end
  | t > end = return (reverse *** reverse $ s)
  | otherwise = do
    delta <- normal 0 10
    let tf = fromIntegral t
        x' = 0.5 * x + 25 * (x / (1 + x * x)) + 8 * cos (1.2 * tf) + delta
    y' <- observe x'
    nlssmHelper (x' : x : xs, y' : ys) (t + 1) end

observe x = do
  eps <- normal 0 10
  return $ 0.05 * x * x + eps
{-
lgssm :: MonadSample m => Double -> Double -> Transition -> Int -> m [State]
lgssm mu v trans t = do
  start <- normal mu v
  states <- foldrM trans ([start], []) [1 .. t]
   <$> states

transition ::
     MonadSample m
  => Double
  -> Double
  -> Double
  -> Double
  -> a
  -> ([Double], [Double])
  -> m ([Double], [Double])
trans ohm sigma alpha beta _ (x:xs, ys) = do
  d <- normal 0 ohm
  eps <- normal 0 sigma
  let x' = alpha * x + d
      y' = beta * x' + eps
  return (x' : x : xs, y' : ys)
plotnlssm :: Int -> IO ()
plotnlssm t = do
  (x, y) <- liftIO $ sampleIO $ nlssm t
  let xpoints = zip [1 ..] x :: [(Double, Double)]
      ypoints = zip [1 ..] y :: [(Double, Double)]
  toFile def ("nlssm" ++ show t ++ ".png") $ do
    plot $ line "x" [xpoints]
    plot $ line "y" [ypoints]
    -}
