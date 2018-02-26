module Main where

import Control.Monad.CC

main = undefined

data D a = !a :+ !a
  deriving (Show, Eq)

instance (Num a, Fractional a) => Num (D a) where
  n :+ n' + m :+ m' = (n + m) :+ (n' + m')
  n :+ n' - m :+ m' = (n - m) :+ (n' - m')
  n :+ n' * m :+ m' = (n * m) :+ (n' * m + n * m')
  negate (n :+ n') = negate n :+ negate n'
  abs (n :+ _) = n :+ 0
  signum (n :+ n') = 1 :+ (n' / n)
  fromInteger n = fromInteger n :+ 0

instance Fractional a => Fractional (D a) where
  n :+ n' / m :+ m' = (n / m) :+ ((n' * m - n * m') / m ^ 2)
  fromRational r = fromRational r :+ 0

-- https://gist.github.com/alexknvl/4412f775328ff25cbe29e63abb33393c

-- f の x における微分係数
grad :: (D Double -> CC ans (D Double)) -> Double -> CC ans Double
grad f x = do
  let x1 = x :+ 0
  _ :+ d <- reset $ \p -> f x1
  return d

-- >>> ex1
-- 1
ex1 = runCC $ do
  p <- newPrompt
  pushPrompt p $ return 1

-- >>> ex2
-- []
ex2 = runCC $ do
  p <- newPrompt
  pushPrompt p $ (1:) . (2:) <$> (withSubCont p $ \_k -> return [])

-- >>> ex2'
-- []
ex2' = runCC $ do
  p <- newPrompt
  pushPrompt p $ (1:) . (2:) <$> abort p (return [])

-- >>> ex3
-- [1, 2]
ex3 = runCC $ do
  p <- newPrompt
  pushPrompt p $ (1:) . (2:) <$> (withSubCont p $ \k -> pushSubCont k (return []))

-- >>> ex4
-- [1]
ex4 = runCC $ reset $ \p -> do
  y <- shift p $ \k -> (1:) <$> k (return [])
  shift p $ \_ -> return y

-- >>> ex4'
-- []
ex4' = runCC $ reset $ \p -> do
  y <- control p $ \k -> (1:) <$> k (return [])
  control p $ \_ -> return y
