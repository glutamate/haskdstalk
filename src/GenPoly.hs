module GenPoly where

import Data.Random
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)

polynomial :: [Double] -> Double -> Double
polynomial coeffs x = go 0 coeffs where
  go _ [] = 0
  go n (c:cs) = c * x ^ n + go (n+1) cs


genPolyData :: [Double] -> Double -> [Double] -> IO [(Double, Double)]
genPolyData coeffs sd xs = sample $ mapM gen xs where
  gen :: Double -> RVar (Double, Double)
  gen x =
    let mn = polynomial coeffs x
    in fmap (\y-> (x, y)) $ normal mn sd

--genUniform :: Int -> (Double, Double) -> IO [Double]
