{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Caret
import Caret.GenData

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String
import Lubeck.DV
import Control.Monad (guard, forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Lubeck.Drawing
import Lubeck.DV.Styling (getStyled)
import Control.Lens(to, _1, _2, _3, _4)
import           Data.Bifunctor
import Lubeck.Str
  ( Str, packStr, unpackStr
  )
import Data.String
import           Text.Blaze          (Markup, Attribute, Tag, AttributeValue
                                     ,customAttribute, string, (!))
import           Text.Blaze.Internal (customParent)

import GenPoly
import qualified Control.Foldl as L

main :: IO ()
main = do
  persons <-genPersons 100
  testPersons <-genPersons 50
  let xs = [-1,-0.99.. 1]
  polyPts <- genPolyData [1,2,-3] 0.1 xs
  let vs = train ols $ map (polyExpand 4) polyPts
  print vs


  --mapM_ print persons
  let b = train (logistic) $ map (prepare [height, const 1] heart_attack) persons
      testPreds :: [(Double,Double)]
      testPreds = flip map testPersons $ \pers -> (height pers, predict logistic b $ predictors [height, const 1] pers)
      plt = drawPlot $ plot persons [x <~ to height ,
--                                     y <~ to (\p-> 10 * (weight p / height p - 1/2.5)) ,
                                     y <~ to weight,
                                     color <~ to heart_attack] pointG
      plt2 = drawPlot $ plot testPreds [x <~ _1,
                                        y <~ _2] pointG
      plt3 = drawPlot $ plot polyPts [x <~ _1,
                                      y <~ _2] pointG
  writeFile "out.html" $ renderHtml $ do
     drawingToBlazeMarkup mempty (plt3 `Lubeck.DV.Styling.getStyled` mempty)
     drawingToBlazeMarkup mempty (plt `Lubeck.DV.Styling.getStyled` mempty)
     drawingToBlazeMarkup mempty (plt2 `Lubeck.DV.Styling.getStyled` mempty)
  print testPreds
  print b
  let avgHeight = L.fold (L.premap height averageF) persons
  print (avgHeight :: Double)
  let morbidityBySmoking = L.fold (groupBy is_male $ L.premap heart_attack frequency) persons
  print (morbidityBySmoking :: Map Bool Double )
  --print $ sumEqProd 100

drawingToBlazeMarkup :: RenderingOptions -> Drawing -> H.Html
drawingToBlazeMarkup opts dr = toSvgAny opts dr (string . unpackStr) $
                  \name attrs nodes ->
                    foldr
                      (\(k,v) n -> n H.! H.customAttribute (fromString k) (fromString v))
                      (customParent (fromString $ unpackStr name) $ mconcat nodes)
                      (fmap (bimap unpackStr unpackStr) attrs)

sumEqProd :: Int -> [(Int,Int)]
sumEqProd hi = do
  x <- [1..hi]
  y <- [1..hi]
  guard (x*y == x+y)
  return (x,y)

groupBy :: (Eq a, Ord a) => (b -> a) -> L.Fold b c -> L.Fold b (Map a c)
groupBy f (L.Fold step initial extract) = L.Fold step1 Map.empty (Map.map extract) where
  step1 mapacc val =
    let group = f val
    in case Map.lookup group mapacc of
        Nothing -> Map.insert group (step initial val) mapacc
        Just vOld -> Map.insert group (step vOld val) mapacc

filterF :: (b -> Bool) -> L.Fold b c -> L.Fold b c
filterF p (L.Fold step initial extract) = L.Fold step1 initial extract where
  step1 acc val =
    if p val then step acc val else acc

averageF :: Fractional a => L.Fold a a
averageF = (/) <$> L.sum <*> L.genericLength

frequency :: L.Fold Bool Double
frequency = (/) <$> filterF id L.genericLength <*> L.genericLength


{-
data Prob a = Prob (Seed -> (a,Seed)) (Expr a)

normal :: Expr Double -> Expr Double -> Prob (Expr Double)


observe :: a -> a -> Prob ()

linreg :: [(Vector Double, Double)] -> Prob (Vector Double)
linreg theData = do
  let n = length theDaya
      p = length $ head $ fst theData
  betas <- sequence $ replicate p $ normal 0 1
  sd <- uniform 0 1
  forM_ theData $ \(x,yobs) -> do
    y <- normal (betas `dot` x) sd
    guard (y==yobs)
  return betas
-}
