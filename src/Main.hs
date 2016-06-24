{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Caret
import Caret.GenData

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String
import Lubeck.DV
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



main :: IO ()
main = do
  persons <-genPersons 200
  testPersons <-genPersons 50
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
  writeFile "out.html" $ renderHtml $ do
     drawingToBlazeMarkup mempty (plt `Lubeck.DV.Styling.getStyled` mempty)
     drawingToBlazeMarkup mempty (plt2 `Lubeck.DV.Styling.getStyled` mempty)
  --print testPreds
  print b

drawingToBlazeMarkup :: RenderingOptions -> Drawing -> H.Html
drawingToBlazeMarkup opts dr = toSvgAny opts dr (string . unpackStr) $
                  \name attrs nodes ->
                    foldr
                      (\(k,v) n -> n H.! H.customAttribute (fromString k) (fromString v))
                      (customParent (fromString $ unpackStr name) $ mconcat nodes)
                      (fmap (bimap unpackStr unpackStr) attrs)
