{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Caret
import Caret.GenData

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String
import Lubeck.DV
import Lubeck.Drawing
import Lubeck.DV.Styling (getStyled)
import Control.Lens(to)
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
  persons <-genPersons 100
  let b = train (logistic) $ map (prepare [height, weight] heart_attack) persons
      plt = drawPlot $ plot persons [x <~ to height, y <~ to weight] pointG
  writeFile "out.html" $ renderHtml $ drawingToBlazeMarkup mempty (plt `Lubeck.DV.Styling.getStyled` mempty)
  print b

drawingToBlazeMarkup :: RenderingOptions -> Drawing -> H.Html
drawingToBlazeMarkup opts dr = toSvgAny opts dr (string . unpackStr) $
                  \name attrs nodes ->
                    foldr
                      (\(k,v) n -> n H.! H.customAttribute (fromString k) (fromString v))
                      (customParent (fromString $ unpackStr name) $ mconcat nodes)
                      (fmap (bimap unpackStr unpackStr) attrs)
