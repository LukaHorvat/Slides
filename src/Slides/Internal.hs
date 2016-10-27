{-# LANGUAGE OverloadedStrings #-}
module Slides.Internal where

import Diagrams (Diagram)
import Diagrams.Backend.SVG (SVG(..))
import qualified Diagrams as Diag
import qualified Diagrams.Backend.SVG as SVG
import Slides.Common

html :: String -> String -> String
html tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

htmlClass :: String -> String -> String -> String
htmlClass tag cls content = "<" ++ tag ++ " class=\"" ++ cls ++ "\">" ++ content ++ "</" ++ tag ++ ">"

htmlCustom :: String -> String -> String -> String
htmlCustom tag att content = "<" ++ tag ++ " " ++ att ++ ">" ++ content ++ "</" ++ tag ++ ">"

dropAllButSvg :: String -> String
dropAllButSvg ('<' : 's' : 'v' : 'g' : rest) = "<svg" ++ rest
dropAllButSvg (_ : xs) = dropAllButSvg xs
dropAllButSvg _        = error "No <svg> tag"

svgFromDiagram :: Int -> Diagram SVG -> String
svgFromDiagram h = dropAllButSvg
                 . show
                 . Diag.renderDia SVG (SVG.SVGOptions (Diag.mkHeight $ fromIntegral h) Nothing "" [] True)

renderLeafContent :: ContentNode -> String
renderLeafContent (Header h s) = html ("h" ++ show h) s
renderLeafContent (Text str)   = str
renderLeafContent (RawSVG width height str) = htmlCustom "svg" (w ++ " " ++ h) str
    where w = "width=\"" ++ show width ++ "px\""
          h = "height=\"" ++ show height ++ "px\""
renderLeafContent Break         = "<br />"
renderLeafContent (Diagram h d) = svgFromDiagram h d
renderLeafContent _             = error "Non-leaf node given to renderLeafContent"
