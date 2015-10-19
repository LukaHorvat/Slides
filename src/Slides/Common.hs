{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Slides.Common where

import Diagrams (Diagram)
import Diagrams.Backend.SVG (SVG(..))
import qualified Diagrams as Diag
import qualified Diagrams.Backend.SVG as SVG
import Data.Colour (Colour)
import Data.FileEmbed

data    Eagerness    = Delay | Immediate deriving (Eq, Show)

data    Presentation = Presentation { slides   :: [Slide]
                                    , style    :: Style
                                    , baseHead :: String }

newtype Slide        = Slide { nodes :: [ContentNode] }

data    ContentNode  = Header Int String | List [ContentNode] | Text String | Break
                     | RawSVG Int Int String | Diagram Int (Diagram SVG)
                     | UnfoldList Eagerness [ContentNode]
                     | Sequence Eagerness [ContentNode]

data    Style        = Style { selectors :: [(Selector, ElementStyle)]
                             , baseCss   :: String }
                       deriving (Eq, Show, Read)

data    Selector     = HeaderSelector Int | UniversalSelector | TextSelector | SlideSelector
                       deriving (Eq, Ord, Show, Read)

data    ElementStyle = ElementStyle { backgroundColor :: Maybe (Colour Float)
                                    , fontFamily      :: Maybe String
                                    , fontSize        :: Maybe Int }
                       deriving (Eq, Show, Read)

emptyPresentation :: Presentation
emptyPresentation = Presentation [] emptyStyle "<meta charset=\"utf-8\" />"

emptyStyle :: Style
emptyStyle = Style [] $(embedStringFile "assets/default.css")

emptyElementStyle :: ElementStyle
emptyElementStyle = ElementStyle Nothing Nothing Nothing

html :: String -> String -> String
html tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

htmlClass :: String -> String -> String -> String
htmlClass tag cls content = "<" ++ tag ++ " class=\"" ++ cls ++ "\">" ++ content ++ "</" ++ tag ++ ">"

htmlCustom :: String -> String -> String -> String
htmlCustom tag att content = "<" ++ tag ++ " " ++ att ++ ">" ++ content ++ "</" ++ tag ++ ">"

dropAllButSvg :: String -> String
dropAllButSvg ('<' : 's' : 'v' : 'g' : rest) = "<svg" ++ rest
dropAllButSvg (x : xs) = dropAllButSvg xs
dropAllButSvg _        = error "No <svg> tag"

svgFromDiagram :: Int -> Diagram SVG -> String
svgFromDiagram h =
    dropAllButSvg . show . Diag.renderDia SVG (SVG.SVGOptions (Diag.mkHeight $ fromIntegral h) Nothing "")

renderLeafContent :: ContentNode -> String
renderLeafContent (Header h s) = html ("h" ++ show h) s
renderLeafContent (Text str)   = str
renderLeafContent (RawSVG width height str) = htmlCustom "svg" (w ++ " " ++ h) str
    where w = "width=\"" ++ show width ++ "px\""
          h = "height=\"" ++ show height ++ "px\""
renderLeafContent Break        = "<br />"
renderLeafContent (Diagram h d) = svgFromDiagram h d
