{-# LANGUAGE TemplateHaskell #-}
module Slides.Common
    ( Eagerness(..), Presentation(..), Slide(..), ContentNode(..), Style(..)
    , Selector(..), ElementStyle(..), emptyPresentation, emptyStyle, emptyElementStyle
    ) where

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
                     | ConcatList [ContentNode]
                     | UnfoldConcatList Eagerness [ContentNode]

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
