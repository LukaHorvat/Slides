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

-- | Describes the behavior of the presentation element.
data Eagerness
    -- | The element won't be displayed until there's an explicit signal from the user (pressing the
    --   arrow key, etc.)
    = Delay
    -- | The element will be displayed as soon as it's encountered.
    | Immediate
    deriving (Eq, Show)

-- | The outermost type. Holds slides and styling.
data Presentation = Presentation { slides   :: [Slide]
                                 , style    :: Style
                                 -- | A plain string that will be put into the \<head> before
                                 --   everything else.
                                 , baseHead :: String }

-- | The outermost type of a single slide. Holds content nodes.
newtype Slide  = Slide { nodes :: [ContentNode] }

-- | The main type in the presentaion. Describes all the possible kinds of content.
data ContentNode
    -- | Generates a \<hN> tag where the N is the first argument.
    = Header Int String
    -- | Generates an unordered list that's immediately displayed.
    | List [ContentNode]
    -- | A plain text node.
    | Text String
    -- | Generates a \<br /> tag. A new line.
    | Break
    -- | Generates an SVG tag with the specified width and height and string contents.
    | RawSVG Int Int String
    -- | Generates an SVG tag from a given height and a Diagram
    | Diagram Int (Diagram SVG)
    -- | Generates a list of elements where each element is delayed. The 'Eagerness' parameter
    --   determines whether the list will immediately display the first element.
    | UnfoldList Eagerness [ContentNode]
    -- | Generates elements in sequence with the next one REPLACING the previous one. The Eagerness
    --   parameter determines whether the first element in sequence will be immediately displayed.
    | Sequence Eagerness [ContentNode]
    -- | Generates a 'list' of elements. The elements themselves are not wrapped in anything,
    --   unlike in a normal list where they're wrapped in \<li> tags, just concatinated together.
    | ConcatList [ContentNode]
    -- | The same as ConcatList with the display behavior of UnfoldList
    | UnfoldConcatList Eagerness [ContentNode]

-- | Rudimentary support for styling
data Style = Style
    { -- | Pairs of selectors that determine what kind of elements to apply the style to and the
      -- | styles themselves.
      selectors :: [(Selector, ElementStyle)]
    , baseCss   :: String }
    deriving (Eq, Show, Read)

-- | Describes which elements to apply the style to.
data Selector
    = HeaderSelector Int
    -- | Applies to everything.
    | UniversalSelector
    | TextSelector
    | SlideSelector
    deriving (Eq, Ord, Show, Read)

data ElementStyle = ElementStyle { backgroundColor :: Maybe (Colour Float)
                                 , fontFamily      :: Maybe String
                                 , fontSize        :: Maybe Int }
                                 deriving (Eq, Show, Read)

-- | An empty presentation set to the default style with UTF8 encoding.
emptyPresentation :: Presentation
emptyPresentation = Presentation [] emptyStyle "<meta charset=\"utf-8\" />"

-- | The default style.
emptyStyle :: Style
emptyStyle = Style [] $(embedStringFile "assets/default.css")

-- | Completely empty element style.
emptyElementStyle :: ElementStyle
emptyElementStyle = ElementStyle Nothing Nothing Nothing
