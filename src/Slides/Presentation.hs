{-# LANGUAGE RecordWildCards, TemplateHaskell, OverloadedStrings #-}
module Slides.Presentation where

import Data.Colour (Colour)
import qualified Data.Colour.SRGB as Colour
import Data.Maybe (catMaybes)
import Data.FileEmbed
import qualified Text.RegexPR as Regex
import Data.String (IsString(..))
import Diagrams (Diagram)
import Diagrams.Backend.SVG (SVG(..))
import qualified Diagrams as Diag
import qualified Diagrams.Backend.SVG as SVG

data    Presentation = Presentation { slides   :: [Slide]
                                    , style    :: Style
                                    , baseHead :: String }

newtype Slide        = Slide { nodes :: [ContentNode] }

data    ContentNode  = Header Int String | List [ContentNode] | Text String | Break
                     | RawSVG Int Int String | Diagram Int (Diagram SVG)

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

class Renderable a where
    render :: a -> String

instance Renderable Presentation where
    render Presentation{..} = html "head" h ++ html "body" b
        where h = baseHead ++ render style
              b = concatMap render slides

instance Renderable Slide where
    render Slide{..} = htmlClass "div" "slide" $ concatMap render nodes

instance Renderable ContentNode where
    render (Header h s) = html ("h" ++ show h) s
    render (List items) = html "ul" $ concatMap (html "li" . render) items
    render (Text str)   = str
    render (RawSVG width height str) = htmlCustom "svg" (w ++ " " ++ h) str
        where w = "width=\"" ++ show width ++ "px\""
              h = "height=\"" ++ show height ++ "px\""
    render Break        = "<br />"
    render (Diagram h d) = svgFromDiagram h d

instance Renderable Style where
    render Style{..} = html "style" (baseCss ++ "\n" ++ concatMap renderSelector selectors)
        where renderSelector (k, v) =  render k ++ " { " ++ render v ++ " }\n"

instance Renderable Selector where
    render (HeaderSelector h) = "h" ++ show h
    render UniversalSelector  = "*"
    render TextSelector       = "p"
    render SlideSelector      = ".slide"

($>) :: Functor f => f a -> (a -> b) -> f b
($>) = flip fmap

instance Renderable ElementStyle where
    render ElementStyle{..} = concat $ catMaybes
        [ backgroundColor $> \col -> "background-color: " ++ Colour.sRGB24show col ++ ";\n"
        , fontFamily $> \ff -> "font-family: " ++ ff ++ ";\n"
        , fontSize $> \fs -> "font-size: " ++ show fs ++ ";" ]

inlineMarkdown :: String -> ContentNode
inlineMarkdown = Text . Regex.subRegexPR "\\*(.+?)\\*" "<i>\\1</i>"
                      . Regex.subRegexPR "_(.+?)_" "<i>\\1</i>"
                      . Regex.subRegexPR "\\*\\*(.+?)\\*\\*" "<b>\\1</b>"
                      . Regex.subRegexPR "__(.+?)__" "<b>\\1</b>"

instance IsString ContentNode where
    fromString = inlineMarkdown

dropAllButSvg :: String -> String
dropAllButSvg ('<' : 's' : 'v' : 'g' : rest) = "<svg" ++ rest
dropAllButSvg (x : xs) = dropAllButSvg xs
dropAllButSvg _        = error "No <svg> tag"

svgFromDiagram :: Int -> Diagram SVG -> String
svgFromDiagram h =
    dropAllButSvg . show . Diag.renderDia SVG (SVG.SVGOptions (Diag.mkHeight $ fromIntegral h) Nothing "")
