{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Slides.Presentation
    ( renderPresentation, writeToFile, module Slides.Common
    ) where

import Data.Colour (Colour)
import qualified Data.Colour.SRGB as Colour
import Data.Maybe (catMaybes)
import qualified Text.RegexPR as Regex
import Data.String (IsString(..))
import Data.List (groupBy)
import Slides.Common
import Slides.Sequencing
import Slides.Internal
import qualified System.IO.UTF8 as UTF8

class Renderable a where
    render :: a -> String

instance Renderable Presentation where
    render Presentation{..} = html "head" h ++ html "body" b
        where h = baseHead ++ render style
              b = concatMap render slides

instance Renderable Slide where
    render Slide{..} = concatMap (htmlClass "div" "slide") sequences
        where sequences = stepsToStrings $ mergeSequences $ map (simplify . sequenceContent) nodes

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
inlineMarkdown = Text . Regex.gsubRegexPR "\\*(.+?)\\*" "<i>\\1</i>"
                      . Regex.gsubRegexPR "_(.+?)_" "<i>\\1</i>"
                      . Regex.gsubRegexPR "\\*\\*(.+?)\\*\\*" "<b>\\1</b>"
                      . Regex.gsubRegexPR "__(.+?)__" "<b>\\1</b>"

instance IsString ContentNode where
    fromString = inlineMarkdown

-- | Render the Presentation to a HTML string.
renderPresentation :: Presentation -> String
renderPresentation = render

-- | Render a Presentation to a HTML file with UTF8 encoding.
writeToFile :: FilePath -> Presentation -> IO ()
writeToFile path = UTF8.writeFile path . render
