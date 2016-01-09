{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | To use this package you need to construct a Presentation tree using the types and constructors
--   from the 'Slides.Common' module (re-exported from this one). Then call one of the functions
--   bellow.
--   The generated HTML will not look like a presentation when you open it up in a browser, but
--   it has CSS guides in place that will split it up into pages properly when you print it.
--   Good default settings are A3-Landscape. You can just print to PDF to get the actual presentation.
--
--   Here's an example
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- sample :: 'Presentation'
-- sample =
--     'emptyPresentation' {
--         'slides' = [
--             'Slide' [
--                 'Header' 2 \"Title\",
--                 'Sequence' 'Immediate' [
--                     -- this delay does nothing because the parent 'Immediate' overrides it
--                     'UnfoldConcatList' 'Delay' [
--                         'Header' 3 \"Example\",
--                         'UnfoldList' 'Immediate' [
--                             \"These lines will unfold one by one.\",
--                             \"You can use some markdown in these strings like _this_ or *this* \\
--                             \\or \__this__ or **this**.\"
--                         ],
--                         'Diagram' 200 someDiagram
--                     ],
--                     'List' [
--                         \"This list will be shown in place of the previous title-list-diagram.\",
--                         \"This item will be shown immediately with the last one.\"
--                     ]
--                 ] -- note that the above title \"Title\" will remain there during the sequence.
--             ],
--             'Slide' [
--                 'Header' 2 \"Another slide\",
--                 'List' [
--                     \"Some text describing stuff.\",
--                     \"More text.\"
--                 ],
--                 'Sequence' 'Delay' [
--                     'Diagram' 200 a,
--                     'Diagram' 200 sequence,
--                     'Diagram' 200 of,
--                     'Diagram' 300 diagrams
--                 ]
--             ]
--         ]
--     }
--
-- main :: IO ()
-- main = 'writeToFile' \"index.html\" sample
-- @
--
--
-- The result is a HTML file which can be printed to PDF and would look like
-- <https://dl.dropboxusercontent.com/u/35032740/ShareX/2015/10/test.pdf this>
module Slides.Presentation
    ( renderPresentation, writeToFile, module Slides.Common
    ) where

import qualified Data.Colour.SRGB as Colour
import Data.Maybe (catMaybes)
import Slides.Common
import Slides.Sequencing
import Slides.Internal

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

-- | Render the Presentation to an HTML string.
renderPresentation :: Presentation -> String
renderPresentation = render

-- | Render a Presentation to an HTML file with UTF8 encoding.
writeToFile :: FilePath -> Presentation -> IO ()
writeToFile path = writeFile path . render
