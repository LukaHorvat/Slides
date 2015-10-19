module Diagrams.JednaTocka where

import Diagrams.Imports
import Diagrams.Opis

box :: Bool -> Angle Double -> Diagram SVG
box on angle = ((square (sqrt 2) `clipTo` rotate angle pattern) <> base) # lwO 1.5
    where base = square (sqrt 2) # fc (if on then peachpuff else turquoise)

chromosome :: [Bool] -> Angle Double -> Diagram SVG
chromosome list angle = hcat $ map (pad 1.1 . (`box` angle)) list

first :: Diagram SVG
first = chromosome [True, False, True] (1/8 @@ turn)
    ||| padX 1.3 (chromosome [True, False] (1/8 @@ turn))

second :: Diagram SVG
second = chromosome [True, True, False] (3/8 @@ turn)
    ||| padX 1.3 (chromosome [False, False] (3/8 @@ turn))

child :: Diagram SVG
child = chromosome [True, False, True] (1/8 @@ turn)
    ||| padX 1.3 (chromosome [False, False] (3/8 @@ turn))

jednaTocka :: Diagram SVG
jednaTocka = vcat $ map (pad 1.1) [center first, plus, center second, padY 1.3 equal, center child]
