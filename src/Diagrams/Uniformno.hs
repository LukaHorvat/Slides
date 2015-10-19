module Diagrams.Uniformno where

import Diagrams.Imports
import Diagrams.JednaTocka (chromosome)
import Diagrams.Opis

firstList, secondList :: [Bool]
firstList = [True, False, True, True, False, False, True, False]
secondList = [True, True, False, False, False, True, True, False]

first :: Diagram SVG
first = chromosome firstList (1/8 @@ turn)

second :: Diagram SVG
second = chromosome secondList (3/8 @@ turn)

child :: Diagram SVG
child = chromosome (zipWith (/=) firstList secondList) (0 @@ turn)

xor :: Diagram SVG
xor = text "XOR" # font "Segoe UI" # scale 0.4

uniformno :: Diagram SVG
uniformno = vcat [center first, separator, separate xor, center second, separate equal, center child]
    where separator = strut (r2 (0, 0.1))
          separate d = vcat [separator, d, separator]
