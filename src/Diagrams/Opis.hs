module Diagrams.Opis where

import Diagrams.Imports

pattern :: Diagram SVG
pattern = lwO 1.5 $ mconcat $ map mkLine [0..10]
    where baseLine = p2 (-1, 0) ~~ p2 (1, 0)
          mkLine i = baseLine # translateY (i / 5 - 1)

patternCircle :: Diagram SVG
patternCircle = circle 1 `clipTo` pattern # rotateBy (1/8)
             <> circle 1

turqCircle :: Diagram SVG
turqCircle = circle 1 # fc turquoise

l :: Diagram SVG
l = lwO 1.5 $ p2 (-0.1, 0) ~~ p2 (0.1, 0)

plus :: Diagram SVG
plus = l <> (l # rotateBy (1/4))

equal :: Diagram SVG
equal = (l # translateY (-0.05)) <> (l # translateY 0.05)

opis :: Diagram SVG
opis = hcat $ map (pad 1.1) elements
    where elements = [turqCircle, plus, patternCircle, equal, patternCircle <> turqCircle]
