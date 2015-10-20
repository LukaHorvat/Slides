module Diagrams.Presjek where

import Diagrams.Imports
import Diagrams.Opis
import Diagrams.JednaTocka

area :: Int -> Diagram SVG
area n = text ("površina " ++ show n) # font "Segoe UI" # fontSize 15

path :: Path V2 Double
path = fromOffsets [ unitX, unitY, unitY, unitX # reflectX # scaleX 2
                         , unitY # reflectY, unitX, unitY # reflectY ]


parent1 :: Diagram SVG
parent1 = path `clipTo` patt <> stroke path # fc peachpuff # lwO 1.5
    where patt = pattern # rotateBy (1/8) # scale 1.3 # translate (r2 (0.1, 1.1))

parent2 :: Diagram SVG
parent2 = reflectX parent1

presjek1 :: Diagram SVG
presjek1 = pad 1.1 $ left ||| strutX 0.1 ||| plus ||| strutX 0.1 ||| right
    where left = center (area 3 === strutY 0.1 === parent1)
          right = center (area 3 === strutY 0.1 === parent2)

presjek2 :: Diagram SVG
presjek2 = pad 1.1 $ parent1
        <> (parent2 # translateX 1)
        <> rect 1 2 # fc turquoise # translate (r2 (0.5, 1))

presjek3 :: Diagram SVG
presjek3 = pad 1.1 $ strutY 0.1 === area 2 === strutY 0.1 === rect 1 2 # fc turquoise # lwO 1.5

presjek4 :: Diagram SVG
presjek4 = pad 1.1 $ strutY 0.1 === area 3 === strutY 0.1 === rect1 === rect2
    where rect1 = rect 1 2 # fc turquoise # lwO 1.5
          rect2 = rect 1 1 # fc peachpuff # lwO 1.5
