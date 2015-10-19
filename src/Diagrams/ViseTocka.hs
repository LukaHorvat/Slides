module Diagrams.ViseTocka where

import Diagrams.Imports
import Diagrams.JednaTocka (chromosome)
import Diagrams.Opis

first :: Diagram SVG
first = hcat [ chromosome [True, False, True] (1/8 @@ turn)
             , strut (r2 (0.3, 0))
             , chromosome [True, False] (1/8 @@ turn)
             , strut (r2 (0.3, 0))
             , chromosome [False, True, False] (1/8 @@ turn) ]

second :: Diagram SVG
second = hcat [ chromosome [True, True, False] (3/8 @@ turn)
              , strut (r2 (0.3, 0))
              , chromosome [False, False] (3/8 @@ turn)
              , strut (r2 (0.3, 0))
              , chromosome [True, True, False] (3/8 @@ turn) ]

child :: Diagram SVG
child = hcat [ chromosome [True, False, True] (1/8 @@ turn)
             , strut (r2 (0.3, 0))
             , chromosome [False, False] (3/8 @@ turn)
             , strut (r2 (0.3, 0))
             , chromosome [False, True, False] (1/8 @@ turn) ]

viseTocka :: Diagram SVG
viseTocka = vcat $ map (pad 1.1) [center first, plus, center second, padY 1.3 equal, center child]
