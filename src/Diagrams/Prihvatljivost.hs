module Diagrams.Prihvatljivost where

import Diagrams.Imports
import Diagrams.JednaTocka
import Diagrams.Opis

choiceBox :: Bool -> Diagram SVG -> Diagram SVG
choiceBox isOn dia = dia <> square (sqrt 2) # fc (if isOn then peachpuff else turquoise)
                                            # lwO 1.5

choice :: [Diagram SVG] -> [Int] -> Diagram SVG
choice diags choices = foldl (\d i -> d # connectOutside (i, True) (i, False)) diag choices
    where chosen = hcat $ map (\i -> named (i, False) $ choiceBox True $ diags !! i) choices
          possible = hcat $ map (pad 1.1) $ zipWith maybeChosen [0..] diags
          diag = vcat [chosen, strut (r2 (0, -0.5)), possible]
          maybeChosen i d = named (i, True) $ choiceBox (i `elem` choices) d

baseShapes :: [Diagram SVG]
baseShapes = map patternize lst
    where lst = [triangle 0.8 # translateY (-0.1), square 0.8, pentagon 0.5, hexagon 0.4, circle 0.4]
          patternize d = stroke d <> (d `clipTo` (pattern # rotateBy (1/8)))

prihvatljivost :: Diagram SVG
prihvatljivost = hcat $ map (padX 1.1) [ choice baseShapes [0, 1, 4]
                                       , choice baseShapes [1, 2, 3]
                                       , choice baseShapes [0, 3, 4] ]
