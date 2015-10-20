{-# LANGUAGE DeriveFunctor #-}
module Slides.Sequencing where

import Slides.Common
import Slides.Internal

data StepF a = Step Eagerness a deriving (Functor, Show)
type Step = StepF String

(<++>) :: Step -> Step -> Step
(Step _ str1) <++> (Step e str2) = Step e (str1 ++ str2)

mergeSequences :: [[Step]] -> [Step]
mergeSequences seqs = case seqs of
    [] -> []
    (x : xs) -> lists x xs
    where endStates = scanl1 (<++>) $ map last seqs
          prepend p = map (p <++>)
          lists x xs = x ++ concat (zipWith prepend endStates xs)

setStepEagerness :: Eagerness -> Step -> Step
setStepEagerness e (Step _ str) = Step e str

setEagerness :: Eagerness -> [Step] -> [Step]
setEagerness _ [] = []
setEagerness e (s : ss) = setStepEagerness e s : ss

simplify :: [Step] -> [Step]
simplify [s] = [setStepEagerness Immediate s]
simplify ss = ss

sequenceContent :: ContentNode -> [Step]
sequenceContent (UnfoldList eager nodes) =
    setEagerness eager $ map (fmap (html "ul")) $ mergeSequences sequences
    where sequences = map (setEagerness Delay . map (fmap (html "li")) . sequenceContent) nodes
sequenceContent (Sequence eager nodes) =
    setEagerness eager $ concatMap (setEagerness Delay . sequenceContent) nodes
sequenceContent (List nodes) =
    setEagerness Immediate $ map (fmap (html "ul")) $ mergeSequences sequences
    where sequences = map (simplify . map (fmap (html "li")) . sequenceContent) nodes
sequenceContent (ConcatList nodes) =
    setEagerness Immediate $ mergeSequences sequences
    where sequences = map (simplify . sequenceContent) nodes
sequenceContent (UnfoldConcatList eager nodes) =
    setEagerness eager $ mergeSequences sequences
    where sequences = map (setEagerness Delay . sequenceContent) nodes
sequenceContent other = [Step Immediate $ renderLeafContent other]

stepsToStrings :: [Step] -> [String]
stepsToStrings [] = []
stepsToStrings (Step _ str : Step Delay str2 : ss) = str : stepsToStrings (Step Delay str2 : ss)
stepsToStrings [Step _ str] = [str]
stepsToStrings (Step _ _ : ss) = stepsToStrings ss
