{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Slides.Presentation
import Data.FileEmbed

index :: String
index = $(embedStringFile "test/index.html")

sample :: Presentation
sample =
    emptyPresentation {
        slides = [
            Slide [
                Header 2 "Title",
                Sequence Immediate [
                    -- this delay does nothing because the parent Immediate overrides it
                    UnfoldConcatList Delay [
                        Header 3 "Example",
                        UnfoldList Immediate [
                            "These lines will unfold one by one.",
                            "You can use some markdown in these strings like _this_ or *this* \
                            \or __this__ or **this**.",
                            "unicodešđčćž"
                        ]
                    ],
                    List [
                        "This list will be shown in place of the previous title-list-diagram.",
                        "This item will be shown immediately with the last one."
                    ]
                ] -- note that the above title "Title" will remain there during the sequence.
            ],
            Slide [
                Header 2 "Another slide",
                List [
                    "Some text describing stuff.",
                    "More text."
                ]
            ]
        ]
    }

main :: IO ()
main = if renderPresentation sample == index then putStrLn "Test passed."
                                             else putStrLn "Test failed."
