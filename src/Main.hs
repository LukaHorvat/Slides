{-# LANGUAGE OverloadedStrings #-}

module Main where
import Slides.Presentation
import Slides.Common
import qualified System.IO.UTF8 as UTF8
import Diagrams.Opis
import Diagrams.JednaTocka
import Diagrams.ViseTocka
import Diagrams.Uniformno

sample :: Presentation
sample =
    emptyPresentation {
        slides = [
            Slide [
                Header 1 "Genetski algoritmi",
                Header 2 "Križanja"
            ],
            Slide [
                Header 2 "Sadržaj",
                Break,
                List [
                    "Opis",
                    "Križanje s jednom točkom prekida",
                    "Križanje s više točka prekida",
                    "Uniformno križanje",
                    "Segmentno križanje",
                    "Križanje presjekom",
                    "Križanje unijom"
                ]
            ],
            Slide [
                Header 2 "Opis",
                Break,
                List [
                    "odabiremo dvije jedinke koje su demonstrirale dobre rezultate",
                    "želimo kombinirati njihove karakteristike",
                    "očekujemo da kombinacija daje dobar ili potencijalno **bolji** rezultat"
                ],
                Diagram 200 opis
            ],
            Slide [
                Header 2 "Križanje s jednom točkom prekida",
                List [
                    "odabire se indeks u kromosomu",
                    "uzima se polovica kromosoma do tog indeksa iz prvog roditelja te \
                    \polovica kromosoma od tog indeksa iz drugog roditelja"
                ],
                Diagram 400 jednaTocka
            ],
            Slide [
                Header 2 "Križanje s više točka prekida",
                List [
                    "odabiremo _k_ indeksa",
                    "uzimamo izmjenično komade iz prvog i drugog roditelja"
                ],
                Diagram 400 viseTocka
            ],
            Slide [
                Header 2 "Uniformno križnje",
                List [
                    "odluka o svakom genu u kromosomu djeteta se donosi zasebno",
                    "geni roditelja se u parovima proslijeđuju nekoj funkciji koja određuje \
                    \gen djeteta"
                ],
                Diagram 400 uniformno
            ]
        ]
    }



main :: IO ()
main = return ()
