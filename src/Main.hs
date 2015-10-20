{-# LANGUAGE OverloadedStrings #-}

module Main where
import Slides.Presentation
import Slides.Common
import qualified System.IO.UTF8 as UTF8
import Diagrams.Opis
import Diagrams.JednaTocka
import Diagrams.ViseTocka
import Diagrams.Uniformno
import Diagrams.Prihvatljivost
import Diagrams.Presjek

sample :: Presentation
sample =
    emptyPresentation {
        slides = [
            Slide [
                Header 1 "Genetski algoritmi",
                Header 2 "Križanja",
                Break,
                Break,
                Break,
                Break,
                Break,
                Break,
                Break,
                "_Luka Horvat_",
                Break,
                "_19. 10. 2015._"
            ],
            Slide [
                Header 2 "Sadržaj",
                Break,
                List [
                    "Opis",
                    "Križanje s jednom točkom prekida",
                    "Križanje s više točaka prekida",
                    "Segmentno križanje",
                    "Uniformno križanje",
                    "Križanje presjekom"
                ]
            ],
            Slide [
                Header 2 "Opis",
                Break,
                List [
                    "Odabiremo dvije jedinke koje su demonstrirale dobre rezultate.",
                    "Želimo kombinirati njihove karakteristike.",
                    "Očekujemo da kombinacija daje dobar ili potencijalno **bolji** rezultat."
                ],
                Diagram 200 opis
            ],
            Slide [
                Header 2 "Križanje s jednom točkom prekida",
                List [
                    "Odabire se indeks u kromosomu.",
                    "Uzima se polovica kromosoma do tog indeksa iz prvog roditelja te \
                    \polovica kromosoma od tog indeksa iz drugog roditelja."
                ],
                Diagram 500 jednaTocka
            ],
            Slide [
                Header 2 "Križanje s više točaka prekida",
                List [
                    "Odabiremo _k_ indeksa.",
                    "Uzimamo izmjenično komade iz prvog i drugog roditelja."
                ],
                Diagram 500 viseTocka
            ],
            Slide [
                Header 2 "Segmentno križanje",
                Break,
                List [
                    "Varijanta križanja s više točaka prekida.",
                    "Broj _k_ se određuje posebno za svako križanje.",
                    "Pogodno ako nam duljina kromosoma nije konstantna."
                ]
            ],
            Slide [
                Header 2 "Uniformno križanje",
                List [
                    "Odluka o svakom genu u kromosomu djeteta donosi se zasebno.",
                    "Geni roditelja se u parovima proslijeđuju nekoj funkciji koja određuje \
                    \gen djeteta."
                ],
                Diagram 400 uniformno
            ],
            Slide [
                Header 2 "Prihvatljivost rješenja",
                Sequence Immediate [
                    UnfoldConcatList Delay [
                        Header 3 "Primjer",
                        UnfoldList Immediate [
                            "Odabiremo _n_ elemenata iz nekog skupa.",
                            "Jedan odabir označavamo bit-vektorom gdje jedinica na _i_-tom mjestu \
                            \signalizira da je _i_-ti element odabran.",
                            "Broj jedinica mora biti _n_!"
                        ],
                        Diagram 200 prihvatljivost
                    ]
                ]
            ],
            Slide [
                Header 2 "Prihvatljivost rješenja",
                List [
                    "Konkretan problem često zahtjeva određene uvjete koje potencijalno rješenje \
                    \mora zadovoljavati.",
                    "Ako već imamo dva rješenja koja zadovoljavaju uvijet, htjeli bi ih kombinirati \
                    \tako da njihovo dijete čim lakše dovedemo do zadovoljavajućeg stanja."
                ]
            ],
            Slide [
                Header 2 "Križanje presjekom",
                List [
                    "Uzmemo one elemente skupa koji su u oba odabira.",
                    "Tim postupkom imamo garanciju da je broj odabranih elemenata _≤n_",
                    "Sad možemo dopuniti rješenje do prihvatljivog."
                ],
                Sequence Delay [
                    Diagram 200 presjek1,
                    Diagram 200 presjek2,
                    Diagram 200 presjek3,
                    Diagram 300 presjek4
                ]
            ],
            Slide [
                Header 2 "Reference",
                List [
                    "-referenca-"
                ]
            ]
        ]
    }



main :: IO ()
main = return ()
