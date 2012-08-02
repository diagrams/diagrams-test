{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.Colour

t1 = text "H" # fcA (black `withOpacity` 0.5) <> square 1 # lw 0

t2 = text "H" # opacity 0.5 <> square 1 # lw 0

main = defaultMain (t1 ||| t2)