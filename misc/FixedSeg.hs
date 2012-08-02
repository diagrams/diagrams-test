{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

env = getEnvelope (FLinear (p2 (2,3)) (p2 (5,8)))

main = defaultMain (square 2 ||| mempty # withEnvelope env ||| square 2)