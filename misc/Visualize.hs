{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Graphics.Rendering.Diagrams.Points

funs          = map (flip (^)) [2..6]
visualize f   = showLabels $
                stroke (star (StarFun f) (regPoly 7 1)) # lw 0.05 # lc red
                <> stroke' with { vertexNames = [[0 .. 6 :: Integer]] }
                           (regPoly 7 1)
                   # lw 0.02
example       = hcat' with {sep = 0.5} $ map visualize funs

-- main = print (names (example :: Diagram Cairo R2))

main = defaultMain (example # fontSize 0.5)
