{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmF she #-}

import Data.Maybe
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

bez :: Diagram Cairo R2
bez = translateX (-1) $ fromSegments [Cubic (r2 (1,2)) (r2 (3,-1)) (r2 (4,1))]

c :: Diagram Cairo R2
c = circle 1 # scaleY 2

d :: Diagram Cairo R2
d = square 1 <> square 0.7 # translate (r2 (1/2, -0.3))

seg = Linear (r2 (3,0)) # rotateBy (13/16)

p :: Path R2
p = fromOffsets [unitX, unitX]

-- main = defaultMain . bg white . pad 1.1 $ linesTo (p2 (0, -2)) bez

main = animMain anim

linesTo pt d = d <> mconcat (catMaybes $ map l [0, 1/25 .. tau/2 :: Rad])
  where l dir = (pt ~~) <$> (traceP pt (e dir) d)

animBez = linesTo (p2 (0,-2)) <$> (| rotateBy ui ~bez |)

anim = animBez # stretch 5 <> pure (animRect animBez # fc white)
