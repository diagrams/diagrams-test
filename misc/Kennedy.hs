{-# LANGUAGE NoMonomorphismRestriction
           , TypeFamilies 
           , FlexibleInstances
  #-}

import Graphics.Rendering.Diagrams.Points

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Layout.Tree
import Data.Tree

import Data.Maybe
import Control.Arrow

lf x = Node x []

t1 = Node 'A' [Node 'B' (map lf "CDE"), Node 'F' [Node 'G' (map lf "HIJ")]]

t2 = Node 'A' [Node 'B' (map lf "CDE"), lf 'F', Node 'G' (map lf "HIJ")]

fullTree 1 = Node 1 []
fullTree n = Node n [t, t]
  where t = fullTree (n-1)

type instance V (Tree (a,x)) = V x

instance Transformable x => Transformable (Tree (a,x)) where
  transform = fmap . second . transform

tD = Node (rect 1 3) [Node (circle 0.2) [], Node (hcat . replicate 3 $ circle 1) [], Node (eqTriangle 5) []]

dia4 = renderTree ((<> circle 1 # fc white) . text . show) (~~) (symmLayout t1 # scale 4)

dia = renderTree id (~~) (symmLayout' with { slWidth  = fromMaybe (0,0) . extentX
                                           , slHeight = fromMaybe (0,0) . extentY } 
                            tD) # fc white

main = defaultMain dia