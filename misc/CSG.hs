{-# LANGUAGE TypeFamilies #-}

-- | Unit tests for 3D CSG Operations

module CSG where

import           Diagrams.Prelude       hiding (nearly)
import           Diagrams.ThreeD.Shapes
import           Linear.Epsilon

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Maybe

tests :: TestTree
tests = testGroup "solids"
        [ testGroup "sphere"
          [ testGroup "Inside Query"
            [ testCase "origin is inside" $ assertInside True sphere origin
            , testCase "near surface inside" $
              assertInside True sphere zOneMinus
            , testCase "near surface outside" $
              assertInside False sphere zOnePlus
            ]
          , testGroup "Trace"
            [ testCase "+x from origin" $ rayTraceV origin unitX sphere @?= Just unitX
            , testCase "+y from origin" $  rayTraceV origin unitY sphere @?=Just unitY
            , testCase "+z from origin" $  rayTraceV origin unitZ sphere @?=Just unitZ
            ]
          , testGroup "Envelope"
            [ testCase "+x envelope" $ envelopeV unitX sphere @?= unitX
            , testCase "+y envelope" $ envelopeV unitY sphere @?= unitY
            , testCase "+z envelope" $ envelopeV unitZ sphere @?= unitZ
            ]
          ]
   , testGroup "cube"
     [ testGroup "Inside Query"
       [ testCase "near origin inside" $
         assertInside True cube (mkP3 slight slight slight)
       , testCase "near origin outside" $
         assertInside False cube (mkP3 (-slight) slight slight)
       , testCase "near opposite corner inside" $
         assertInside True cube (mkP3 oneMinus oneMinus oneMinus)
       , testCase "near opposite corner outside" $
         assertInside False cube (mkP3 oneMinus onePlus oneMinus)
       ]
     , testGroup "Trace"
       -- If we trace from the origin, we find the intersection at the origin!
       [ testCase "+x from near origin" $
         rayTraceV (mkP3 slight 0 0) unitX cube @?=Just (oneMinus *^ unitX)
       , testCase "+y from near origin" $
         rayTraceV (mkP3 0 slight 0) unitY cube @?=Just (oneMinus *^ unitY)
       , testCase "+z from near oprigin" $
         rayTraceV aboveOrigin unitZ cube @?=Just (oneMinus *^ unitZ)
       ]
     , testGroup "Envelope"
       [ testCase "+x envelope" $ envelopeV unitX cube @?= unitX
       ,  testCase "+y envelope" $ envelopeV unitY cube @?= unitY
       , testCase "+z envelope" $ envelopeV unitZ cube @?= unitZ
       ]
     ]
   , testGroup "cone"
     [ testGroup "Inside Query"
       [ testCase "slightly above origin" $
         assertInside True cone aboveOrigin
       , testCase "slightly below origin" $
         assertInside False cone (mkP3 0 0 (-slight))
       , testCase "near (1,0,0) inside" $
         assertInside True cone (mkP3 oneMinus 0 (slight/2))
       , testCase "near (1,0,0) outside" $
         assertInside False cone (mkP3 onePlus 0 0)
       , testCase "slightly below apex" $
         assertInside True cone zOneMinus
       , testCase "slightly above apex" $
         assertInside False cone zOnePlus
       ]
     , testGroup "Trace"
       [ testCase "+x from near origin" $
         fromJust (rayTraceV aboveOrigin unitX cone) `nearly` (oneMinus *^ unitX)
       , testCase "+z from near origin" $
         fromJust (rayTraceV aboveOrigin unitZ cone) `nearly` (oneMinus *^ unitZ)
       , testCase "+x from near (1,0,0) inside" $
         fromJust (rayTraceV (mkP3 (1-2*slight) 0 slight) unitX cone) `nearly` (slight *^ unitX)
       , testCase "+x from near (1,0,0) outside" $
         assertNothing $ rayTraceV (mkP3 1 0 slight) unitX cone
       , testCase "+z from near apex inside" $
         fromJust (rayTraceV zOneMinus unitZ cone) `nearly` (slight *^ unitZ)
       , testCase "+Z from near apex outside" $
         assertNothing $rayTraceV zOnePlus unitZ cone
       ]
     , testGroup "Envelope"
       [ testCase "+x from origin" $ envelopeV unitX cone @?= unitX
       , testCase "+y from origin" $ envelopeV unitY cone @?= unitY
       , testCase "+z from origin" $ envelopeV unitZ cone @?= unitZ
       ]
     ]
   , testGroup "cylinder"
     [ testGroup "Inside Query"
       [ testCase "slightly above origin" $
         assertInside True cylinder aboveOrigin
       , testCase "slightly below top" $
         assertInside True cylinder zOneMinus
       , testCase "near (1,0,0) inside" $
         assertInside True cylinder (mkP3 oneMinus 0 slight)
       , testCase "near (1,0,0) outside" $
         assertInside False cylinder (mkP3 onePlus 0 slight)
       ]
     , testGroup "Trace"
       [ testCase "+x from near origin" $
         fromJust (rayTraceV aboveOrigin unitX cylinder) `nearly` unitX
       , testCase "+x from near (1,0,0) inside" $
         fromJust (rayTraceV (mkP3 oneMinus 0 slight) unitX cylinder) `nearly` (slight *^ unitX)
       , testCase "+x from near (1,0,0) outside" $
         assertNothing $ rayTraceV (mkP3 onePlus 0 slight) unitX cylinder
       , testCase "+z from near origin" $
         fromJust (rayTraceV aboveOrigin unitZ cylinder) `nearly` (oneMinus *^ unitZ)
       , testCase "+z from near top inside" $
         fromJust (rayTraceV zOneMinus unitZ cylinder) `nearly` (slight *^ unitZ)
       , testCase "+z from near top outside" $
         assertNothing $ rayTraceV zOnePlus unitZ cylinder
       ]
     , testGroup "Envelope"
       [ testCase "+x from origin" $ envelopeV unitX cylinder @?= unitX
       , testCase "+y from origin" $ envelopeV unitY cylinder @?= unitY
       , testCase "+z from origin" $ envelopeV unitZ cylinder @?= unitZ
       ]
     ]
   ]

-- Conveniences to avoid repetition

-- constrain to Double to reduce inline type signatures in this module
assertInside :: (Inside t, Vn t ~ V3 Double) => Bool -> t -> P3 Double -> Assertion
assertInside result t p  = result @?= getAny (runQuery (inside t) p)

-- | near enough for test success
slight :: Double
slight = 0.001

-- | slightly more than 1
onePlus :: Double
onePlus = 1 + slight

-- | slightly less than 1
oneMinus :: Double
oneMinus = 1 - slight

nearly :: Epsilon a => a -> a -> Assertion
nearly a b = assertBool "Expression was not near enough to zero" $
             nearZero (a - b)

assertNothing :: Maybe a -> Assertion
assertNothing = assertBool "Expression was not Nothing" . isNothing

aboveOrigin :: Point V3 Double
aboveOrigin = mkP3 0 0 slight

zOnePlus :: Point V3 Double
zOnePlus = mkP3 0 0 onePlus

zOneMinus :: Point V3 Double
zOneMinus = mkP3 0 0 oneMinus
