{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

import Control.Lens hiding ((#), at, from)
import Numeric.Extras

import Diagrams.Prelude
import Diagrams.Coordinates
import Diagrams.Located
import Data.AffineSpace.Point

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Control.DeepSeq

------------------------------------------------------------
    -- Approximate Comparison for Doubles, Points

epsilon = 0.001

class Approx a where
  (=~) :: a -> a -> Bool

infix 4 =~

--instance (Fractional a, Ord a) => Approx a where
instance Approx Double where
  (=~) a b = abs (a - b) < epsilon

instance Approx R2 where
    z1 =~ z2 = (z1^._x) =~ (z2^._x) && (z1^._y) =~ (z2^._y)

instance Approx v => Approx (Point v) where
    p =~ q = unPoint p =~ unPoint q

instance Approx Angle where
    a =~ b = normA (a^.rad) =~ normA (b^.rad) where
      normA ang = let ang' = ang `fmod` pi in if ang' >= 0 then ang' else ang'+pi

instance Approx (Offset Closed R2) where
    OffsetClosed v0 =~ OffsetClosed v1 = v0 =~ v1

instance Approx (Segment Closed R2) where
    Linear o0 =~ Linear o1 = o0 =~ o1
    Cubic c0 d0 o0 =~ Cubic c1 d1 o1 = c0 =~ c1 && d0 =~ d1 && o0 =~ o1

instance Approx (Trail R2) where
    t0 =~ t1 = and $ zipWith (=~) (trailSegments t0) (trailSegments t1)

instance (Approx a, Approx (V a), AdditiveGroup (V a)) => Approx (Located a) where
    a0 =~ a1 = (loc a0 .-. origin) =~ (loc a1 .-. origin) && unLoc a0 =~ unLoc a1

instance Approx a => Approx (Maybe a) where
    Nothing =~ Nothing = True
    Nothing =~ Just _ = False
    Just _ =~ Nothing = False
    Just l =~ Just r = l =~ r
------------------------------------------------------------
-- Arbitrary instances for Points, Paths

instance Arbitrary R2 where
    arbitrary = (^&) <$> arbitrary <*> arbitrary

instance (Arbitrary v, AdditiveGroup v) => Arbitrary (Point v) where
    arbitrary = P <$> arbitrary

instance Arbitrary Angle where
    arbitrary = review rad <$> arbitrary
------------------------------------------------------------
-- NFData instances for Paths, all trivial

instance NFData R2

instance NFData v => NFData (Point v) where
    rnf p = rnf $ unPoint p

------------------------------------------------------------
-- Some unit tests to start with

tests = [
    testGroup "TwoD.Arc" [
           testProperty "arc start" $ \s e -> s /= e ==> pathVertices (arc s e :: Path R2) ^? _head . _head =~ Just (origin .+^ fromDirection s)
-- Second test does not pass, because of arc behavior on > fullTurn
-- Probably arc code should stay the same, but docs should explain better.
         , testProperty "arc end" $ \s e -> s /= e ==> pathVertices (arc s e :: Path R2) ^? _head . _last =~ Just (origin .+^ fromDirection e)
         ]
    ]

main = defaultMain tests
