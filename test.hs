{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Writer.Strict

import DNA

wTestGroup :: String -> Writer [TestTree] a -> TestTree
wTestGroup n m = testGroup n $ execWriter m

wTestCase :: MonadWriter [TestTree] m => String -> Assertion -> m ()
wTestCase n = tell . (:[]) . testCase n

main = defaultMain tests


tests = wTestGroup "Tests" $ do
    let strand1, strand2 :: Strand
        strand1 = "TAGATCCAGTCCACATCGA"
        strand2 = "CGGATACTAAACCGA"

    wTestCase "Ribosome strand1" $
       ribosome strand1 @?= [[Search R Py, Ins A, Search R Pu, Move R, Ins T, Move L, Cut, Switch, Copy True]]

    wTestCase "Affinity strand1" $
       enzymeAffinity (head $ ribosome strand1) @?= C

    wTestCase "Ribosome strand2" $
       ribosome strand2 @?= [[Copy True, Ins A, Search R Py, Copy False], [Cut, Copy True]]

