{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables, LambdaCase #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series (generate)
import Control.Monad.Writer.Strict
import Data.List (sort)

import qualified Zipper as Z
import DNA

tell1 :: MonadWriter [w] m => w -> m ()
tell1 = tell . (:[])

wTestGroup :: String -> Writer [TestTree] a -> TestTree
wTestGroup n m = testGroup n $ execWriter m

wTestCase :: MonadWriter [TestTree] m => String -> Assertion -> m ()
wTestCase n = tell1 . testCase n

main = defaultMain $ testGroup "Tests" [zipperTests, dnaTests]


zipperTests = wTestGroup "Zipper Tests" $ do
    tell1 $ SC.testProperty "from/toList" $
        \(l :: [Int]) -> over (generate $ \d -> take d [0..(length l - 1)]) $ \i ->
            l == [] || l == Z.toList (Z.fromList i l)

    tell1 $ SC.testProperty "reverse" $
        \(z :: Z.Zipper Int) ->
            reverse (Z.toList z) == Z.toList (Z.reverse z)


dnaTests = wTestGroup "DNA Tests" $ do
    let strand1, strand2 :: Strand
        strand1 = "TAGATCCAGTCCACATCGA"
        strand2 = "CGGATACTAAACCGA"
        enzyme4 = [Search R Pu, Ins C, Copy True, Move R, Move L, Switch, Search L Pu, Ins T]
        enzyme3 = [Search R Py, Copy True, Search R Pu, Cut]

    wTestCase "synthesize examples" $ do
       synthesize strand1 @?= [[Search R Py, Ins A, Search R Pu, Move R, Ins T, Move L, Cut, Switch, Copy True]]
       synthesize strand2 @?= [[Copy True, Ins A, Search R Py, Copy False], [Cut, Copy True]]

    wTestCase "Affinity examples" $ do
       enzymeAffinity (head $ synthesize strand1) @?= C
    --    enzymeAffinity enzyme4 @?= G -- error in the book ?
       enzymeAffinity enzyme3 @?= A

    wTestCase "runEnzyme basic" $ do
        runEnzyme [Move R, Ins T] "ACA" 0 @?= ["ACTA"]
        runEnzyme [Move R, Ins T] "ACA" 1 @?= ["ACAT"]
        runEnzyme [Del] "ACT" 0 @?= ["CT"]
        runEnzyme [Del] "ACT" 1 @?= ["AT"]
        runEnzyme [Del, Ins T] "ACA" 0 @?= ["CTA"]

    wTestCase "runEnzyme abort" $ do
        runEnzyme [Ins G, Move R, Ins T] "AAA" 2 @?= ["AAAG"]
        runEnzyme [Ins G, Move R, Move L, Ins T] "AAA" 2 @?= ["AAAG"]
        runEnzyme [Ins G, Move L, Move L, Ins T] "AAA" 2 @?= ["AATAG"]
        runEnzyme [Ins G, Move L, Move L, Ins T] "AAA" 0 @?= ["AGAA"]

    wTestCase "runEnzyme copy" $ do
        runEnzyme [Copy True] "ACG" 0 @?= ["ACG", "T"]
        runEnzyme [Copy True, Move R, Move R] "ACG" 0 @?= ["ACG", "CGT"]
        runEnzyme [Copy True, Copy False, Move R, Move R, Copy True] "ACG" 0 @?= ["ACG", "C", "T"]
        runEnzyme [Copy True, Move R, Copy False, Move R, Copy True] "ACG" 0 @?= ["ACG", "CGT"]
        runEnzyme [Copy True, Ins T] "ACG" 0 @?= ["ATCG", "AT"]
        runEnzyme [Copy True, Copy False, Ins T] "ACG" 0 @?= ["ATCG", "T"]
        runEnzyme [Copy True, Move R, Move L, Copy False, Ins T] "ACG" 0 @?= ["ATCG", "G", "T"]
        runEnzyme [Copy True, Move R, Move R, Move L, Del] "ACG" 0 @?= ["AG", "CT"]
        runEnzyme [Copy True, Move R, Move R, Move L, Copy False, Del] "ACG" 0 @?= ["A", "G", "CGT"]

    wTestCase "runEnzyme switch" $ do
        runEnzyme [Switch] "AAAGGGGTTATCCCC" 0 @?= ["AAAGGGGTTATCCCC"]
        runEnzyme [Switch, Ins T] "ACG" 0 @?= ["ACG"]
        runEnzyme [Switch, Switch, Ins T] "ACG" 0 @?= ["ACG"]
        runEnzyme [Copy True, Copy False, Switch, Switch, Ins G] "ACG" 0 @?= ["AGCG", "T"]
        runEnzyme [Copy True, Copy False, Switch, Move L, Switch, Ins G] "ACG" 0 @?= ["T", "ACG"]
        runEnzyme [Copy True, Copy False, Switch, Ins C, Move L, Switch, Ins G] "AAA" 0 @?= ["AGAA", "TC"]
        runEnzyme [Copy True, Switch, Ins C, Move L, Copy False, Switch, Ins G] "AAA" 0 @?= ["GAGAA", "TC"]
        runEnzyme [Copy True, Move R, Copy False, Switch, Ins A] "ACG" 0 @?= ["GAT", "A", "CG"]

    wTestCase "runEnzyme cut" $ do
        runEnzyme [Cut] "ACGT" 0 @?= ["CGT", "A"]
        runEnzyme [Cut] "ACGT" 1 @?= ["GT", "AC"]
        runEnzyme [Cut] "ACGT" 3 @?= ["ACGT"]
        runEnzyme [Copy True, Move R, Move R, Move L, Cut] "ACGT" 0 @?= ["GT", "C", "AC", "GT"]

    wTestCase "runEnzyme search" $ do
        runEnzyme [Search R Py, Ins T] "ACGT" 0 @?= ["ACTGT"]
        runEnzyme [Search R Pu, Ins T] "ACGT" 0 @?= ["ACGTT"]
        runEnzyme [Search R Pu, Search R Pu, Ins T] "ACGT" 0 @?= ["ACGT"]

    wTestCase "enzyme examples" $ do
        runEnzyme [Del, Move R, Ins T] "ACA" 0 @?= ["CAT"]
        runEnzyme enzyme3 "CAAAGAGAATCCTCTTTGAT" 1 @?= ["AT", "CAAAGAGAATCCTCTTTG", "CAAAGAGGA"]
        runEnzyme enzyme4 "TAGATCCAGTCCATCGA" 8 @?= ["ATG", strand1]


    -- -- bruteforce assuming simplifying assumptions does not work
    -- tell1 $ SC.testProperty "self-replicator" $
    --     changeDepth (+3) $
    --     exists $ \(strand :: Strand) ->
    --         let enzymes = synthesize strand
    --          in enzymes /= []
    --             && runEnzyme (head enzymes) strand 0 == [strand, strand]

    -- hypothesis: use a self-complementary strand
    let mkComplementary base = base ++ reverse (map baseComplement base)

    -- -- bruteforce assuming self-complementary does not work
    -- tell1 $ SC.testProperty "self-replicator" $
    --     changeDepth (+3) $
    --     exists $ \(base :: Strand) ->
    --         let strand = mkComplementary base
    --             enzymes = synthesize strand
    --          in enzymes /= []
    --             && runEnzyme (head enzymes) strand 0 == [strand, strand]

    -- hypothesis: use a self-complementary strand that, as an enzyme,
    --     goes into copy mode and travels right as far as possible
    let mkReplicator :: [Structure] -> Strand
        mkReplicator pypu =
            let pypuDNA = concatMap (\case Pu -> "TC"; Py -> "TA") pypu
             in mkComplementary $ "CG" ++ pypuDNA

    -- -- brute-force works !
    -- tell1 $ SC.testProperty "self-replicator" $
    --     forAll $ \(pypu :: [Structure]) ->
    --         let strand = mkReplicator pypu
    --             enzyme = head $ synthesize strand
    --          in runEnzyme enzyme strand 0 /= [strand, strand]

    wTestCase "self-replicator" $ do
        let -- pypu = [Pu,Pu,Pu,Py]
            -- strand = mkReplicator pypu
            strand = "CGTCTCTCTATAGAGAGACG"
            enzyme = head $ synthesize strand
        strand @?= reverse (map baseComplement strand)
        enzymeAffinity enzyme @?= C
        runEnzyme enzyme strand 0 @?= [strand, strand]

