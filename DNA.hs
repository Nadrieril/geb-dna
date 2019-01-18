{-# LANGUAGE LambdaCase, OverloadedStrings, TypeSynonymInstances, FlexibleInstances,
    FlexibleContexts, ConstraintKinds #-}
module DNA where

import Data.String (IsString(..))
import Control.Arrow (first, second)
import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.RWS

import qualified Zipper as Z

data Base = A | C | G | T
    deriving (Show, Read, Eq)

data BaseType = Py | Pu
    deriving (Show, Eq)

baseType :: Base -> BaseType
baseType = \case
    A -> Pu
    G -> Pu
    C -> Py
    T -> Py

baseComplement :: Base -> Base
baseComplement = \case
    A -> T
    C -> G
    G -> C
    T -> A


data Direction = L | R
    deriving (Show, Eq)

data AminoAcid =
      Cut | Del | Switch
    | Move Direction | Copy Bool
    | Ins Base
    | Search Direction BaseType
    deriving (Show, Eq)

dupletToAA :: (Base, Base) -> Maybe AminoAcid
dupletToAA = \case
    (A, A) -> Nothing
    (A, C) -> Just Cut
    (A, G) -> Just Del
    (A, T) -> Just Switch
    (C, A) -> Just $ Move R
    (C, C) -> Just $ Move L
    (C, G) -> Just $ Copy True
    (C, T) -> Just $ Copy False
    (G, A) -> Just $ Ins A
    (G, C) -> Just $ Ins C
    (G, G) -> Just $ Ins G
    (G, T) -> Just $ Ins T
    (T, A) -> Just $ Search R Py
    (T, C) -> Just $ Search R Pu
    (T, G) -> Just $ Search L Py
    (T, T) -> Just $ Search L Pu


type Bend = Int -- 0=straight; 1=right; -1=left

aaBend :: AminoAcid -> Bend
aaBend = \case
    Cut -> 0
    Del -> 0
    Switch -> 1
    Move _ -> 0
    Copy True -> 1
    Copy False -> -1
    Ins A -> 0
    Ins C -> 1
    Ins G -> 1
    Ins T -> -1
    Search R Py -> 1
    Search _ _ -> -1


type Strand = [Base]
type Enzyme = [AminoAcid]

instance {-# OVERLAPPING  #-} IsString Strand where
    fromString = map (read . (:[]))

enzymeAffinity :: Enzyme -> Base
enzymeAffinity e = let
    totalBend = sum $ init $ tail $ map aaBend e
    in [A, G, T, C] !! (totalBend `mod` 4)

split :: [a] -> ([a], [a])
split = \case
    [] -> ([], [])
    (a:b:q) -> first (a:) $ second (b:) $ split q
    (_:q) -> split q

firstL :: (a -> a) -> [a] -> [a]
firstL f = \case
    [] -> []
    x:q -> (f x):q

collect_streaks :: [Maybe a] -> [[a]]
collect_streaks = \case
    Nothing:q -> [] : collect_streaks q
    (Just x):q -> firstL (x:) $ collect_streaks q
    [] -> [[]]

ribosome :: Strand -> [Enzyme]
ribosome strand = filter (not . null) $ collect_streaks $ map dupletToAA duplets
    where
        half_strands = split strand
        duplets = zip (fst half_strands) (snd half_strands)



data BasePos = BPTop | BPBot | BPBoth

flipBasePos :: BasePos -> BasePos
flipBasePos = \case
    BPTop -> BPBot
    BPBot -> BPTop
    BPBoth -> BPBoth

data DoubledBase = DoubledBase Base BasePos

flipDoubledBase :: DoubledBase -> DoubledBase
flipDoubledBase (DoubledBase b p) = DoubledBase (baseComplement b) (flipBasePos p)

baseDB :: Lens' DoubledBase Base
baseDB f (DoubledBase b p) = (flip DoubledBase p) <$> f b

posDB :: Lens' DoubledBase BasePos
posDB f (DoubledBase b p) = (DoubledBase b) <$> f p


type StrandState = (Bool, Z.Zipper DoubledBase)

copy :: Lens' StrandState Bool
copy = _1

strand :: Lens' StrandState (Z.Zipper DoubledBase)
strand = _2

type MonadStrand m = (MonadState StrandState m, MonadError () m)

-- initStrandState :: Strand -> StrandState
-- initStrandState = StSt [] . fmap (, True)

-- moveStSt :: Direction -> StrandState -> Maybe StrandState
-- moveStSt L

either1ToMaybe :: Either () a -> Maybe a
either1ToMaybe = either (const Nothing) Just

maybeToEither1 :: Maybe a -> Either () a
maybeToEither1 = maybe (Left ()) Right

liftMaybe :: MonadError () m => Maybe a -> m a
liftMaybe = liftEither . maybeToEither1

execAA :: MonadStrand m => AminoAcid -> m ()
execAA = \case
    Switch -> strand %= (Z.reverse . fmap flipDoubledBase)
    Move d -> do
        s <- use strand
        s' <- liftMaybe $
            case d of
                L -> Z.moveLeft s
                R -> Z.moveRight s
        strand .= s'
        c <- use copy
        when c $ strand . Z.focus . posDB .= BPBoth
    Copy b -> copy .= b
    Ins b -> do
        c <- use copy
        strand %= Z.insertR (DoubledBase b (if c then BPBoth else BPBot))
    Search d t -> do
        execAA $ Move d
        b <- use $ strand . Z.focus . baseDB
        when (baseType b /= t) $ execAA $ Search d t

runEnzyme :: Enzyme -> Strand -> Int -> Strand
runEnzyme enzyme strd i = fst $ (\m -> evalRWS m () initState) $ do
        runExceptT enzymeAction
        toListOf (Z.elements . baseDB) <$> use strand
    where
        initZipper = Z.mkZipper i $ fmap (flip DoubledBase BPBot) strd
        initState :: StrandState
        initState = (False, initZipper)
        enzymeAction :: ExceptT () (RWS () () StrandState) ()
        enzymeAction = forM_ enzyme execAA
