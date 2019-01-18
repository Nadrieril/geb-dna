{-# LANGUAGE LambdaCase, OverloadedStrings, TypeSynonymInstances, FlexibleInstances,
    FlexibleContexts, ConstraintKinds, MultiParamTypeClasses, DeriveGeneric, RankNTypes #-}
module DNA where

import Data.String (IsString(..))
import Control.Arrow (first, second)
import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Test.SmallCheck.Series
import GHC.Generics (Generic)

import qualified Zipper as Z


either1ToMaybe :: Either () a -> Maybe a
either1ToMaybe = either (const Nothing) Just

maybeToEither1 :: Maybe a -> Either () a
maybeToEither1 = maybe (Left ()) Right

liftMaybe :: MonadError () m => Maybe a -> m a
liftMaybe = liftEither . maybeToEither1

(%?=) :: (MonadState s m, MonadError () m) => Lens s s a b -> (a -> Maybe b) -> m ()
(%?=) l f = do
    x <- use $ getting l
    x' <- liftMaybe $ f x
    l .= x'

-- separate even-numbered elements from odd-numbered elements
unbraid :: [a] -> ([a], [a])
unbraid = \case
    [] -> ([], [])
    (a:b:q) -> first (a:) $ second (b:) $ unbraid q
    (_:q) -> unbraid q

collectStreaks :: [Maybe a] -> [[a]]
collectStreaks = filter (not . null) . aux
    where aux = \case
            Nothing:q -> [] : aux q
            (Just x):q -> over _head (x:) $ aux q
            [] -> [[]]

-- smallcheck
instance Monad m => Serial m Base where
instance Monad m => Serial m Structure where



data Base = A | C | G | T
    deriving (Show, Read, Eq, Generic)

baseComplement :: Base -> Base
baseComplement = \case
    A -> T
    C -> G
    G -> C
    T -> A


type Strand = [Base]

instance {-# OVERLAPPING  #-} IsString Strand where
    fromString = map (read . (:[]))


data Structure = Py | Pu
    deriving (Show, Eq, Generic)

structure :: Base -> Structure
structure = \case
    A -> Pu
    G -> Pu
    C -> Py
    T -> Py


data Direction = L | R
    deriving (Show, Eq)

data AminoAcid =
      Cut | Del | Switch
    | Move Direction | Copy Bool
    | Ins Base
    | Search Direction Structure
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
    (G, b) -> Just $ Ins b
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


type Enzyme = [AminoAcid]

enzymeAffinity :: Enzyme -> Base
enzymeAffinity e = let
    totalBend = sum $ init $ tail $ map aaBend e
    in [A, G, T, C] !! (totalBend `mod` 4)

synthesize :: Strand -> [Enzyme]
synthesize strand = collectStreaks $ map dupletToAA duplets
    where
        half_strands = unbraid strand
        duplets = zip (fst half_strands) (snd half_strands)


data Position = This | Other | Both
    deriving (Show, Eq)

flipPosition :: Position -> Position
flipPosition = \case
    Other -> This
    This -> Other
    Both -> Both


data DoubledBase = DoubledBase Base Position
    deriving (Show, Eq)

base :: Lens' DoubledBase Base
base f (DoubledBase b p) = (flip DoubledBase p) <$> f b

pos :: Lens' DoubledBase Position
pos f (DoubledBase b p) = (DoubledBase b) <$> f p

doubleBase :: Base -> DoubledBase
doubleBase = flip DoubledBase This

flipDoubledBase :: DoubledBase -> DoubledBase
flipDoubledBase (DoubledBase b p) = DoubledBase (baseComplement b) (flipPosition p)

extractDoubledBase :: DoubledBase -> (Maybe Base, Maybe Base)
extractDoubledBase (DoubledBase b p) =
    case p of
      This -> (Just b, Nothing)
      Other -> (Nothing, Just $ baseComplement b)
      Both -> (Just b, Just $ baseComplement b)


-- doubled strand + cursor into it
type StrandState = Z.Zipper DoubledBase

collectStrands :: StrandState -> [Strand]
collectStrands l = collectStreaks this ++ collectStreaks (reverse other)
    where (this, other) = unzip $ fmap extractDoubledBase $ Z.toList l

rotateStrandState :: StrandState -> StrandState
rotateStrandState = Z.reverse . fmap flipDoubledBase

initStrandState :: Strand -> Int -> StrandState
initStrandState s i = Z.fromList i $ fmap doubleBase s


-- (copy mode, strand state)
type EnzymeState = (Bool, StrandState)

initEnzymeState :: Strand -> Int -> EnzymeState
initEnzymeState s i = (False, initStrandState s i)


type MonadRunEnzyme m =
    ( MonadState EnzymeState m -- state of the strand(s) and copy mode
    , MonadWriter [Strand] m -- strands cut off along the way
    , MonadError () m -- allow aborting
    )

copy :: Lens' EnzymeState Bool
copy = _1

strand :: Lens' EnzymeState (Z.Zipper DoubledBase)
strand = _2

execAA :: MonadRunEnzyme m => AminoAcid -> m ()
execAA aa = do
    case aa of
        Copy b -> copy .= b
        Ins b -> strand %= Z.insertR (doubleBase b)
        Move L -> strand %?= Z.moveLeft
        Move R -> strand %?= Z.moveRight
        Switch -> strand %= rotateStrandState
        Cut -> do
            rem <- strand %%= Z.cutR
            case rem of
              Nothing -> return ()
              Just z -> tell $ collectStrands z
        Del -> do
            c <- use copy
            p <- use $ strand . Z.focus . pos
            if c || p == This
                then strand %?= Z.deleteR
                else do
                    strand . Z.focus . pos .= Other
                    execAA $ Move R
        Search d s -> do
            execAA $ Move d
            s' <- use $ strand . Z.focus . base . to structure
            when (s /= s') $ execAA $ Search d s
    ensureFocus
    ensureCopied

    where
        -- if there is no base under the cursor, abort
        ensureFocus = do
            p <- use $ strand . Z.focus . pos
            when (p == Other) $ throwError ()
        -- in copy mode, everywhere the cursor goes must be copied
        ensureCopied = do
            c <- use copy
            when c $ strand . Z.focus . pos .= Both


runEnzyme :: Enzyme -> Strand -> Int -> [Strand]
runEnzyme enzyme strd i = runwRWS initState $ do
        runExceptT enzymeAction
        s <- use strand
        tell $ collectStrands s
    where
        runwRWS :: s -> RWS () w s () -> w
        runwRWS s m = snd $ evalRWS m () s

        initState :: EnzymeState
        initState = initEnzymeState strd i

        enzymeAction :: ExceptT () (RWS () [Strand] EnzymeState) ()
        enzymeAction = forM_ enzyme execAA
