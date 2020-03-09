{-# LANGUAGE LambdaCase #-}

module Old.Analysis.ControlFlow where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc
import Old.Syntax.Anf
import Old.Syntax.Base
import Old.Syntax.Denormalized

data Conf = Conf ELabel Addr
  deriving (Eq, Ord, Show)

data Addr = Addr ELabel | Nil
  deriving (Eq, Ord, Show)

data Val = Closure (Scope ELabel) | Val
  deriving (Eq, Ord, Show)

data Kont = Kont (Scope ELabel) Addr
  deriving (Eq, Ord, Show)

newtype Store k v = Store (Map k (Set v))
  deriving (Eq, Ord, Show)

instance (Ord k, Ord v) => Semigroup (Store k v) where
  Store l <> Store r = Store (Map.unionWith (<>) l r)

instance (Ord k, Ord v) => Monoid (Store k v) where
  mempty = Store Map.empty

instance (Pretty k, Pretty v) => Pretty (Store k v) where
  pretty (Store s) = group . vsep $ f <$> Map.toList s
    where
      f (k, v) = pretty k <+> "->" <+> nest 2 (hsep $ pretty <$> Set.toList v)

type VStore = Store Var Val

type KStore = Store ELabel Kont

data MState = MState (Set Conf) VStore KStore
  deriving (Eq, Ord, Show)

push :: (Scope ELabel) -> Addr -> KStore -> (Addr, KStore)
push s@(Scope _ _ l) prev store = (Addr l, extend l (Kont s prev) store)

expr :: (Pattern ELabel) -> ELabel
expr (PatConstructor _ (Scope _ _ l)) = l
expr (PatConst _ l) = l
expr (PatWild l) = l

extend :: (Ord k, Ord v) => k -> v -> Store k v -> Store k v
extend k v (Store s) = Store $ Map.insertWith (<>) k (Set.singleton v) s

enter :: (Scope ELabel) -> [Val] -> VStore -> (ELabel, VStore)
enter (Scope l cnt b) vs store =
  let args = fmap (Local l) [0 .. cnt] `zip` vs
      store' = List.foldl' (\m -> \case (k, v) -> extend k v m) store args
   in (b, store')

getValues :: Ord k => k -> Store k v -> [v]
getValues k (Store s) = case s Map.!? k of
  Nothing -> []
  Just vs -> Set.toList vs

aval :: Atom ELabel -> VStore -> [Val]
aval (Var x) s = getValues x s
aval (Constant _) _ = [Val]
aval (Lambda s) _ = [Closure s]
aval (CApp _ _) _ = [Val]

vals :: [Val]
vals = Val : vals

step :: Denormalized -> VStore -> KStore -> Conf -> [(Conf, VStore, KStore)]
step p vs ks (Conf l k) = case getExpr l p of
  Let e b -> let (k', ks') = push b k ks in [(Conf e k', vs, ks')]
  App f xs -> do
    Closure s <- aval f vs
    xs' <- traverse (flip aval vs) xs
    let (e, vs') = enter s (NE.toList xs') vs
    return (Conf e k, vs', ks)
  Match a ps -> ps >>= \case
    PatWild l -> pure (Conf l k, vs, ks)
    PatConst _ l -> pure (Conf l k, vs, ks)
    PatConstructor _ sc -> do
      let (e, vs') = enter sc vals vs
      pure (Conf e k, vs', ks)
  Err _ -> []
  Atom a -> case k of
    Addr addr -> do
      Kont s k <- getValues addr ks
      v <- aval a vs
      let (e, vs') = enter s [v] vs
      pure (Conf e k, vs', ks)
    Nil -> []

drive :: Denormalized -> MState -> MState
drive p s@(MState cs vs ks) =
  let (cs', vss, kss) = unzip3 $ foldMap (step p vs ks) cs
      vs' = fold vss
      ks' = fold kss
      s' = MState (Set.fromList cs') vs' ks'
   in if s == s' then s else drive p s'

run :: Denormalized -> MState
run p@(Denormalized es ds) =
  let main = ds Map.! "main"
      vs = Set.singleton . Closure <$> Map.mapKeys Global ds
      (l, vs') = enter main vals (Store vs)
   in drive p $ MState (Set.singleton $ Conf l Nil) vs' mempty

closures :: MState -> Atom ELabel -> Set SLabel
closures (MState cs vs ks) a = foldMap f $ aval a vs
  where
    f Val = Set.empty
    f (Closure (Scope l _ _)) = Set.singleton l

appClosures :: Denormalized -> Map ELabel (Set SLabel)
appClosures p@(Denormalized es ds) =
  let s = run p
      f (App a _) = Just $ closures s a
      f _ = Nothing
   in Map.mapMaybe f es
