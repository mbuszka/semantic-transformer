{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Transform.Cfa where

import           Syntax                         ( Const(..)
                                                , Cons
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Foldable                  ( fold )
import           Data.Text.Prettyprint.Doc

data Expr a
  = Err String
  | App (Atom a) (NonEmpty (Atom a))
  | Match (Atom a) [Pattern a]
  | Let a (Expr a) (Expr a)
  | Atom (Atom a)
  deriving (Eq, Ord, Show)

data Atom a
  = Var a
  | Const Const
  | Lambda (NonEmpty a) (Expr a)
  | CApp Cons (NonEmpty (Atom a))
  deriving (Eq, Ord, Show)

data Pattern a
  = PatConst Const (Expr a)
  | PatCons Cons (NonEmpty a) (Expr a)
  | PatWild (Expr a)
  deriving (Eq, Ord, Show)

-- Context free abstract interp
data Val a
  = Closure (NonEmpty a) (Expr a)
  | Value
  deriving (Eq, Ord, Show)

data Kont a
  = KAddr (Addr a)
  | KNil
  deriving (Eq, Ord, Show)

data Bind a = Bind a (Expr a) (Kont a)
  deriving (Eq, Ord, Show)

newtype Store k v = MkStore { unStore :: Map k (Set v) }
  deriving (Eq, Ord, Show, Semigroup, Monoid)

type VStore a = Store a (Val a)

newtype Addr a = Addr (Expr a)
  deriving (Eq, Ord, Show)

type KStore a = Store (Addr a) (Bind a)

data Conf a = MkConf (Expr a) (Kont a)
  deriving (Eq, Ord, Show)

data State a = MkState (Set (Conf a)) (VStore a) (KStore a)
  deriving (Eq, Ord, Show)


expr :: Pattern a -> Expr a
expr (PatCons _ _ e) = e
expr (PatConst _ e ) = e
expr (PatWild e    ) = e

extend :: Ord k => Store k v -> k -> v -> Store k v
extend (MkStore s) k v = MkStore (s <> Map.singleton k (Set.singleton v))

singletonStore :: k -> v -> Store k v
singletonStore k v = MkStore $ Map.singleton k (Set.singleton v)

get :: Ord k => Store k v -> k -> [v]
get s k = Set.toList $ (unStore s) Map.! k

alloc :: Expr a -> Addr a
alloc = Addr

aval :: Ord a => Atom a -> VStore a -> [Val a]
aval (Var   x    ) store = get store x
aval (Const _    ) _     = [Value]
aval (Lambda xs e) _     = [Closure xs e]
aval (CApp   _  _) _     = [Value]

step
  :: Ord a => VStore a -> KStore a -> Conf a -> [(Conf a, VStore a, KStore a)]
step vstore kstore (MkConf exp k) = case exp of
  Let x exp' rest ->
    let kaddr   = alloc rest
        k'      = Bind x rest k
        kstore' = extend kstore kaddr k'
    in  [(MkConf rest (KAddr kaddr), vstore, kstore')]
  App f vs -> do
    Closure xs exp <- aval f vstore
    vs'            <- traverse (\a -> aval a vstore) vs
    let vstore' = vstore <> fold ((NE.zipWith singletonStore) xs vs')
    return (MkConf exp k, vstore', kstore)
  Match x ps -> do
    p <- ps
    case p of
      PatWild e      -> return (MkConf e k, vstore, kstore)
      PatConst _ e   -> return (MkConf e k, vstore, kstore)
      PatCons _ xs e -> do
        let vstore' = vstore <> foldMap (\x -> singletonStore x Value) xs
        return (MkConf e k, vstore', kstore)
  Err  _ -> []
  Atom a -> case k of
    KAddr addr -> do
      Bind x e k <- get kstore addr
      v          <- aval a vstore
      let vstore' = extend vstore x v
      return (MkConf e k, vstore', kstore)

drive :: Ord a => State a -> State a
drive s@(MkState cfs vstore kstore) =
  let (cfs', vs, ks) = unzip3 $ foldMap (step vstore kstore) cfs
      vstore'        = fold vs
      kstore'        = fold ks
      s'             = MkState (Set.fromList cfs') vstore' kstore'
  in  if s == s' then s else drive s'


prettyPat :: Pretty a => Pattern a -> Doc ann
prettyPat (PatConst c e) =
  pretty "|" <+> pretty c <+> pretty "->" <+> prettyExpr NoParens e
prettyPat (PatCons t xs e) =
  pretty "|"
    <+> pretty t
    <+> hsep (NE.toList . fmap pretty $ xs)
    <+> pretty "->"
    <+> prettyExpr NoParens e
prettyPat (PatWild e) =
  pretty "|" <+> pretty "_" <+> pretty "->" <+> prettyExpr NoParens e

prettyAtom :: Pretty a => Parenthesise -> Atom a -> Doc ann
prettyAtom _ (Var   v) = pretty v
prettyAtom _ (Const c) = pretty c
prettyAtom p (Lambda xs e) =
  parenLam p
    $   pretty "fun"
    <+> (hsep . fmap pretty . NE.toList) xs
    <+> pretty "->"
    <+> block (prettyExpr NoParens e)
prettyAtom p (CApp c xs) = parenApp p $ pretty c <+> (prettyApp xs)

prettyExpr :: Pretty a => Parenthesise -> Expr a -> Doc ann
prettyExpr p (Err e   ) = parenApp p $ pretty "err" <+> pretty (String e)
prettyExpr p (App f xs) = parenApp p $ prettyAtom ParenAll f <+> prettyApp xs
prettyExpr p (Match a ps) =
  parenLam p $ pretty "match" <+> prettyAtom NoParens a <> hardline <> vsep ps'
  where ps' = fmap prettyPat ps

prettyApp (x      :| []) = prettyAtom ParenApp x
prettyApp (x :| y :  ys) = prettyAtom ParenAll x <+> prettyApp (y :| ys)

data Parenthesise = NoParens | ParenApp | ParenAll

parenApp NoParens = id
parenApp _        = parens

parenLam ParenAll = parens
parenLam _        = id

block :: Doc ann -> Doc ann
block x = flatAlt x (nest 2 $ hardline <> x)
