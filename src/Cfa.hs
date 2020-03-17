{-# LANGUAGE TupleSections #-}

module Cfa where

import Control.Lens
import Control.Applicative ((<|>), empty)
import Control.Monad (guard)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc
import MyPrelude
import Syntax
import Control.Monad.IO.Class (MonadIO)
import Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.NonDet

data Value
  = Closure Label Env [Var] Label
  | TopLevel Var
  | Struct Tag [ValuePtr]
  | Str
  deriving (Eq, Ord)

data Function
  = Top Var
  | Anon Label
  deriving (Eq, Ord)

type Env = Map Var ValuePtr

newtype ValuePtr = ValuePtr Label deriving (Eq, Ord)

newtype ContPtr = ContPtr Label deriving (Eq, Ord)

newtype CacheDst = CacheDst Label deriving (Eq, Ord)

data Cont
  = EvalApp Env [ValuePtr] [Label] CacheDst ContPtr
  | EvalCons Label Tag Env [ValuePtr] [Label] CacheDst ContPtr
  | EvalCase Env (Patterns Label) CacheDst ContPtr
  | Halt
  deriving (Eq, Ord)

data Store
  = Store
      { _values :: Map ValuePtr (Set Value),
        _conts :: Map ContPtr (Set Cont)
      }
  deriving (Eq, Ord)

data Static
  = Static
      { _terms :: Map Label (TermF Label),
        _topLevel :: Map Var (Scope Label)
      }

$(makeLenses ''Store)

$(makeLenses ''Static)

data Config
  = Eval Env Label ContPtr
  | Continue ValuePtr ContPtr
  deriving (Eq, Ord)

type Res = (Store, Config)

type Effs r = Members '[Reader Static, State Store, NonDet] r

denormalise :: Member (State Static) r => Term -> Sem r Label
denormalise t = do
  t' <- traverse denormalise (unTerm t)
  modify (terms %~ Map.insert (label t) t')
  pure $ label t

term :: Member (Reader Static) r => Label -> Sem r (TermF Label)
term lbl = asks (view $ terms . at lbl) >>= \case
  Nothing -> error "Term not found"
  Just t -> pure t

oneOf :: (Foldable t, Member NonDet r) => t a -> Sem r a
oneOf = foldr (<|>) empty . fmap pure . toList

derefK :: Effs r => ContPtr -> Sem r Cont
derefK p = do
  xs <- gets (view conts)
  oneOf (xs Map.! p)

derefV :: Effs r => ValuePtr -> Sem r Value
derefV p = do
  xs <- gets (view values)
  oneOf (xs Map.! p)

copy :: Member (State Store) r => ValuePtr -> CacheDst -> Sem r ()
copy src (CacheDst dst) = do
  vs <- gets $ views values (Map.! src)
  modify (values %~ Map.insertWith (<>) (ValuePtr dst) vs)

insertK :: Member (State Store) r => Label -> Cont -> Sem r ContPtr
insertK lbl k = do
  let ptr = ContPtr lbl
  modify $ conts %~ Map.insertWith (<>) ptr (Set.singleton k)
  pure ptr

insertV :: Member (State Store) r => Label -> Value -> Sem r ValuePtr
insertV lbl v = do
  let ptr = ValuePtr lbl
  modify $ values %~ Map.insertWith (<>) ptr (Set.singleton v)
  pure ptr

eval :: Effs r => Env -> Label -> ContPtr -> Sem r Config
eval env lbl k = term lbl >>= \case
  App f es -> do
    k' <- insertK f (EvalApp env [] es (CacheDst f) k)
    pure $ Eval env f k'
  Abs (Scope xs body) -> do
    ptr <- insertV lbl (Closure lbl env xs body)
    pure $ Continue ptr k
  Var x -> do
    ptr <- case env Map.!? x of
      Just v -> pure v
      Nothing -> asks (views topLevel (Map.member x)) >>= \b ->
        if b
          then insertV lbl (TopLevel x)
          else error $ "Unknown variable: " <> show (pretty x) <> " at " <> show (pretty lbl)
    pure $ Continue ptr k
  Cons (Record c []) -> do
    ptr <- insertV lbl (Struct c [])
    pure $ Continue ptr k
  Cons (Record c (e : es)) -> do
    k' <- insertK e (EvalCons lbl c env [] es (CacheDst e) k)
    pure $ Eval env e k'
  Case e ps -> do
    k' <- insertK e (EvalCase env ps (CacheDst e) k)
    pure $ Eval env e k'
  Error -> empty
  Let{} -> error "Not implemented yet"

continue :: Effs r => ValuePtr -> ContPtr -> Sem r Config
continue vPtr kPtr = do
  derefK kPtr >>= \case
    EvalApp env vs es dst k -> do
      copy vPtr dst
      case es of
        e : es' -> do
          k' <- insertK e $ EvalApp env (vPtr : vs) es' (CacheDst e) k
          pure $ Eval env e k'
        [] -> do
          let f : as = reverse (vPtr : vs)
          apply f as k
    Halt -> empty
    EvalCons lbl c env vs es dst k -> do
      copy vPtr dst
      case es of
        e : es' -> do
          k' <- insertK e $ EvalCons lbl c env (vPtr : vs) es' (CacheDst e) k
          pure $ Eval env e k'
        [] -> do
          cPtr <- insertV lbl $ Struct c (reverse $ vPtr : vs)
          pure $ Continue cPtr k
    EvalCase env ps dst k -> do
      copy vPtr dst
      applyCases env vPtr ps k

applyCases :: Effs r => Env -> ValuePtr -> Patterns Label -> ContPtr -> Sem r Config
applyCases environment vPtr (Patterns patterns) k =
  let aux env (PVar x : ps) (v : vs) = aux (Map.insert x v env) ps vs
      aux env (PWild : ps) (_ : vs) = aux env ps vs
      aux env (PCons (Record c ps') : ps) (v : vs) = derefV v >>= \case
        Struct c' vs' -> do
          guard (c' == c)
          guard (length ps' == length vs')
          aux env (ps' <> ps) (vs' <> vs)
        _ -> empty
      aux env [] [] = pure env
      aux _ _ _ = empty
   in do
        (p, Scope xs l) <- oneOf patterns
        env <- aux environment [insertNames p xs] [vPtr]
        pure $ Eval env l k

apply :: Effs r => ValuePtr -> [ValuePtr] -> ContPtr -> Sem r Config
apply f as k = derefV f >>= \case
  Closure _ env xs body ->
    pure $ Eval (Map.fromList (xs `zip` as) `Map.union` env) body k
  TopLevel x -> do
    Scope xs body <- asks (views topLevel (Map.! x))
    pure $ Eval (Map.fromList (xs `zip` as)) body k
  _ -> empty

step :: Static -> (Store, Set Config) -> (Store, Set Config)
step static (store, confs) = (s, Set.fromList cs)
  where
    (s, cs) = run . runReader static . runState store . runNonDet $ comp
    comp = oneOf confs >>= \case
      Eval env e k -> eval env e k
      Continue v k -> continue v k

analyse :: (MonadStx m, MonadIO m) => Program Term -> m (Map Label (Set Function))
analyse p =
  let (st, Program defs (DataDecl tpName records)) = run $ runState (Static Map.empty Map.empty) (traverse denormalise p)
      static = st & topLevel .~ ts
      ts = Map.fromList (map aux defs)
      aux (Def _ x s) = (x, s)
      Scope xs main = ts Map.! mkVar "main"
      go s = let s' = step static s in if s == s' then s else go s'
      build env rs = rs <&> \case Record n es -> Struct n (fmap (env Map.!) es)
   in do
        tpEnv <-
          Map.fromList
            <$> sequence
              [ (tpName,) . ValuePtr <$> freshLabel,
                ("string",) . ValuePtr <$> freshLabel
              ]
        pprint tpEnv
        let elems = Set.fromList $ build tpEnv records
            vStore =
              Map.fromList
                [ (tpEnv Map.! tpName, elems),
                  (tpEnv Map.! "string", Set.singleton Str)
                ]
            kStore = Map.singleton (ContPtr main) (Set.singleton Halt)
            store = Store vStore kStore
            env = Map.fromList (xs `zip` [tpEnv Map.! tpName])
            conf = Set.singleton (Eval env main (ContPtr main))
            (Store vals _, _) = go (store, conf)
            t (ValuePtr lbl, vs) = (lbl, Set.fromList . concatMap (\case
              TopLevel x -> [Top x]
              Closure l _ _ _ -> [Anon l]
              _ -> []) . Set.toList $ vs)
        pure . Map.fromList . map t . Map.toList $ vals


instance Pretty ValuePtr where
  pretty (ValuePtr lbl) = pretty lbl

instance Pretty ContPtr where
  pretty (ContPtr lbl) = pretty lbl

instance Pretty Value where
  pretty (Closure lbl _ _ _) = "closure" <+> pretty lbl
  pretty (TopLevel x) = pretty x
  pretty (Struct c ps) = "{" <+> pretty c <+> (hsep . map pretty) ps <+> "}"
  pretty Str = "string"

instance Pretty Cont where
  pretty (EvalApp _ vs es _ k) =
    group ("EvalApp(vs=" <+> sep (map pretty vs) <+> "es" <+> sep (map pretty es) <+> "k=" <+> pretty k <> ")")
  pretty Halt = "Halt"
  pretty (EvalCons _ _ _ _ _ _ k) = "(EvalCons" <+> pretty k <> ")"
  pretty (EvalCase _ _ _ k) = "(EvalCase" <+> pretty k <> ")"

instance Pretty Store where
  pretty (Store vs ks) = vsep [pretty vs, pretty ks]

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty = vsep . map pretty . Map.toList

instance Pretty a => Pretty (Set a) where
  pretty = sep . map pretty . toList

instance Pretty Function where
  pretty (Top x) = pretty x
  pretty (Anon l) = pretty l
