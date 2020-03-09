{-# LANGUAGE TupleSections #-}

module Cfa where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Syntax (Label, Var)

data Value = Closure Env Var Label deriving (Eq, Ord)

data Term = App Label Label | Abs Var Label | Var Var

type Env = Map Var Label

newtype ValuePtr = ValuePtr Label deriving (Eq, Ord)

newtype ContPtr = ContPtr Label deriving (Eq, Ord)

data Cont
  = EvalArg Label Env Label ContPtr
  | EvalApp Label Value ContPtr
  | Halt
  deriving (Eq, Ord)

data Store
  = Store
      { _values :: Map ValuePtr (Set Value),
        _conts :: Map ContPtr (Set Cont)
      }

data Static
  = Static
      {_terms :: Map Label Term}

$(makeLenses ''Store)

$(makeLenses ''Static)

data Config
  = Eval Env Label ContPtr
  | Continue ValuePtr ContPtr

type Res = (Store, Config)

-- step :: (Label -> Term) -> Store -> Config -> (Store, [Config])
-- step term store conf =

data TermNotFound = TermNotFound deriving (Show)

instance Exception TermNotFound

term :: MonadReader Static m => Label -> m Term
term lbl = asks (view $ terms . at lbl) >>= \case
  Nothing -> bug TermNotFound
  Just t -> pure t

derefK :: Label -> Store -> [Cont]
derefK p s = toList (views conts (Map.! p) s)

derefV :: Label -> Store -> [Value]
derefV p s = toList (views values (Map.! p) s)

copy :: Label -> Label -> Store -> Store
copy from to store =
  let vs = store & views values (Map.! from)
   in store & values %~ (Map.insertWith (<>) to vs)

insertK :: Label -> Cont -> Store -> Store
insertK lbl k s = s & conts %~ Map.insertWith (<>) lbl (Set.singleton k)

insertV :: Label -> Value -> Store -> Store
insertV lbl v s = insertVs lbl (Set.singleton v) s

insertVs :: Label -> Set Value -> Store -> Store
insertVs lbl vs s = s & values %~ Map.insertWith (<>) lbl vs

eval :: MonadReader Static m => Env -> Store -> Label -> Label -> m [Res]
eval env store lbl k = term lbl >>= \case
  App f e ->
    let store' = insertK f (EvalArg env e k) store
     in pure [(store', Eval env f f)]
  Abs x body ->
    let store' = insertV lbl (Closure env x body) store
     in pure [(store', Continue lbl k)]
  Var x -> pure [(store, Continue (env Map.! x) k)]

continue :: MonadReader Static m => Store -> Label -> Label -> m [Res]
continue store v k =
  let store' = copy v k store
   in forM [(k, v) | k <- derefK k store', v <- derefV v store'] $ \case
        (EvalArg env lbl k, v) ->
          let store'' = insertK lbl (EvalApp v k) store'
           in pure (store'', Eval env lbl lbl)
        (EvalApp (Closure env x body) k, v) ->
          pure (store', Eval (Map.insert x v env) body k)
{- TODO:
  - values are bound to address coresponding to the expresion wich produced them
  - each expression along the way should re-bind the values to its address
  - this way store serves as a 'cache' for each expression in the program
  - continuations are bound to address corresponding to subexpression wich will
    be evaluated next
 -}


