module Pipeline.Anf
  ( transform,
  )
where

import Syntax

data Anf = Atom Term | Expr Term

unwrap :: Anf -> Sem r Term
unwrap (Atom t) = pure t
unwrap (Expr t) = pure t

type Effs r = Members [FreshVar, FreshLabel] r

atom :: Effs r => TermF Term -> Sem r Anf
atom t = Atom <$> term t

expr :: Effs r => TermF Term -> Sem r Anf
expr t = Expr <$> term t

atomic :: Effs r => (Term -> Sem r Term) -> Anf -> Sem r Term
atomic k tm = case tm of
  Atom a -> k a
  Expr e -> do
    v <- freshVar "var"
    body <- k =<< term (Var v)
    term $ Let (LetAnnot {letGenerated = True}) v e body

toAnfValue :: Effs r => ValueF Term -> (Anf -> Sem r Term) -> Sem r Term
toAnfValue v k = case v of
  Number n -> k =<< (atom . Cons $ Number n)
  String t -> k =<< (expr . Cons $ String t)
  Boolean b -> k =<< (atom . Cons $ Boolean b)
  Record c ts -> seqAnf ts [] (k <=< expr . Cons . Record c)

toAnf' :: Effs r => Term -> Sem r Term
toAnf' t = toAnf t unwrap

toAnf :: Effs r => Term -> (Anf -> Sem r Term) -> Sem r Term
toAnf Term {..} k = do
  case termTerm of
    Var {} -> k $ Atom Term {..}
    Abs a s -> do
      t' <- traverse toAnf' s
      k $ Expr Term {termTerm = Abs a t', ..}
    App f ts ->
      toAnf f (atomic \f' -> seqAnf ts [] (\ts' -> k =<< expr (App f' ts')))
    Let a x t s ->
      toAnf t (\t' -> term =<< Let a x <$> (unwrap t') <*> toAnf s k)
    Cons v -> toAnfValue v k
    Case t cs ->
      toAnf t (atomic \t' -> k =<< expr . Case t' =<< traverse toAnf' cs)
    Panic -> term Panic

seqAnf :: Effs r => [Term] -> [Term] -> ([Term] -> Sem r Term) -> Sem r Term
seqAnf [] acc k = k (reverse acc)
seqAnf (t : ts) acc k = toAnf t (atomic \t' -> seqAnf ts (t' : acc) k)

transform :: Effs r => Program Term -> Sem r (Program Term)
transform = traverse toAnf'
