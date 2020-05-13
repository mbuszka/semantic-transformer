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
atom t = Atom <$> mkTerm' t

expr :: Effs r => TermF Term -> Sem r Anf
expr t = Expr <$> mkTerm' t

atomic :: Effs r => (Term -> Sem r Term) -> Anf -> Sem r Term
atomic k tm = case tm of
  Atom a -> k a
  Expr e -> do
    v <- freshVar "var"
    body <- k =<< mkTerm' (Var v)
    mkTerm' $ Let (LetAnnot {letGenerated = True}) (PVar v) e body

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
  case termF of
    Var {} -> k $ Atom Term {..}
    Abs a xs t -> do
      t' <- toAnf' t
      k $ Expr Term {termF = Abs a xs t', ..}
    App f ts ->
      toAnf f (atomic \f' -> seqAnf ts [] (\ts' -> k =<< expr (App f' ts')))
    Let a x t s ->
      toAnf t (\t' -> mkTerm termLoc =<< Let a x <$> (unwrap t') <*> toAnf s k)
    Cons v -> toAnfValue v k
    Case t cs ->
      toAnf t (atomic \t' -> k =<< expr . Case t' =<< traverse toAnf' cs)
    Error err -> mkTerm termLoc $ Error err

seqAnf :: Effs r => [Term] -> [Term] -> ([Term] -> Sem r Term) -> Sem r Term
seqAnf [] acc k = k (reverse acc)
seqAnf (t : ts) acc k = toAnf t (atomic \t' -> seqAnf ts (t' : acc) k)

transform :: Effs r => Program Term -> Sem r (Program Term)
transform = traverse toAnf'
