-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
{-# OPTIONS_GHC -Wno-type-defaults -Wno-incomplete-uni-patterns -Wno-unused-imports #-}
module MicroHs.Desugar(
  desugar,
  LDef, showLDefs
  ) where
--import Debug.Trace
import Prelude
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.State.Strict as S --Xhiding(ap)
--Ximport Control.Monad as S hiding(ap)
--Ximport Compat
--Ximport GHC.Stack
--Ximport Debug.Trace

import MicroHs.Parse
import MicroHs.Exp
import MicroHs.TypeCheck

type LDef = (Ident, Exp)

desugar :: TModule [EDef] -> TModule [LDef]
desugar atm =
  case atm of
    TModule mn tys syns vals ds -> TModule mn tys syns vals (concatMap (dsDef mn) ds)

dsDef :: IdentModule -> EDef -> [LDef]
dsDef mn adef =
  case adef of
    Data _ cs ->
      let
        f i = "$f" ++ showInt i
        fs = [f i | (i, _) <- zip (enumFrom 0) cs]
        dsConstr i cts =
          case cts of
            (c, ts) ->
              let
                xs = ["$x" ++ showInt j | (j, _) <- zip (enumFrom 0) ts]
              in (qual mn c, lams xs $ lams fs $ apps (Var (f i)) (map Var xs))
      in  zipWith dsConstr (enumFrom 0) cs
    Type _ _ -> []
    Fcn f eqns -> [(f, dsEqns eqns)]
    Sign _ _ -> []
    Import _ -> []

oneAlt :: Expr -> EAlts
oneAlt e = EAlts [([], e)] []

dsBind :: EBind -> [LDef]
dsBind abind =
  case abind of
    BFcn f eqns -> [(f, dsEqns eqns)]
    BPat p e ->
      let
        v = newVar (allVarsBind abind)
        de = (v, dsExpr e)
        ds = [ (i, dsExpr (ECase (EVar v) [(p, oneAlt $ EVar i)])) | i <- patVars p ]
      in  de : ds

dsEqns :: [Eqn] -> Exp
dsEqns eqns =
  case eqns of
    Eqn aps _ : _ ->
      let
        vs = allVarsBind $ BFcn "" eqns
        xs = take (length aps) $ newVars vs
        ex = runS (vs ++ xs) (map Var xs) [(map dsPat ps, dsAlts alts, hasGuards alts) | Eqn ps alts <- eqns]
      in foldr Lam ex xs
    _ -> impossible

hasGuards :: EAlts -> Bool
hasGuards (EAlts [([], _)] _) = False
hasGuards _ = True

dsAlts :: EAlts -> (Exp -> Exp)
dsAlts (EAlts alts bs) = dsBinds bs . dsAltsL alts

dsAltsL :: [EAlt] -> (Exp -> Exp)
dsAltsL []                 dflt = dflt
dsAltsL [([], e)]             _ = dsExpr e  -- fast special case
dsAltsL ((ss, rhs) : alts) dflt =
  let
    erest = dsAltsL alts dflt
    x = newVar (allVarsExp erest)
  in eLet x erest (dsExpr $ dsAlt (EVar x) ss rhs)

dsAlt :: Expr -> [EStmt] -> Expr -> Expr
dsAlt _ [] rhs = rhs
dsAlt dflt (SBind p e : ss) rhs = ECase e [(p, EAlts [(ss, rhs)] []), (EVar dummyIdent, oneAlt dflt)]
dsAlt dflt (SThen e   : ss) rhs = EIf e (dsAlt dflt ss rhs) dflt
dsAlt dflt (SLet bs   : ss) rhs = ELet bs (dsAlt dflt ss rhs)

dsBinds :: [EBind] -> Exp -> Exp
dsBinds ads ret =
  case ads of
    [] -> ret
    d:ds ->
      let
        dsd = dsBind d
        de = dsBinds ds ret
        def ir a =
          case ir of
            (i, r) -> App (Lam i a) (App (Lit (LPrim "Y")) (Lam i r))
      in  foldr def de dsd

dsExpr :: Expr -> Exp
dsExpr aexpr =
  case aexpr of
    EVar i -> Var i
    EApp f a -> App (dsExpr f) (dsExpr a)
    ELam xs e -> dsLam xs e
    ELit (LChar c) -> Lit (LInt (ord c))
--    ELit (LStr cs) -> dsExpr $ EList $ map (ELit . LChar) cs
    ELit l -> Lit l
    ECase e as -> dsCase e as
-- For now, just sequential bindings; each recursive
    ELet ads e -> dsBinds ads (dsExpr e)
    EList es -> foldr (app2 cCons) cNil $ map dsExpr es
    ETuple es -> Lam "$f" $ foldl App (Var "$f") $ map dsExpr es
    EDo mn astmts ->
      case astmts of
        [] -> error "empty do"
        stmt : stmts ->
          case stmt of
            SBind p e ->
              if null stmts then error "do without final expression"
              else
--                case p of
--                  EVar v -> dsExpr $ EApp (EApp (EVar (mqual mn ">>=")) e) (ELam [v] $ EDo mn stmts)
--                  _ ->
                    let
                      nv = newVar (allVarsExpr aexpr)
                      body = ECase (EVar nv) [(p, oneAlt $ EDo mn stmts), (EVar dummyIdent, oneAlt $ eError "dopat")]
                      res = dsExpr $ EApp (EApp (EVar (mqual mn ">>=")) e) (ELam [EVar nv] body)
                    in res
                      
            SThen e ->
              if null stmts then
                dsExpr e
              else
                dsExpr $ EApp (EApp (EVar (mqual mn ">>")) e) (EDo mn stmts)
            SLet ds ->
              if null stmts then error "do without final expression" else
                dsExpr $ ELet ds (EDo mn stmts)

    ESectL e op ->
      App (dsExpr (EVar op)) (dsExpr e)
    ESectR op e ->
      app2 cFlip (dsExpr (EVar op)) (dsExpr e)
    EIf e1 e2 e3 ->
      app2 (dsExpr e1) (dsExpr e3) (dsExpr e2)
    ECompr e astmts ->
      case astmts of
        [] -> dsExpr (EList [e])
        stmt : stmts ->
          case stmt of
            SBind p b ->
              let
                nv = newVar (allVarsExpr aexpr)
                body = ECase (EVar nv) [(p, oneAlt $ ECompr e stmts), (EVar dummyIdent, oneAlt $ EList [])]
              in app2 (Var "Data.List.concatMap") (dsExpr (ELam [EVar nv] body)) (dsExpr b)
            SThen c ->
              dsExpr (EIf c (ECompr e stmts) (EList []))
            SLet ds ->
              dsExpr (ELet ds (ECompr e stmts))
    EAt _ _ -> undefined
    EUVar _ -> undefined
    ECon c ->
      if eqChar (head (conIdent c)) ',' then
        undefined  -- not implemented yet
      else
        Var (conIdent c)

dsLam :: [EPat] -> Expr -> Exp
dsLam ps e =
  let
    vs = allVarsExpr (ELam ps e)
    xs = take (length ps) (newVars vs)
    ex = runS (vs ++ xs) (map Var xs) [(map dsPat ps, dsAlts $ oneAlt e, False)]
  in foldr Lam ex xs

mqual :: Maybe Ident -> Ident -> Ident
mqual mqi i =
  case mqi of
    Just qi -> qual qi i
    Nothing -> i

-- Handle special syntax for lists and tuples
dsPat :: --XHasCallStack =>
         EPat -> EPat
dsPat ap =
  case ap of
    EVar _ -> ap
    ECon _ -> ap
    EApp f a -> EApp (dsPat f) (dsPat a)
    EList ps -> dsPat $ foldr (\ x xs -> EApp (EApp consCon x) xs) nilCon ps
    ETuple ps -> dsPat $ foldl EApp (tupleCon (length ps)) ps
    EAt i p -> EAt i (dsPat p)
    ELit _ -> ap
    _ -> impossible

consCon :: EPat
consCon =
  let
    n = "Data.List.[]"
    c = "Data.List.:"
  in ECon $ Con [(n, 0), (c, 2)] c

nilCon :: EPat
nilCon =
  let
    n = "Data.List.[]"
    c = "Data.List.:"
  in ECon $ Con [(n, 0), (c, 2)] n

tupleCon :: Int -> EPat
tupleCon n =
  let
    c = tupleConstr n
  in ECon $ Con [(c, n)] c

dummyIdent :: Ident
dummyIdent = "_"

eError :: String -> Expr
eError s = EApp (ELit (LPrim "error")) (ELit $ LStr s)

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

newVars :: [Ident] -> [Ident]
newVars is = deleteFirstsBy eqIdent [ "q" ++ showInt i | i <- enumFrom 1 ] is

newVar :: [Ident] -> Ident
newVar = head . newVars

showLDefs :: [LDef] -> String
showLDefs = unlines . map showLDef

showLDef :: LDef -> String
showLDef a =
  case a of
    (i, e) -> i ++ " = " ++ showExp e

----------------

dsCase :: Expr -> [ECaseArm] -> Exp
dsCase ae as =
  let
    r = runS (allVarsExpr (ECase ae as)) [dsExpr ae] [([dsPat p], dsAlts alts, hasGuards alts) | (p, alts) <- as]
  in --trace (showExp r) $
     r

type MState = [Ident]  -- supply of unused variables.

type M a = State MState a
type Arm = ([EPat], Exp -> Exp, Bool)  -- boolean indicates that the arm has guards
type Matrix = [Arm]

newIdents :: Int -> M [Ident]
newIdents n = S.do
  is <- get
  put (drop n is)
  S.return (take n is)

newIdent :: M Ident
newIdent = S.do
  is <- get
  put (tail is)
  S.return (head is)

runS :: [Ident] -> [Exp] -> Matrix -> Exp
runS used ss mtrx =
  --trace ("runS " ++ show (ss, mtrx)) $
  let
    supply = deleteFirstsBy eqIdent [ "x" ++ showInt i | i <- enumFrom 1 ] used
--    ds :: [Exp] -> [Exp] -> M Exp
    ds xs aes =
      case aes of
        []   -> --letBind (S.return eMatchErr) $ \ d ->
                dsMatrix eMatchErr (reverse xs) mtrx
        e:es -> letBind (S.return e) $ \ x -> ds (x:xs) es
  in S.evalState (ds [] ss) supply

data SPat = SPat Con [Ident]    -- simple pattern
  --Xderiving(Show, Eq)

-- Desugar a pattern matrix.
-- The input is a (usually identifier) vector e1, ..., en
-- and patterns matrix p11, ..., p1n   -> e1
--                     p21, ..., p2n
--                     pm1, ..., pmn   -> em
-- Note that the RHSs are of type Exp.
dsMatrix :: --XHasCallStack =>
            Exp -> [Exp] -> Matrix -> M Exp
dsMatrix dflt iis aarms =
 if null aarms then
   S.return dflt
 else
 case iis of
 [] -> let { (_, f, _) : _ = aarms } in S.return $ f dflt
 i:is -> S.do
  let
    (arms, darms, rarms) = splitArms aarms
    ndarms = map (\ (EVar x : ps, ed, g) -> (ps, substAlpha x i . ed, g) ) darms
--  traceM ("split " ++ show (arms, darms, rarms))
  letBind (dsMatrix dflt iis rarms) $ \ drest ->
    letBind (dsMatrix drest is ndarms) $ \ ndflt ->
     if null arms then
       S.return ndflt
     else S.do
      let
        idOf (p:_, _, _) = conIdent (pConOf p)
        idOf _ = impossible
        grps = groupEq (on eqIdent idOf) arms
        oneGroup grp = S.do
          let
            (pat:_, _, _) : _ = grp
            con = pConOf pat
          xs <- newIdents (conArity con)
          let
            one arg =
              case arg of
                (p : ps, e, g) ->
                  case p of
                    EAt a pp -> one (pp:ps, substAlpha a i . e, g)
                    _        -> (pArgs p ++ ps, e, g)
                _ -> impossible
          cexp <- dsMatrix ndflt (map Var xs ++ is) (map one grp)
          S.return (SPat con xs, cexp)
--      traceM $ "grps " ++ show grps
      narms <- S.mapM oneGroup grps
      S.return $ mkCase i narms ndflt

eMatchErr :: Exp
eMatchErr = dsExpr $ EApp (ELit (LPrim "error")) (ELit $ LStr "no match")

-- If the first expression isn't a variable, the use
-- a let binding and pass variable to f.
letBind :: M Exp -> (Exp -> M Exp) -> M Exp
letBind me f = S.do
  e <- me
  if cheap e then
    f e
   else S.do
    x <- newIdent
    r <- f (Var x)
    S.return $ eLet x e r

cheap :: Exp -> Bool
cheap ae =
  case ae of
    Var _ -> True
    Lit _ -> True
    App (Lit _) _ -> True
    _ -> False

-- Ugh, what a hack
isInt :: String -> Bool
isInt cs =
  case cs of
    c:ds ->
      isDigit c ||
      eqChar c '-' && case ds of { d:_ -> isDigit d; _ -> False }
    _ -> False

-- Could use Prim "==", but that misses out some optimizations
eEqInt :: Exp
eEqInt = Var "Data.Int.=="

mkCase :: Exp -> [(SPat, Exp)] -> Exp -> Exp
mkCase var pes dflt =
  --trace ("mkCase " ++ show pes) $
  case pes of
    (SPat (Con cs name) _, arhs) : _ ->
      -- A hack for Int pattern matching
      if isInt name then
        let
          cond = app2 eEqInt var (Lit (LInt (readInt name)))
        in app2 cond dflt arhs
      else
        let
          arm ck =
            let
              (c, k) = ck
              (vs, rhs) = head $ [ (xs, e) | (SPat (Con _ i) xs, e) <- pes, eqIdent c i ] ++
                                 [ (replicate k dummyIdent, dflt) ]
            in (SPat (Con cs c) vs, rhs)
        in  eCase var (map arm cs)
    _ -> impossible

eCase :: Exp -> [(SPat, Exp)] -> Exp
eCase e as =
  --trace ("eCase " ++ showExp e ++ "\n" ++
  --       unlines [ unwords (conIdent c : xs) ++ " -> " ++ showExp r | (SPat c xs, r) <- as ]) $
  apps e [lams xs r | (SPat _ xs, r) <- as ]

-- Split the matrix into segments so each first column has initially patterns -- followed by variables, followed by the rest.
splitArms :: Matrix -> (Matrix, Matrix, Matrix)
splitArms am =
  let
    isConPat (p:_, _, _) = not (isPVar p)
    isConPat _ = impossible
    isVarPat (p:_, _, g) = isPVar p && not g  -- only group variable patterns that cannot fail
    isVarPat _ = False
    (ps, nps) = span isConPat am
    (ds, rs)  = spanUntil isVarPat nps
  in (ps, ds, rs)

-- Change from x to y inside e.
-- XXX Doing it at runtime.
substAlpha :: Ident -> Exp -> Exp -> Exp
substAlpha x y e =
  if eqIdent x dummyIdent then
    e
  else
    substExp x y e

eLet :: Ident -> Exp -> Exp -> Exp
eLet i e b =
  if eqIdent i dummyIdent then
    b
  else
    case b of
      Var j | eqIdent i j -> e
      _ ->
        case filter (eqIdent i) (freeVars b) of
          []  -> b                -- no occurences, no need to bind
          [_] -> substExp i e b   -- single occurrence, substitute  XXX coule be worse if under lambda
          _   -> App (Lam i b) e  -- just use a beta redex

pConOf :: --XHasCallStack =>
          EPat -> Con
pConOf ap =
  case ap of
    ECon c -> c
    EAt _ p -> pConOf p
    EApp p _ -> pConOf p
    ELit (LInt i) -> let { n = showInt i } in Con [(n, 0)] n
    _ -> impossible

pArgs :: EPat -> [EPat]
pArgs ap =
  case ap of
    ECon _ -> []
    EApp f a -> pArgs f ++ [a]
    _ -> impossible

-- XXX quadratic
groupEq :: forall a . (a -> a -> Bool) -> [a] -> [[a]]
groupEq eq axs =
  case axs of
    [] -> []
    x:xs ->
      let
        (es, ns) = partition (eq x) xs
      in (x:es) : groupEq eq ns
