module Converter where

import qualified Relation as Rel
import qualified Algebra as Alg
import qualified Natural as Nat


-- Haskell module generated by the BNF converter

import AbsMinSQL
import qualified AbsRelAlgebra as A
import qualified Relation as R
import PrintMinSQL
import ErrM

import qualified Data.Map as M
import Control.Monad


type SEnv = Alg.Env

initSEnv = Alg.Env M.empty

trueCond :: A.Cond
trueCond = A.CEq (A.EInt 0) (A.EInt 0) ---- trivially true condition

starIdent :: A.Ident
starIdent = A.Ident "*"  ---- aggregation over entire table

putRel :: A.Rel -> IO ()
putRel = putStrLn . Alg.prRel

failure x = error $ "not yet " ++ show x

transId :: Ident -> Rel.Id
transId = Alg.ident2id . transIdent

transIdent :: Ident -> A.Ident
transIdent (Ident x) = A.Ident x

transStr :: Str -> String
transStr x = case x of
  Str str  -> init (tail str) -- removing '

transScript :: SEnv -> Script -> IO SEnv
transScript env x = case x of
  SStm commands  -> foldM transCommand env commands

transCommand :: SEnv -> Command -> IO SEnv
transCommand env x = case x of
  CQuery table -> do
    let rel = transTable table
    mapM_ putStrLn ["## " ++ printTree x, "", Alg.prRel rel, "", Rel.prTable (Alg.evalRel env rel), Nat.queryRel rel, ""]
    return env
  CInsert id values  -> do
    let r = transId id
    case M.lookup r (Alg.tables env) of
      Just t -> do
        let t' = t {Rel.tdata = map (Alg.evalExp t []) (transVALUES values) : Rel.tdata t}
        return $ env {Alg.tables = M.insert r t' (Alg.tables env)}
      _ -> do
        putStrLn $ "unknown table " ++ r
        return env
  CCreateTable id typings  -> do
    let ltyps = map transTyping typings
    let newtable = Rel.initTable {
         Rel.tindex = M.fromList [(l,(i,t)) | ((l,t),i) <- zip ltyps [0..]],
         Rel.tlabels = map fst ltyps
         }
    return $ env {Alg.tables = M.insert (printTree id) newtable (Alg.tables env)}
  CDescribe id -> do
    let r = transId id
    case M.lookup r (Alg.tables env) of
      Just t -> do
        putStrLn $ R.prTable t
      _ -> do
        putStrLn $ "table " ++ r ++ " not found"
    return env
  CUpdate id settings where'  -> failure x
  CDelete star id where'  -> failure x
  CCreateDatabase id  -> failure x
  CAlterTable id alter  -> failure x
  CCreateView id query  -> failure x
  CCreateAssertion id condition  -> failure x

transQuery :: Query -> A.Rel
transQuery q = case q of
  QSelectWith defs query  -> failure q

  QSelect top_ distinct_ columns table where_ group_ having_ order_ -> 
    transDISTINCT distinct_ $ transORDER order_ $ rel
     where
       rel = grp $ transWHERE where_ $ transTable table
       grp r = case (condHAVING having_, group_, topaggrs) of
         ([],GNone,[]) -> transColumns columns $ r
         ([],_,_)      -> A.RGroup (map transExp2Ident topattrs) (map transAggrExp topaggrs) r
         ([c],_,_)     -> transColumns columns $
                            A.RSelect (transCondition c) $ 
                              A.RGroup (map transExp2Ident topattrs) (map transAggrExp topaggrs) r
         
       colexps = expsColumns columns
       
       -- have to collect these in \gamma
       topaggrs  = [e | e <- colexps, isAggrExp e]   -- aggregations in top level SELECT
       topattrs  = [e | e <- colexps, not (isAggrExp e)]
       haveaggrs = [e | c <- condHAVING having_, e <- condExps c]  -- aggregations in HAVING

       -- collecting from conditions ---
       condExps cond = case cond of
         COper x _ y -> expExps x  ++ expExps y
         CAnd c d    -> condExps c ++ condExps d
         COr c d     -> condExps c ++ condExps d
         _ -> error $ "not yet condExps " ++ show cond
       expExps exp = case exp of
         _ | isAggrExp exp -> [exp]
         _ -> [] ---- TODO: binary arithm ops
       isAggrExp exp = case exp of
         ENameAlias e i -> isAggrExp e
         EAggr _ _ _    -> True
         EAggrAll _ _   -> True
         _ -> False 

transColumns :: Columns -> A.Rel -> A.Rel
transColumns cs rel = case expsColumns cs of
  []    -> rel       -- select *
  exps  -> A.RProject (map transExp exps) rel 

expsColumns :: Columns -> [Exp]
expsColumns cs = case cs of
  CCAll      -> []
  CCExps cs  -> cs

transWHERE :: WHERE -> A.Rel -> A.Rel
transWHERE wh rel = case wh of
  WNone -> rel
  WCondition cond -> A.RSelect (transCondition cond) rel

transTable :: Table -> A.Rel
transTable t = case t of
  TName i               -> A.RTable (transIdent i)
  TNameAlias a u        -> A.RRename (A.RRelation (transIdent u)) (transTable a)
  TQuery q              -> transQuery q
  TProduct u v          -> A.RCartesian (transTable u) (transTable v)
  TUnion u all_ v       -> A.RUnion (transTable u) (transTable v)
  TIntersect u all_ v   -> A.RIntersect (transTable u) (transTable v)
  TExcept u all_ v      -> A.RExcept (transTable u) (transTable v)
  TJoin u v on          -> A.RThetaJoin (transTable u) (transON on) (transTable v)
  TNatJoin u v          -> A.RJoin (transTable u) (transTable v)
  TNatFullJoin table1 table2  -> failure t
  TLeftJoin table1 table2 on3  -> failure t
  TRightJoin table1 table2 on3  -> failure t

transExp :: Exp -> A.Exp
transExp x = case x of
  EName i        -> A.EIdent (transIdent i) 
  EQual i q      -> A.EIdent (A.Ident (Rel.qualify (transId i) (transId q)))
  ENameAlias i q -> A.EIdent (transIdent q) ---- as used in table heading, TODO get value
  EInt n         -> A.EInt n
  EStr s         -> A.EString (transStr s)
  EString str    -> A.EString str
  ENull  -> failure x
  EList exp exps  -> failure x
  EAggr op dist_ arg -> A.EAggr (transAggrOper op) (transExp2Ident arg)
  EAggrAll op dist -> A.EAggr (transAggrOper op) starIdent
  EDef  -> failure x
  EAny exp  -> failure x
  EAll exp  -> failure x
  EMul exp1 exp2  -> failure x
  EDiv exp1 exp2  -> failure x
  EAdd exp1 exp2  -> A.EAdd (transExp exp1) (transExp exp2)
  ESub exp1 exp2  -> failure x

transExp2Ident :: Exp -> A.Ident
transExp2Ident e = case transExp e of 
  A.EIdent i -> i
  _ -> error $ "expression cannot be used as ident: " ++ show e

transAggrExp :: Exp -> A.Aggregation
transAggrExp exp = case exp of
  ENameAlias (EAggr op dist_ arg) val -> A.AgFun (transAggrOper op) (transExp2Ident arg) (A.EIdent (transIdent val))
  EAggr op dist_ arg  -> A.AgFun (transAggrOper op) (transExp2Ident arg) (A.EIdent (A.Ident (printTree exp)))
----  EAggr op dist_ arg  -> A.AgFun (transAggrOper op) (transExp2Ident arg) (A.EAggr  (transAggrOper op) (transExp2Ident arg))
  ENameAlias (EAggrAll op dist_) val -> A.AgFun (transAggrOper op) starIdent (A.EIdent (transIdent val))
  EAggrAll op dist_ -> A.AgFun (transAggrOper op) starIdent (A.EIdent (A.Ident (printTree exp)))
  _ -> error $ "not aggregation expression " ++ show exp

transAggrOper :: AggrOper -> A.Function
transAggrOper op = case op of
  AOAvg   -> A.FAvg
  AOMax   -> A.FMax
  AOMin   -> A.FMin
  AOSum   -> A.FSum
  AOCount -> A.FCount

transON :: ON -> A.Cond
transON x = case x of
  OnNone  -> trueCond
  OnCondition condition  -> transCondition condition

transALL :: ALL -> A.Rel -> A.Rel
transALL x rel = case x of
  ANone  -> A.RDistinct rel
  AAll  -> rel

transDISTINCT :: DISTINCT -> A.Rel -> A.Rel
transDISTINCT x rel = case x of
  DNone  -> rel
  DDISTINCT  -> A.RDistinct rel

transTOP :: TOP -> A.Rel -> A.Rel
transTOP x rel = case x of
  TNone  -> rel
  TNumber n  -> failure x
  TPercent n  -> failure x

transGROUP :: GROUP -> [A.Ident]
transGROUP x = case x of
  GNone  -> []
  GGroupBy exps -> map transExp2Ident exps

condHAVING :: HAVING -> [Condition]
condHAVING x = case x of
  HNone  -> []
  HCondition condition -> [condition]

transORDER :: ORDER -> A.Rel -> A.Rel
transORDER ord rel  = case ord of
  ONone  -> rel
  OOrderBy ids desc_  -> A.RSort (map transExp2Ident ids) rel

---- TODO: rel algebra for descending
transDESC :: DESC -> ()
transDESC x = case x of
  DAsc  -> ()
  DDesc  -> ()

transVALUES :: VALUES -> [A.Exp]
transVALUES x = case x of
  VColVal ids exps  -> failure x
  VVal exps  -> map transExp exps
  VTable table  -> failure x
  VColTable id ids table  -> failure x


transSetting :: Setting -> ()
transSetting x = case x of
  SVal id exp  -> failure x

transSTAR :: STAR -> ()
transSTAR x = case x of
  StNone  -> failure x
  StStar  -> failure x

transCondition :: Condition -> A.Cond
transCondition x = case x of
  COper exp1 oper exp2  -> transOper oper (transExp exp1) (transExp exp2) 
  CAnd c1 c2  -> A.CAnd (transCondition c1) (transCondition c2)
  COr c1 c2  -> A.COr (transCondition c1) (transCondition c2)
  CNot c  -> A.CNot (transCondition c)
  CExists exp  -> failure x
  CIsNotNull exp  -> failure x
  CBetween exp1 exp2 exp3  -> failure x
  CNotBetween exp1 exp2 exp3  -> failure x

transOper :: Oper -> A.Exp -> A.Exp -> A.Cond
transOper x = case x of
  OEq  -> A.CEq
  ONeq -> A.CNEq
  OGt  -> A.CGt
  OLt  -> A.CLt
  OGeq -> failure x
  OLeq -> failure x
  OLike -> A.CLike
  ONotLike -> failure x
  OIn -> failure x
  ONotIn -> failure x

transTyping :: Typing -> (Rel.Id, Rel.Type)
transTyping x = case x of
  TColumn id type' constraints_ default_  -> (Alg.ident2id (transIdent id), transType type')
  TConstraint constraint ids  -> failure x
  TForeignKey exp id ids policys  -> failure x
  TReferences id1 id2 ids3 policys4  -> failure x
  TNamedConstraint id constraint  -> failure x

transType :: Type -> Rel.Type
transType x = case x of
  TIdent (Ident "INT") -> Rel.TInt 
  TIdent _ -> Rel.TString ----
  TSized id n  -> transType (TIdent id) ----

{-
transDEFAULT :: DEFAULT -> Result
transDEFAULT x = case x of
  DefNone  -> failure x
  DefExp exp  -> failure x

transConstraint :: Constraint -> Result
transConstraint x = case x of
  CNotNull  -> failure x
  CUnique  -> failure x
  CPrimaryKey  -> failure x
  CForeignKey exp id ids policys  -> failure x
  CReferences id ids policys  -> failure x
  CCheck condition  -> failure x
  CNamed id constraint  -> failure x

transPolicy :: Policy -> Result
transPolicy x = case x of
  PDelete action  -> failure x
  PUpdate action  -> failure x

transAction :: Action -> Result
transAction x = case x of
  ACascade  -> failure x
  ASetNull  -> failure x

transAlter :: Alter -> Result
transAlter x = case x of
  AAdd typing  -> failure x
  ADrop id  -> failure x
  AAlter id type'  -> failure x
  ADropPrimaryKey  -> failure x
  ADropConstraint id  -> failure x

-}
