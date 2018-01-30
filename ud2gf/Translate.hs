module Translate where

import TreeConv
import PGF
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Char
import Data.Ord (comparing)
import Text.PrettyPrint (render)
import Debug.Trace

-- the trace functions have been modified to accepted variable names
-- as arguments 
tracePrS   varname v = v -- trace ("#" ++ varname ++ "\t" ++ show v) v
tracePrSub varname v = v -- trace ("\t" ++ "#--" ++ varname ++ "\t" ++ show v)
tracePrL   varname v = v -- trace ("##" ++ varname ++ "\t" ++ show v ++ "\n\n")
traceMsgPr mesg    v = v -- trace ("#" ++ m ++ " " ++ show v)

gftree2pgftree :: GFTree -> PGF.Tree
gftree2pgftree = gf2pgf
 where
  gf2pgf (T f ts) = mkFunApp (fun f) (map gf2pgf ts)
  mkFunApp s = case s of
      '"':_:_ | last s == '"' -> const (mkStr s)
      _ -> mkApp (mkCId s)

abstree2gftree :: Configuration -> AbsTree -> GFTree
abstree2gftree config at@(T an _) =
  wrapBackup config (cat an) (backtrees an) (gftree an)

abstree2gftrees :: Configuration -> AbsTree -> [GFTree]
-- because we want all candidate trees at the top to be 
-- returned after respective backups for that candidate
-- have been wrapped
-- previous function should become obsolete
abstree2gftrees config at@(T an _) = map (\((t,c),b) -> wrapBackup config c b t) (zip tcs bcs)  where 
   tcs = allTreecands an
   bcs = allBackcands an

-- annotatate with endo- and exocentric function applications
abstreeAnnotFuns :: Configuration -> Maybe Int -> AbsTree -> AbsTree
abstreeAnnotFuns config mbeam (T an ats) =
   let
     ats1 = map (annotTree) ats       -- bottom-up: annotate subtrees first
     an1  = annot an ats1             -- then build the tree in the top node
     an2  = applyBackup an1 ats1      -- then apply backup functions to gather the remaining children
     ats2 = ats1
   in T an2 ats2 
   -- T an2{gftree = wrapBackup config (cat an2) (backtrees an2) (gftree an2)} ats2  
   -- the commented part does not work. It can be removed. 

 where

  annotTree  = abstreeAnnotFuns config mbeam
  funApps    = funAppsOn config (TreeConv.functions config)
  backupApps = funAppsOn config (backups config)

  annot an ats =
    let
    
      funapps  = funApps an ats
      endofuns = sortBy (comparing proximity) [tcu | (tcu,True)  <- funapps]
      exofuns  = [tcu | (tcu,False) <- funapps]
      
    in case (endofuns,exofuns) of
    
      -- terminate if no functions apply
      ([], []) -> an {gftree = t, cat = c, treecands = finalcands}        
                    where
                      (t,c):finalcands = bestCands (allTreecands an)
                      
      -- try endocentric funtions if available
      (_:_, _) -> annot (an {gftree = t, cat=c, treecands = cands}) ats
                    where
                      (t,c):cands = bestCands $ endofuns ++ allTreecands an
                      
     -- if not, try exocentric functions
      (_, _:_) -> annot (an {gftree = t, cat=c, treecands = cands}) ats
                    where
                      (t,c):cands = bestCands $ exofuns ++ allTreecands an

  -- proximity of endofunction to head: distance of the closest modifier node --- add 100 to avoid empty list
  proximity (gft,cat) = minimum $ 100 : [d | n <- children gft, let d = abs (positio an - src (root n)), d /= 0]
  
  bestCands tcs = maybe tcs_ (\n -> take n tcs_) mbeam
    where 
      tcs_ = bestCands0 tcs

  -- original criteria used 
  bestCands0 tcs =                                                             
    let
      tls  = [((t,c),(length ns,sizeTree t)) |
               (t,c) <- tcs, let ns = nodesUsedGen src t]                      -- cands with coverage and size
      stls = sortBy (\ (_,m) (_,n) -> compare n m) tls                         -- descending order
    in map fst $ case stls of
        (_,(m,_)):_ -> takeWhile ((==m) . fst . snd) stls                      -- choose cands with maximal coverage ;
        _ -> stls                                                              -- size can differ if cat is different 

  -- this is now obsolete. an attempt to make sure exocentric functions are applied in best order
  -- added on 2017/03/16
  bestCands1 n tcs =
    let
      tls  = [((t,c), (length ns-n, length ns)) |
               (t,c) <- tcs, let ns = nub $ nodesUsedGen src t]                -- cands with coverage and size
      stls = sortBy (\ (_,(m,_)) (_,(n,_)) -> compare n m) tls                 -- descending order
    in map fst $ case stls of                                               
        (_,(m,_)):_ -> takeWhile ((==m) . fst . snd) stls                      -- choose cands with maximal coverage ;
        _ -> stls                                                              -- size can differ if cat is different                    
  
  bestCands2 tcs = 
    let
      tls  = [((t,c),(length ns,length nis)) |                -- moving sizeTree t from sorting criteria
               (t,c) <- tcs, let ns = nodesUsedGen src t,     -- cands with coverage and size
               let nis = nodesUsedGen src (ignoreBackups t)]  -- cands not covered by Backups (ignored for now)
      stls = sortBy (\ (_,m) (_,n) -> compare n m) tls        -- descending order
    in map fst $ case stls of                                             
        (_,(m,_)):_ -> takeWhile ((==m) . fst . snd) stls     -- choose cands with maximal coverage ;
        _ -> stls                                             -- size can differ if cat is different 

  {-
  -- now obsolete? (maybe) prefer to keep the abstreeAnnotFuns completely non-deterministic
  bestCands3 tcs = 
    let 
      probGFTree = R.cfProbTree prob R.funsGFTree 
      tls  = [((t,c),(length ns, probGFTree t)) | 
               (t,c) <- tcs, let ns = nodesUsedGen src t]
      stls = sortBy (\ (_,m) (_,n) -> compare n m) tls
    in map fst $ case stls of
        (_,(m,_)):_ -> takeWhile ((==m) . fst . snd) stls
        _ -> stls
  --} 

  bestCands4 tcs = 
    let
      tls   = tracePrS "stats" $ [((t,c), (length ns, length nis)) | 
                (t,c) <- tcs, let ns=nodesUsedGen src t,                       -- candidates with coverage and size
                   let nis=nodesUsedGen src (ignoreBackups t)]
      cs    = nub $ map snd tcs                                                -- categories of each candidate
      stls  = [sortBy (\ (_,m) (_,n) -> compare n m) 
                 [cand | cand@((_,c),_) <- tls, c == cat] | 
               cat <- cs]                                                      -- for each category, sort ind. candidates
      tcls  = concat [maxCover cands | cands <- stls]
    in map fst tcls 
    where
      maxCover stls = case stls of
        (_,(m,n)):_ -> takeWhile ((==(m,n)) . snd) stls
        _ -> stls 

  applyBackup0 an ats =                                                        -- original applyBackup function
    let
      gft     = gftree an
      useds   = nub $ nodesUsedGen src gft                                     -- list the children used in the actual gftree
      ats1    = [at | at <- ats, notElem (positio (root at)) useds]            -- list the children that are not used
      backs   = [t | at@(T n ts) <- ats1,
                   t <- take 1 $ sortBy                                        -- take the maximal backup to get the best lin
                     (\t u -> compare (sizeTree t) (sizeTree u))
                       [t | ((t,_),_) <- backupApps n ts]]
    in an{backtrees = backs}                                                   -- just store the backup at this point: 
                                                                               -- it may be applied to any candidate later

  -- added on 2017/03/15
  applyBackup1 an ats =
    let
      useds   = nub $ nodesUsedGen src (gftree an)                             -- list the children used in the actual gftree
      unuseds = [positio (root at) | at <- ats, notElem (positio (root at)) useds]
      ats1    = [at | at <- ats, elem (positio (root at)) unuseds]             -- list the children that are not used
      wrapB n = n {gftree=wrapBackup config (cat n) (backtrees n) (gftree n), cat=(cat n)}
      backs2  = [t |                                                           -- find one Backup for each of these children
                   at@(T n ts) <- ats1,                                        -- n can contain backtrees of its own 
                   t <- take 1 $ sortBy                                        -- take the maximal backup to get the best lin
                     (\t u -> compare (sizeTree t) (sizeTree u))
                     [t | 
                        let wrapn=wrapB n,                                     -- wrap n with its backtrees first 
                        ((t,_),_) <- backupApps wrapn ts]]
    in an{backtrees = backs2}                                                  -- just store the backup at this point: 
                                                                               -- it may be applied to any candidate later

  applyBackup an ats = 
    let
      useds gft       = nodesUsedGen src gft          -- list the children given the actual gftree
      unusedats gft   = [at | at <- ats,              -- list the children not used given the actual gftree
                           notElem (positio (root at)) (useds gft)]               
      backs gft       = [t  |                         -- find one Backup for each of these children
                           at@(T n ts) <- unusedats gft,
                           t <- take 1 $ sortBy       -- take the maximal backup to get the best lin
                              (\t u -> compare (sizeTree t) (sizeTree u))
                                        [t | ((t,_),_) <- backupApps n ts]]
      subbacks gft    = concat [(backtrees n) | at@(T n ts) <- unusedats gft]
      allbacks gft    = backs gft ++ subbacks gft

      alltrees        = [t | (t,c) <- allTreecands an]
      bcands          = map allbacks alltrees
    in
      an{backtrees = head bcands, backcands = tail bcands}                     -- just store the backup at this point: it may be applied to any candidate later


-- all possible function application result on the current tree, both endo- and exocentric
funAppsOn :: Configuration -> [FunInfo] -> AbsNode -> [AbsTree] -> [((GFTree,Cat),Bool)]
funAppsOn config funinfos an ats =
  [((tree,cv),isEndo)                                                          -- type of head, value type, if endo
      |
    let posan = positio an,                                                    -- note the position of the root

                                                                          -- 1. select the head tree candidate from the root
    (tt,ct) <- allTreecands an,                                                -- a candidate tree and its type given in the root
    let uds = nodesUsedGen src tt,                                             -- note which nodes have been used by this candidate
    
                                                                          -- 2. collect all candidates from the subtrees
    let givenArgs = [((c,lab rat),((tags rat, lem rat), (t,(c,back)))) |       -- category c of candidate t, from subtree with this label
                                                                               -- along with the morph. tags: changed 10/09/17
                         at    <- ats, let rat = root at,                      -- coming from any subtree
                         let treecands = allTreecands rat,
                         let backcands = allBackcands rat,
                         ((t,c),back) <- zip treecands backcands,              -- and any candidate in that tree
                         let subuds = nodesUsedGen src t,                      -- note which nodes have been used by this subtree
                         not (any (\n -> elem n uds) subuds)                   -- this subtree may not contain nodes already used
                         ],
    let allGivenArgs = ((ct,"head"),((tags an, lem an), (tt,(ct,[])))) : givenArgs,    -- add the head tree to the given args
                                                                               -- and its morph. tags: changed 10/09/17
                                                                               -- its backup will be added at the last step
                                                                               -- it can use the old tree in the same node as subtree
    
                                                                          -- 3. select a possible function to apply
    fi <- funinfos, (ca,"head") <- argtypes fi, ca == ct,                      -- function whose head cat matches the candidate
--    and [hasMorpho tags mo an | ("head",mo) <- morphoconstraints fi],        -- and the root satisfies its morpho constraints
-- morph. constraints are checked after arguments are found: changed 10/09/17
    let cv = valtype fi,                                                       -- its value type can be any cat cv
    let soughtArgs = argtypes fi,                                              -- arguments sought for the function

                                                                          -- 4. build a new tree to be added to candidates
    Just tbs <- [mapM (\x -> lookup x allGivenArgs) soughtArgs],               -- make sure you can find all sought arguments
    and [and [hasMorpho id mo tags && hasLemma mo lem |                        -- check morph constraints for label l
                 (l_,mo) <- morphoconstraints fi, l==l_
             ] |                                                               -- changed 10/09/17
            ((_,l),((tags,lem),_)) <- (zip soughtArgs tbs)],                   -- check if morph. constraints are satisfied for all arguments
    let fu = initGFNode{fun=funid fi, src=posan},                              -- mark the function with the current position
    let ts = [wrapBack c b t | (_,(t,(c,b))) <- tbs],                          -- apply the backups to the embedded arguments (except head),
    let tree = T fu ts,                                                        -- apply the function to the arguments found
    isNewCand tree,                                                            -- don't add the same tree again
    let isEndo = ca == cv                                                      -- endocentric iff the type of the tree doesn't change
  ]
 where
   isNewCand t = notElem t [u | (u,_) <- allTreecands an]
   wrapBack = wrapBackup config

wrapBackup :: Configuration -> Cat -> [GFTree] -> GFTree -> GFTree
wrapBackup config cat0 backs gt0 = case backs of
  [] -> gt
  _  -> T appw [gt, app2 "MkBackups" bu1 bu2]
 where
  gt   = gt0                                                                   -- just change the wrapper cat, 
                                                                               -- not yet the tree and its cat, 
                                                                               -- so that functions above can match
  --- otherwise: = cleanupGFTree config gt0
  cat  = maybe cat0 id $ M.lookup cat0 (helpcategories config)  -- cat0
  appw = initGFNode{fun = backupPrefix ++ cat, src = srct}                     ---- TODO: define backupPrefix in config ?

  sbs  = sort [(src (root b),b) | b <- backs]                                  -- sort by position
  srct = src (root gt0)
  bu1  = comb [b | (i,b) <- sbs, i <  srct]
  bu2  = comb [b | (i,b) <- sbs, i >= srct]                                    --- cannot be ===
  comb ts = case ts of
    [] -> app0 "BaseBackup"         ---- TODO: define Base,ConsBackup in config?
    t:tt -> app2 "ConsBackup" t (comb tt)


-- restore the tree without backup functions; used for coverage statistics
ignoreBackups :: GFTree -> GFTree
ignoreBackups t = case t of
  T f [b,c] | isWrapped f -> ignoreBackups b
  T f ts -> T f (map ignoreBackups ts)
 where
   isWrapped f = isPrefixOf backupPrefix (fun f)

---- dropSpine :: Int -> GFTree -> [GFTree]
---- dropSpine pos st@(T f ts) = [sts | sts<-ts, src (root sts) /= pos] ++ dropSpine 


--ignoreBackupHeads   :: GFTree -> [GFTree]
--ignoreBackupSplines :: GFTree -> [GFTree]

ignoreBackupSplines t = case t of
  T f [b,c] | isBackup f -> concatMap ignoreBackupSplines (dropHeadSpline b) ++ ignoreBackupSplines c
  T f ts -> concatMap ignoreBackupSplines ts
 where
   isBackup f = isPrefixOf backupPrefix (fun f)
   dropHeadSpline b = [t | t <- children b, src (root t) /= src (root b)] 

backupPrefix   = "Backup"     -- the function for wrapping something in backup: the wrapped tree is "interpreted"
mkBackupPrefix = "MkBackups"  -- the function for building a backup: all of this is "uninterpreted"


-- eliminate helper functions and unknown lexical functions
cleanupGFTree :: Configuration -> GFTree -> GFTree
cleanupGFTree config = unAux . unSyncat where
  unAux t@(T f ts) = case (fun f, map unAux ts) of
  
    (c,ts1) -> case M.lookup c defs of
      Just (xs,d) -> eval (zip xs ts1) d
      _ -> T f ts1

  defs = definitions config
  
  unSyncat t = case t of
    t -> t -----
   ---- (filter (not . syn . root) ts))

-- a call-by-value functional language interpreter
  eval env t@(T g us) =
    let us1 = map (eval env) us
    in case lookup (fun g) env of
         Just v -> v
         _ -> T g us1

prPGFTree :: PGF.Tree -> String
prPGFTree = showExpr []

prPGFTreeLong :: PGF.Tree -> String
prPGFTreeLong t = maybe (prPGFTree t) prGFTreeLong (mgf t) where
   mgf e = case PGF.unApp e of
     Just (f,es) -> fmap (apps (PGF.showCId f)) (mapM mgf es)
     _ -> case unStr e of
       Just s -> return $ app0 s
       _ -> error $ "prpgf " ++ prPGFTree e ----Nothing

prPGFTreeShort :: PGF.Tree -> String
prPGFTreeShort t = maybe (prPGFTree t) prGFTree (mgf t) where
   mgf e = case PGF.unApp e of
     Just (f,es) -> fmap (apps (PGF.showCId f)) (mapM mgf es)
     _ -> case unStr e of
       Just s -> return $ app0 s
       _ -> error $ "prpgf " ++ prPGFTree e ----Nothing

--- type checking is needed as long as the configurations file is not type-checked wrt the grammar
linTreeAllLang :: PGF -> PGF.Tree -> [String]
linTreeAllLang pgf t = case inferExpr pgf t of
  Left err  -> ["TYPE ERROR: " ++ render (ppTcError err)]
  Right (exp,_) -> map (\ (lang,l) -> "("++(show lang)++")" ++ " STRING TRANSLATION : " ++ unlexBind l) (linearizeAllLang pgf exp)

linTree :: PGF -> CId -> PGF.Tree -> String
linTree pgf lang t = case inferExpr pgf t of
  Left err  -> unwords ["TYPE ERROR: " ++ render (ppTcError err)]
  Right (exp,_) -> unlexBind $ linearize pgf lang exp

missingFunctions :: S.Set CId -> GFTree -> [Fun]
missingFunctions funs t = [f | f <- funsIn t, notString f, not (S.member (mkCId f) funs)] where
  funsIn (T f ts) = fun f : concatMap funsIn ts
  notString f = take 1 f /= "\""


