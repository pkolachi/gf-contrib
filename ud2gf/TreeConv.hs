module TreeConv where

import Data.List
import Data.Char
import qualified Data.Map.Strict as M

conll2deptree :: String -> DepTree
conll2deptree =
  depnodes2tree .
  map row2depnode .
  filter validDepNode .
  map words .
  filter ((/="#") . take 1) .
  lines

validDepNode ws = length ws > 7 && all isDigit (head ws)

data Tree n = T n [Tree n] deriving (Show,Eq,Ord)

root (T n _) = n
children (T _ ts) = ts

mapTree :: (n -> m) -> Tree n -> Tree m
mapTree f (T n ts) = T (f n) (map (mapTree f) ts)

sizeTree :: Tree n -> Int
sizeTree (T _ ts) = 1 + sum (map sizeTree ts)

type MorphoTags = [(String,String)]   -- String 
-- changed on 2017/09/01
-- also changed hasMorpho, hasLemma, hasTagandMorpho functions
-- switched from plain string to list of tuples

data DepNode = DN {
  source   :: Int,
  word     :: String,
  lemma    :: String,
  postag   :: POS,
  xpostag  :: POS,             -- added later. helpful for use in PrepocUD.hs
  morpho   :: MorphoTags,
  position :: Int,
  label    :: Label,
  extdeps  :: String,
  misc     :: String,          -- called in CoNLL-U as misc 
  status   :: String          
  } deriving (Show,Eq,Ord)

initDepNode :: DepNode
initDepNode = DN 0 "" "" "" "" [] 0 "" "" "" ""

type DepTree = Tree DepNode

-- Read the morph feature string and return 
-- MorphoTags / [MorphoConstraints]
morph2fs :: String -> MorphoTags
morph2fs morph = case break (=='|') morph of
  ("_","")      -> []
  (feat,"")     -> mf2mc feat : []
  (feat,_:rest) -> mf2mc feat : morph2fs rest
  where 
    mf2mc mf = case break (=='=') mf of
     (fn, '=':fv) -> (fn, fv)
     (fn, "")     -> (fn, "")

fs2morph :: MorphoTags -> String
fs2morph feats = case feats of 
  []      -> "_"
  (f,v):rest  -> concat . intersperse "|" $ map (\(x,y) -> x++"="++y) $ (f,v):rest

row2depnode :: [String] -> DepNode
row2depnode (positio:wor:lemm:posta:xpos:morph:sourc:labe:edep:mis:_) =
  initDepNode {word=wor,lemma=lemm,postag=posta, 
    position = read positio,
    xpostag = xpos,          -- fine-grained POS
    morpho = morph2fs morph,
    source = read sourc,
    label = labe,
    extdeps = edep,
    misc = mis            -- comments no longer ignored
  }

depnodes2tree :: [DepNode] -> DepTree
depnodes2tree dns = c2t root where 
  root = case [dn | dn <- dns, label dn == "root"] of
    [dn] -> dn
    _ -> error "no unique root"
  c2t dn = T dn (children dn)
  children dn = [c2t d | d <- dns, source d == position dn]

prDepTree :: DepTree -> String
prDepTree = prTree prDepNode

-- indented tree without parentheses
prTree :: (n -> String) -> Tree n -> String
prTree prn = unlines . prt 0 where
  prt i (T n ts) = (replicate i ' ' ++ prn n) : concatMap (prt (i + 4)) ts

-- unindented tree with parentheses
prTreeShort :: (n -> String) -> Tree n -> String
prTreeShort prn (T n ts) = unwords (prn n : map prt ts) where
  prt t@(T tn tns) = if (null tns) then (prn tn) else "(" ++ unwords (prn tn : map prt tns) ++ ")"

prDepNode :: DepNode -> String
prDepNode n = unwords [label n, lemma n, postag n, fs2morph (morpho n), show (position n), status n]

type Fun   = String
type Cat   = String
type Var   = String     -- used in DefMap
type Label = String
type POS   = String
type Lang  = String     -- should no more be needed except in lexicon lookup

data GFNode = GN {
  fun  :: Fun,          -- GF function
  src  :: Int           -- the source word, default 0; used for counting coverage
  } deriving (Show,Eq,Ord)

type GFTree = Tree GFNode

initGFNode = GN "NONE" 0

initGFTree = T initGFNode []

prGFTree = prTreeShort prGFNode

prGFTreeLong = unlines . prt 0 where
  prt i (T n ts)
    | all (null . children) ts =
        [replicate i ' ' ++ unwords (prGFNode n : map (prGFNode . root) ts)]    -- 1 line if all args are atomic
    | otherwise = (replicate i ' ' ++ prGFNode n) : concatMap (prt (i + 4)) ts

prGFNode n = fun n

-- rename [GFTree] as BackT (useful when adding backup trees for other gf tree candidates)
type BackT = [GFTree]

-- added backcands for each treecand  on 2017/05/03
data AbsNode = AN {
  gftree    :: GFTree,         -- the tree constructed; must always be a valid GF tree
  cat       :: Cat,            -- the type of this tree
  lab       :: Label,          -- original label, used in intermediate phases
  treecands :: [(GFTree,Cat)], -- local subtree candidates ---- used for lexical items only
  backtrees :: BackT,          -- collection of unused nodes for the top GF tree, if any 
  backcands :: [BackT],        -- collection of unused nodes for the rest of GF candidates
  tags      :: MorphoTags,     -- morphological tags
  udtag     :: POS,            -- UD tag. helpful for OOV word handling. Otherwise, not used
  lem       :: String,         -- lemma helpful for handling lexicalized configs 
  positio   :: Int             --- 0 for non-lexical functions
  } deriving (Show,Eq,Ord)

initAbsNode :: AbsNode
initAbsNode = AN initGFTree "String" "dep" [] [] [] [] "" "" 0   -- rename backup label to dep label to match gf2ud

type AbsTree = Tree AbsNode

-- add the actually 1-ranked tree to the other candidates
allTreecands an = (gftree an, cat an) : treecands an

-- retrive all the backups GF trees for the respective candidate trees
allBackcands an = backtrees an : backcands an

cnstf f i    = initGFNode {fun=f,src=i}
cnst f i     = T (cnstf f i) []

apps f xs    = T (initGFNode{fun=f}) xs
app0 f       = apps f []
app1 f a     = apps f [a]
app2 f a b   = apps f [a,b]
app3 f a b c = apps f [a,b,c]

prAbsTree :: AbsTree -> String
prAbsTree = prTree prAbsNode

prAbsNode :: AbsNode -> String
prAbsNode n = unwords [
  lab n,
  udtag n,
  prGFTree (gftree n) ++ " : " ++ cat n,
  prTreecands (treecands n),
  prBacktrees (backtrees n),
  prCoveredNodes (gftree n),   -- added 2017/04/26 
  show (positio n)
  ]

prBacktrees cs   = "{" ++ concat (intersperse ", " [prGFTree t | t <- cs]) ++ "}"

prCoveredNodes t = "(" ++ concat (intersperse ","  [show n | n <- nub $ nodesUsedGen src t]) ++ ")"

prTreecands cs = "[" ++ concat (intersperse ", " [prGFTree t ++ " : " ++ c | (t,c) <- cs]) ++ "]"

sortByNodesUsed    :: [AbsTree] -> [AbsTree]
sortByNodesUsed ts = map snd $ reverse $ sort [(length (nodesUsedGen positio t),t) | t <- ts]

-- depreceated. 
sortByNodesUnUsed  :: Int -> [AbsTree] -> [AbsTree]
sortByNodesUnUsed n ts = map snd $ sort [(n - (length (nodesUsedGen positio t)),t) | t <- ts]

-- sortByNodesCovered :: [GFTree] -> [GFTree]
-- sortByNodesCovered ts = map snd $ reverse $ sort [(length (nodesUsedGen src t),t) | t <- ts]

nodesUsedGen :: (n -> Int) -> Tree n -> [Int]
nodesUsedGen posi ast = nub (nus ast) where
  nus (T f ts) = (case posi f of 0 -> [] ; p -> [p]) ++ concatMap nus ts

-----------------------------------------------------
-- the first step from dep to abs: lexical annotation
-- this is the main algorithm in this module
-----------------------------------------------------

-- (1) change the datastructure, use quoted lemma as tree of type String 
dep2absNode :: DepNode -> AbsNode
dep2absNode d = initAbsNode {
  gftree = T initGFNode{fun = quote (lemma d), src = position d} [],
  lab = label d,
  tags = morpho d,
  udtag = postag d,
  lem = lemma d,
  positio = position d
  }

-- (2) annotate each node with candidate lexical items
-- produces just one tree, where lexical ambiguities are stored in candidates at each node
deptree2abstreeLex :: Configuration -> Dictionary -> DepTree -> AbsTree
deptree2abstreeLex config dict dt@(T dn dts) = T (annot dn) (map annots dts) where

  annots = deptree2abstreeLex config dict
  annot d = absnode0{gftree = gft, cat = gfc, treecands = tail cands} where
  
    absnode0 = dep2absNode d  -- decorate absnode0 from the original depnode d

    (gft,gfc) = head cands
  
    cands0 = lookLex (lemma d) (postag d) (position d) (morpho d)
    cands1 = cands0 ++ [(gftree absnode0, cat absnode0)]                       -- keep initial String as backup candidate
    cands2 = case filter ((/="String") . snd) cands1 of                        -- remove String candidates if there are others
               cs@(_:_) -> cs                                                  --- note: a String can sometimes be right,
               _ -> case coverOOV (head cands1) (postag d) (position d) (morpho d) of  --- e.g. when a word is taken literally or is missing from lexicon
                      cs@(_:_) -> cs
                      _ -> cands1 
               -- _ -> cands1                                                  --- e.g. when a word is taken literally as a name
    cands  = nub cands2

  udCatMap = categories config

  -- lexicon lookup for a lemma with certain POS and morphological tags                
  lookLex lem pos i mor = concat $ maybe [] (map (mkLex lem i)) $ do -- this "do" block returns Maybe [(Cat,[Fun])]
  
    cis <- M.lookup pos udCatMap                                               -- find categories compatible with POS, 
                                                                               -- with morpho and lemma constraints
    
    let cs = [catid ci |                                                       -- filter those categories that 
                                                                               -- match the morphological and lemma constraints
                ci <- cis,        
                and [hasMorpho id mc mor && hasLemma mc lem | ("head",mc) <- mconstraints ci]
             ]
    let lclem = map toLower lem 

    return [(c,fs) |                                                           -- for each such category, find the functions 
                                                                               -- with this lemma and category
      c <- cs, let fs = maybe [] id $ M.lookup (lclem,c) dict
      ]

  -- after lookup, build a set of GF trees
  mkLex lem i (c,fs) = case fs of
    _:_ -> [(cnst f i, c) | f <- fs]                                           -- GF function constant with its category 
    []  -> [(cnst (quote lem) i, "String")]                                    -- unknown words are kept as strings

  -- OOV handling. Added 2017/04/04
  coverOOV (gft,gfc) pos i mor = alltrees where
    oovFunctions = [fi | fi <- (functions config), length (argtypes fi) == 1, elem (gfc,"head") (argtypes fi)]
    oovtrees = [(tree,cv) | 
                     fi <- oovFunctions,
                     and [hasTagandMorpho id mo pos mor | ("head",mo) <- morphoconstraints fi],
                     let cv = valtype fi,
                     let fu = initGFNode{fun=funid fi, src=i},
                     let tree = T fu [gft]                                     -- apply the function to the String argument
               ]
    alltrees = oovtrees

--------------------------------------------------------------
---- configurations as data, read from a set of *.labels files
--------------------------------------------------------------

type CatMap = M.Map POS [CatInfo]
type DefMap = M.Map Fun ([Var],GFTree)

data Configuration = Conf {
  grammarname    :: String,  -- name of abstract syntax
  categories     :: CatMap,
  functions      :: [FunInfo],
  backups        :: [FunInfo],
  definitions    :: DefMap,
  helpcategories :: M.Map Cat Cat    -- added 2017/04/06 for handling sub-categories
  } deriving Show

data FunInfo = FI {
  funid    :: Fun,
  valtype  :: Cat,
  argtypes :: [(Cat,Label)],
  morphoconstraints :: [LMorphoConstraint]
  } deriving (Show,Eq,Ord)

data CatInfo = CI {
  catid        :: String,
  mconstraints :: [LMorphoConstraint]
  } deriving (Show,Eq,Ord)

---- switched from simple string to tuple
type MorphoConstraint  = (String,String) 
---- allows for constraints like case.lemma="of"
---- changed on 2017/10/09 
type LMorphoConstraint = (Label,MorphoConstraint) 

-- check if a morphological constraint is among the morpho tags
hasMorpho :: (n -> MorphoTags) -> MorphoConstraint -> n -> Bool
hasMorpho mf ("lemma",_) node = True 
hasMorpho mf c node = elem c (mf node)
--  | take 6 c == "lemma=" = True       -- this is a lemma constraint, not morpho
--  | otherwise = isInfixOf c (mf node) ---- use structure

-- a special case is constraint of form lemma=x
hasLemma :: MorphoConstraint -> String -> Bool
hasLemma (mf,mv) lemma = case mf of
  "lemma" -> lemma == mv
  _ -> True

-- added 2017/04/04
-- check if a morphological constraint is among the morpho tags
hasTagandMorpho :: (n -> MorphoTags) -> MorphoConstraint -> POS -> n -> Bool
hasTagandMorpho id mc pos node = case fst mc of 
  "udtag" -> snd mc == pos
  _ -> hasMorpho id mc node 
--  | take 6 c == "udtag=" = hasUDtag c pos
--  | otherwise = hasMorpho id c node 

initFunInfo = FI "" "" [] []
initCatInfo = CI "" [] 

depArgTypes :: FunInfo -> [(Cat,Label)]
depArgTypes fi = [(c,l) | (c,l) <- argtypes fi, l /= "head"]

prFunInfos fis = "["++ unwords (map funid fis) ++ "]"

lexFunInfo :: Fun -> Cat -> FunInfo
lexFunInfo f c = initFunInfo {funid = f, valtype = c}

-------------

type Dictionary = M.Map (String,Cat) [Fun]  -- (word,cat) -> funs
emptyDictionary = M.empty

-- added on 2017/08/29 to handle variants if provided in an auxiliary dictionary
mergeDictionary :: Dictionary -> Dictionary -> Dictionary
mergeDictionary init var = M.unionWith (++) init var 

-- added on 2017/11/07 to handle case-insensitive lookup
lcDictionary :: Dictionary -> Dictionary
lcDictionary dict = M.fromListWith (++) $ map (\ ((x,y),val) -> ((map toLower x,y),val) ) $ M.toList dict 
------------------------
---- auxiliaries

unlexBind :: String -> String
unlexBind s = case s of
  ' ':'&':'+':' ':cs -> unlexBind cs
  c:cs -> c:unlexBind cs
  _ -> s

quote s = "\"" ++ s ++ "\""  ---- TODO escape

-- only if length s >= 3. 
unquote s       | s == "\"" = s      -- if string itself is quotes 
unquote s@(q:w) | q == '\"' && last w == '\"' = init w 
unquote s@(q:w) | otherwise = s

