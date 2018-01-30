{-# LANGUAGE FlexibleContexts #-}

module Ranking where

import TreeConv
import qualified PGF as PGF 
import Data.Char
import Data.List
import qualified Data.Map.Strict as M 
import Data.Maybe (fromMaybe,catMaybes)
import Debug.Trace

-- the trace functions have been modified to accepted variable names as arguments 
tracePrS   varname v = v -- trace ("#" ++ varname ++ "\t" ++ show v) v
tracePrSub varname v = v -- trace ("\t" ++ "#--" ++ varname ++ "\t" ++ show v) v
tracePrL   varname v = v -- trace ("##" ++ varname ++ "\t" ++ show v ++ "\n\n") v
traceMsgPr mesg    v = v -- trace ("#" ++ mesg ++ " " ++ show v) v

{-
type SmProbTable  t = M.Map t Double 
type USmProbTable t = M.Map t (Maybe Double)
data ProbDist t = PD { 
    catprobs :: SmProbTable t,
    funprobs :: SmProbTable t
 } deriving (Show)
 this structure falls short when trying to re-estimate the prob distribution 
 for helper functions. It is better to maintain a list of functions 
 corresponding to each value type
-}

data ProbDist t = PD {
  catprobs :: M.Map t (Double, [t]),  -- this is a simplified structure to what is used in the Haskell runtime
                                      -- the list of functions for each value category are included, but not
                                      -- the function probs themselves
  funprobs :: M.Map t Double
} deriving (Show)

type GFFun   = Fun                    -- fun names used in the configs
type PGFFun  = PGF.CId                -- CIds found in the compiled PGF 
type GFDist  = ProbDist GFFun
type PGFDist = ProbDist PGFFun

-- if the original PGF has been compiled with probs file option
-- construct a Dist using those probabilities
-- The PGF API prevents from retrieving the actual distribution
-- embedded in a PGF
-- So, this is more or less a dummy function
readPGFProbs :: PGF.PGF -> PGFDist
readPGFProbs gram = readProbs gram basetable where
  probtable = PGF.defaultProbabilities gram
  basetable = lines $ PGF.showProbabilities probtable

-- construct a Table using probability distribution provided in an 
-- auxiliary file and the PGF. This allows for smoothing across functions
-- and categories where no prior prob value is specified. This ensures that
-- irrespective of the initial values, the probability distribution is 
-- always proper. 
-- The smoothing method 
readProbs :: PGF.PGF -> [String] -> PGFDist
readProbs gram ss = let
   distr  = M.fromList [(PGF.mkCId f,read p) | f:p:_ <- map words ss]
   cprobs = backoffSmooth [(c,M.lookup c distr) | 
                              c<-PGF.categories gram]                          -- smoothed distribution over cats
   fprobs = concatMap backoffSmooth [
               [(f,M.lookup f distr) | f<-PGF.functionsByCat gram cat] | 
                                       cat<-PGF.categories gram]
   ctable = M.fromList [(c,(p,PGF.functionsByCat gram c))  | 
                              (c,p)<-cprobs]
   ftable = M.fromList fprobs
 in PD ctable ftable

-- new functions are created using defs
-- we do not remove functions that are commented out by configs
-- this assigns non-trivial mass to functions that are never used in UD2GF
-- uncertain how to address this
mkProbabilities :: Configuration -> PGFDist -> GFDist
mkProbabilities config basep = let
   cprobs0 = [(PGF.showCId c, (p, map PGF.showCId funs)) | (c,(p,funs)) <- M.toList (catprobs basep)]
   fprobs0 = [(PGF.showCId f, p) | (f,p) <- M.toList (funprobs basep)]
   ctable0 = M.fromList cprobs0
   ftable0 = M.fromList fprobs0
   allfns  = [(c,                                                              -- categories in PGF
                  funs                                                         -- functions in PGF
               ++ [f | fid<-(functions config), 
                       let f=funid fid, M.member f (definitions config), 
                           valtype fid == c]                                   -- helper functions defined in configurations
              ) | (c,(p,funs)) <- cprobs0]
          ++ [(cat,                                                            -- sub-categories in configurations
                  [f | fid<-(functions config),                                
                       let f=funid fid, valtype fid == cat]                    -- helper functions to create sub-categories
              ) | cat <- map fst $ M.toList (helpcategories config)]
          ++ [(cat,                                                            -- syncategorematic words/cats
                  [f | fid<-(functions config), 
                       let f=funid fid, valtype fid == cat]                    -- should be empty
              ) | cinfo <- concatMap snd $ M.toList (categories config),
                  let cat=(catid cinfo), M.notMember cat ctable0]              -- syncategorematic words/categories

   helpfns = M.fromList $ catMaybes $ map (probExpr ftable0) $ M.toList (definitions config)  -- probs. of helper functions defined in configurations
   ftable1 = M.union ftable0 helpfns
   ctable1 = M.fromList $ backoffSmooth [(c,p) | c <- nub $ map fst allfns, let p=probCat ctable0 c]
   fprobs2 = concatMap backoffSmooth [[(f,M.lookup f ftable1) | f<-funs] | (c,funs) <- allfns]
   ctable2 = M.fromList [(c,(p,funs)) | (c,funs)<-allfns,let p=fromMaybe 0 $ M.lookup c ctable1]
   ftable2 = M.fromList fprobs2
 in PD ctable2 ftable2

  where 
    probExpr basep (fun,(vars,tree)) = case [f|f<-funsGFTree tree, notElem f vars] of
                                         [] -> Nothing
                                         fs -> Just (fun, product [fromMaybe 1 $ M.lookup f basep | f<-fs])

    probCat  basep cat = case M.lookup cat basep of
                           Just (p, fns) -> Just p 
                           Nothing       -> Nothing

-- using alpha as mixing parameter, calculate smooth distribution 
-- as p = alpha*p_seen + (1-alpha)*p_uniform
laplaceSmooth :: Ord t => [(t, Maybe Double)] -> [(t, Double)]
laplaceSmooth base = [(f,p/pmass) | (f,p) <- smoothd]  -- normalize the smoothed probs
  where
    alpha   = 0.95
    uniform = fromIntegral 1 / (fromIntegral $ length base) 
    smoothd = [(f,fromMaybe 0 mb_d*alpha + (1-alpha)*uniform) | (f,mb_d) <- base] 
    pmass   = sum $ map snd smoothd

-- distribute unseen probability masses uniformly across unseen items
-- not sure if this is the best smoothing scheme: but this is what
-- happens in the PGF runtime at the moment
backoffSmooth :: Ord t => [(t, Maybe Double)] -> [(t, Double)]
backoffSmooth base = [(f,d) | (f,Just d)<-base] ++ [(f,deftp) | (f,Nothing)<-base]
  where
    pmass   = sum [d | (f,Just d) <- base]
    deftp   = case length [f | (f,Nothing) <- base] of
                0   -> 0
                n   -> max 0 ((1-pmass)/fromIntegral n)

showTable :: (Show t, Ord t) => ProbDist t -> String ;
showTable table = unlines $ 
                    concat [[show f ++ "\t" ++ show p | f <- fns, let p=maybe 0.0 id $ M.lookup f (funprobs table)] | (_,(_,fns)) <- M.toList (catprobs table), length fns > 0]       -- probabilities of functions
                    ++ [show c ++ "\t" ++ show p | (c,(p,_)) <- M.toList (catprobs table)]   -- probabilities of categories

showTableShort :: (Show t, Ord t) => ProbDist t -> String ;
showTableShort table = unlines [unwords [show f ++ ":" ++ show p | f <- fns, let p=maybe 0.0 id $ M.lookup f (funprobs table)] | (_,(_,fns)) <- M.toList (catprobs table), length fns > 0]       -- probabilities of functions
                       ++ (unwords [show c ++ ":" ++ show p | (c,(p,_)) <- M.toList (catprobs table)])   -- probabilities of categories

funsTree :: (n -> a) -> Tree n -> [a]
funsTree nl gft = nus gft where
  nus (T f ts) = if null ts then [] else [nl f] ++ concatMap nus ts

funsGFTree :: GFTree  -> [GFFun]
funsGFTree = funsTree fun

funsPGFTree :: PGF.Tree -> [PGFFun]
funsPGFTree expr = case PGF.unApp expr of
                     Just (f, es) -> f:concatMap funsPGFTree es
                     _ -> case PGF.unStr expr of
                            Just s -> [PGF.mkCId s]
                            _ -> []

cfLogProbTree :: Ord a => ProbDist a -> (Tree n -> [a]) -> Tree n -> Double
cfLogProbTree prob funs tree = sum probValues where
  funsList   = funs tree
  unk        = 0       
  probValues = map (\x -> maybe unk log $ M.lookup x (funprobs prob)) funsList

cfProbTree    :: Ord a => ProbDist a -> (Tree n -> [a]) -> Tree n -> Double
cfProbTree prob funs tree = product probValues where
  funsList   = funs tree
  unk        = 1    -- probTree in Probabilistic.hs uses 1 for unknown items in Map. Why??
  probValues = map (\x -> maybe unk id $ M.lookup x (funprobs prob)) funsList

-- ranks trees in ascending order
-- metric takes a tree and returns an Ord type
rankTrees   :: Ord a => (Tree n -> a) -> [Tree n] -> [Tree n]
rankTrees metric cands = map snd rcands where
  rcands = sortBy (\ (m,_) (n,_) -> compare n m) ecands
  ecands = zip scores cands
  scores = map metric cands

-- metric function takes two strings as arguments and returns a double 
-- BLEU , F-measure , TER -- all of these are possible metrics
rankStrings :: Ord a => (String -> String -> a) -> (PGF.Tree -> String) -> String -> [PGF.Tree] -> [PGF.Tree]
rankStrings metric linfun reflin cands = map snd rcands where
  scorer = metric reflin
  scores = map scorer lins
  lins   = map linfun cands
  ecands = zip scores cands
  rcands = sortBy (\ (m,_) (n,_) -> compare n m) ecands

type LinString = String
type Sequence  = [LinString]
type Counter   = M.Map Sequence Int

collectCounts :: Int -> Sequence -> Counter 
collectCounts n seq = M.fromListWith (+) $ map (\item -> (item, 1)) (ngrams n padseq) where
  padseq = ["<-BOS->" | i<-[1..n-1]] ++ seq ++ ["<-EOS->" | i<-[1..n-1]]
  ngrams n seq = takeWhile ((==n) . length) $ (take n seq): ngrams n (tail seq)

precision_i :: Int -> Sequence -> Sequence -> Double
precision_i i refs hyps  =  let
  refcs  = collectCounts i refs
  hypcs  = collectCounts i hyps
  match  = M.foldl' (+) 0 $ M.intersection refcs hypcs                         -- another way of summing over a map
  hyplen = M.foldl' (+) 0 $ hypcs
  in (fromIntegral match)/(fromIntegral hyplen)

modprecision_i :: Int -> Sequence -> Sequence -> Double
modprecision_i i refs hyps = let
  refcs  = collectCounts i refs
  hypcs  = collectCounts i hyps
--  cliphypcs =    -- clip counts   -- TODO 
  match  = M.foldl' (+) 0 $ M.intersection refcs hypcs
  hyplen = M.foldl' (+) 0 $ hypcs
  in (fromIntegral match)/(fromIntegral hyplen)

recall_i :: Int -> Sequence -> Sequence -> Double
recall_i i refs hyps = let 
  refcs  = collectCounts i refs
  hypcs  = collectCounts i hyps
  match  = M.foldl' (+) 0 $ M.intersection refcs hypcs
  reflen = M.foldl' (+) 0 $ refcs
  in (fromIntegral match)/(fromIntegral reflen)

preprocessor :: LinString -> LinString -> (Sequence, Sequence)
preprocessor ref hyp = (reflc, hypunesc) where 
  reflc     = words $ map toLower ref
  hyplc     = words $ map toLower hyp
  hypclean  = filter (not . flip elem ["[","]"]) hyplc
  hypunesc  = map unquote hypclean

-- Implementation of Papineni et. al BLEU 
-- Cross-checked against multiple sources to verify correctness of stats
-- NLTK: 
-- Sacre-BLEU:  https://github.com/mjpost/sacreBLEU
bleuScore :: Int -> LinString -> LinString -> Double
bleuScore n refstr hypstr = bp * (exp $ sum [w*p | (w,p)<-zip weights modprecs])  
  where
    (ref,hyp) = preprocessor refstr hypstr
    weights   = map (\i -> (fromIntegral 1)/(fromIntegral n)::Double) [1..n]
    modprecs  = map (log . (\i -> modprecision_i i ref hyp))   [1..n]
    len       = (fromIntegral (length ref))/(fromIntegral (length hyp)) :: Double
    bp        = min 1.0 $ exp (1-len)

-- GLEU score: first reported in Google's NMT paper (Wu et. al, 2016)
-- https://arxiv.org/abs/1609.08144
-- because BLEU is bad when used on the sentence-level, the paper proposes 
-- GLEU for their experiments with Reinforcement Learning
-- defined as min(precision, recall) for all ngrams < n
-- Base implementation found in Python NLTK 3.2
-- http://www.nltk.org/_modules/nltk/translate/gleu_score.html#corpus_gleu
gleuScore :: Int -> LinString -> LinString -> Double
gleuScore n refstr hypstr = (fromIntegral tp)/(fromIntegral $ max tpfn tpfp)::Double 
  where
    (ref,hyp) = preprocessor refstr hypstr
    refcounts = M.unionsWith (+) $ map (\i -> collectCounts i ref) [1..n]
    hypcounts = M.unionsWith (+) $ map (\i -> collectCounts i hyp) [1..n]
    overlaps  = M.intersectionWith (min) refcounts hypcounts
    tp        = M.foldl' (+) 0 $ overlaps                                      -- True positives
    tpfn      = M.foldl' (+) 0 $ refcounts                                     -- True positives + False negatives
    tpfp      = M.foldl' (+) 0 $ hypcounts                                     -- True positives + False positives
