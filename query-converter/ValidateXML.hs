{-# LANGUAGE GADTs #-}
module ValidateXML where

-- based on a Haskell module generated by the BNF converter

import AbsXML
import PrintXML
import ParXML
import LexXML

import ErrM

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

---- TODO: better printer in ToXML
printXML :: Tree a -> String
printXML = unspace . printTree where
  unspace s = case s of
    '<':' ':cs -> '<':unspace cs
    '<':'/':' ':cs -> '<':'/':unspace cs
    ' ':'>':cs -> '>':unspace cs
    ' ':'/':'>':cs -> '/':'>':unspace cs
    c      :cs -> c  :unspace cs
    _ -> s

getXML :: FilePath -> IO Document
getXML f = do
  s <- readFile f
  let ex = pDocument (myLexer s) >>= validateDocument
  case ex of
    Ok x -> return x
    Bad s -> putStrLn s >> return (DXML HNone DTDNone (EEmpty (ETEmpty (Ident "x") [])))

validateDocument :: Document -> Err Document
validateDocument d@(DXML header dtd element) = do
  syntaxCheckElement element
  case dtd of
    DTDNone -> return ()
    _ -> validate dtd element
  return d

syntaxCheckElement :: Element -> Err ()
syntaxCheckElement t = case t of
  ETag starttag elements endtag -> do
    case (starttag,endtag) of
      (STTag i _, ETTag j) | i /= j -> fail $ "syntax check error: element " ++ printXML starttag ++ " ends with " ++ printXML endtag
      _ -> mapM_ syntaxCheckElement elements
  _ -> return ()

validate :: DTD -> Element -> Err ()
validate (DTDDecl i definitions) el = do
  case typeElement el of
    j | j /= i -> fail $ "validation error: expected doctype " ++ printXML i ++ " but found " ++ printXML j
    _ -> return ()
  valEl el
 where
   valEl el = case typeElement el of
     t | t == pcdataIdent -> return ()
     t -> do
       rhs <- getRHS t
       case rhs of
         _ -> return ()
       {-
       REmpty -> 
       RPCData  -> failure t
       RIdent i -> failure t
       RStar rhs -> failure t
       RSPlus rhs -> failure t
       ROpt rhs -> failure t
       RSeq rhs0 rhs1 -> failure t
       RAlt rhs0 rhs1 -> failure t
       -}
   getRHS t = case lookup t [(i,rhs) | DElement i rhs <- definitions] of
     Just r -> return r
     _ -> fail $ "validation error: DTD has no element type " ++ printXML t
   
typeElement :: Element -> Ident
typeElement t = case t of
  ETag (STTag i _) _ _ -> i -- end tag already syntax checked
  EEmpty (ETEmpty i _) -> i
  EData _ -> pcdataIdent

pcdataIdent = Ident "#PCData"

transAttr :: Attr -> Result
transAttr t = case t of
  AValue i str -> failure t

transDTD :: DTD -> Result
transDTD t = case t of
  DTDNone  -> failure t
  DTDDecl i definitions -> failure t

transDefinition :: Definition -> Result
transDefinition t = case t of
  DElement i rhs -> failure t
  DAttlist i attributes -> failure t

transRHS :: RHS -> Result
transRHS t = case t of
  REmpty  -> failure t
  RPCData  -> failure t
  RIdent i -> failure t
  RStar rhs -> failure t
  RSPlus rhs -> failure t
  ROpt rhs -> failure t
  RSeq rhs0 rhs1 -> failure t
  RAlt rhs0 rhs1 -> failure t

transAttribute :: Attribute -> Result
transAttribute t = case t of
  AAttr i atype required -> failure t

transAType :: AType -> Result
transAType t = case t of
  ACData  -> failure t
  AId  -> failure t
  AIdRef  -> failure t

transRequired :: Required -> Result
transRequired t = case t of
  ReqRequired  -> failure t
  ReqImplied  -> failure t





transDocument :: Document -> Result
transDocument t = case t of
  DXML header dtd element -> failure t

transWord :: Word -> Result
transWord t = case t of
  WIdent i -> failure t
  WInt n -> failure t
  WFloat d -> failure t

transStartTag :: StartTag -> Result
transStartTag t = case t of
  STTag i attrs -> failure t

transEndTag :: EndTag -> Result
transEndTag t = case t of
  ETTag i -> failure t

transEmptyTag :: EmptyTag -> Result
transEmptyTag t = case t of
  ETEmpty i attrs -> failure t

transHeader :: Header -> Result
transHeader t = case t of
  HNone  -> failure t
  HVersion  -> failure t


transXPath :: XPath -> Result
transXPath t = case t of
  XPCont xaxis xitem xcond xpath -> failure t
  XPEnd  -> failure t

transXAxis :: XAxis -> Result
transXAxis t = case t of
  XAPlain  -> failure t
  XADesc  -> failure t

transXItem :: XItem -> Result
transXItem t = case t of
  XINone  -> failure t
  XIElem i -> failure t
  XIAttr i -> failure t
  XIAxis i0 i1 -> failure t
  XIAnces  -> failure t

transXCond :: XCond -> Result
transXCond t = case t of
  XCNone  -> failure t
  XCOp xexp0 xop1 xexp2 -> failure t

transXExp :: XExp -> Result
transXExp t = case t of
  XEPath xpath -> failure t
  XEIdent i -> failure t
  XEAttr i -> failure t
  XEInt n -> failure t
  XEStr str -> failure t

transXOp :: XOp -> Result
transXOp t = case t of
  XOEq  -> failure t
  XONEq  -> failure t

transIdent :: Ident -> Result
transIdent t = case t of
  Ident str -> failure t



transTree :: Tree c -> Result
transTree t = case t of
  DXML header dtd element -> failure t
  ETag starttag elements endtag -> failure t
  EEmpty emptytag -> failure t
  EData word -> failure t
  WIdent i -> failure t
  WInt n -> failure t
  WFloat d -> failure t
  STTag i attrs -> failure t
  ETTag i -> failure t
  ETEmpty i _ -> failure t
  AValue i str -> failure t
  HNone  -> failure t
  HVersion  -> failure t
  DTDNone  -> failure t
  DTDDecl i definitions -> failure t
  DElement i rhs -> failure t
  DAttlist i attributes -> failure t
  REmpty  -> failure t
  RPCData  -> failure t
  RIdent i -> failure t
  RStar rhs -> failure t
  RSPlus rhs -> failure t
  ROpt rhs -> failure t
  RSeq rhs0 rhs1 -> failure t
  RAlt rhs0 rhs1 -> failure t
  AAttr i atype required -> failure t
  ACData  -> failure t
  AId  -> failure t
  AIdRef  -> failure t
  ReqRequired  -> failure t
  ReqImplied  -> failure t
  XPCont xaxis xitem xcond xpath -> failure t
  XPEnd  -> failure t
  XAPlain  -> failure t
  XADesc  -> failure t
  XINone  -> failure t
  XIElem i -> failure t
  XIAttr i -> failure t
  XIAxis i0 i1 -> failure t
  XIAnces  -> failure t
  XCNone  -> failure t
  XCOp xexp0 xop1 xexp2 -> failure t
  XEPath xpath -> failure t
  XEIdent i -> failure t
  XEAttr i -> failure t
  XEInt n -> failure t
  XEStr str -> failure t
  XOEq  -> failure t
  XONEq  -> failure t
  Ident str -> failure t

