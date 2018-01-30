--# -path=.:gflibsrc/chunk:gflibsrc/finnish/stemmed:gflibsrc/finnish:gflibsrc/api:gflibsrc/translator
-- you need a symlink gflibsrc to your GF/lib/src

concrete UDTranslateFin of UDTranslate =
  TranslateFin,
  VerbFin [UseCopula],
  ExtensionsFin [GenNP,UseQuantPN,ComplVPIVV],
  ExtendFin [VPI2,VPS2,ListVPI2,ListVPS2,MkVPS2,ConjVPS2,ComplVPS2,MkVPI2,ConjVPI2,ComplVPI2],
  UDDictionaryFin **
UDTranslateFunctor - [BackupQS,BackupClSlash, BackupQCl, StringCard, ExplPredSCVPS, PrepBackup, BackupVPI] with
  (Syntax = SyntaxFin),
  (Symbolic = SymbolicFin),
  (Extensions = ExtensionsFin)
  ** open ParadigmsFin, (R=ResFin), StemFin in {
lin
  BackupQS s b = {s = bracket b.s1 ++ s.s ++ bracket b.s2} ;
  BackupClSlash cl b = cl ** {s = \\t,a,p => bracket b.s1 ++ cl.s ! t ! a ! p ++ bracket b.s2} ;
  BackupQCl cl b = {s = \\t,a,p => bracket b.s1 ++ cl.s ! t ! a ! p ++ bracket b.s2} ;

  StringCard s = {s = \\_,_ => s.s ; n = plural} ; ---- mkCard s ;
  StringPN s = {s = \\_ => s.s ; h = R.Back} ;
  PrefixPN pn1 pn2 = {s = \\c => pn1.s ! c ++ pn2.s ! c ; h = pn2.h} ; ----- c

  SlashVPS t p np vps = ExtensionsFin.PredVPS np (ExtensionsFin.MkVPS t p vps) ** {c2 = vps.c2} ;
  AdvVPS vps adv = vps ** {s = \\a => vps.s ! a ++ adv.s} ;

  ExplPredSCVPS sc vps = Extensions.PredVPS (mkNP (mkPN [])) (AdvVPS vps (lin Adv sc)) ; ---- pn

  UttVPS vps = lin Utt {s = vps.s ! it_NP.a} ;

  --  : VPS -> S ;
  ImpersS vps = { s = vps.s ! R.agrP3 R.Sg } ;

  -- : VPS -> S ;
  GenericS vps = { s = vps.s ! R.agrP3 R.Sg } ;

  -- : VV -> NP -> S ;         -- ExistNP with VV: "there can be a cat"
  ModExistNP vv np = ModExistNPAdv vv np (ParadigmsFin.mkAdv []) ;

  -- : VV -> NP -> Adv -> S ;  -- ExistNPAdv with VV type: "there can be a cat on the hill"
  ModExistNPAdv vv np adv =
    let exist : VP = mkVP (mkComp (ParadigmsFin.mkAdv "olemassa")) ;
        canExistOnHill : VP = mkVP adv (mkVP vv exist) ;
     in mkS (mkCl np canExistOnHill) ; -- Literally "cat can exist on the hill"

  -- : VV -> NP -> RS -> S ;  -- variant of CleftNP: "it can be a long wait that results in nothing"
  ModCleftNP vv np rs =
    let beJohn : VP = mkVP (mkComp (mkNP np rs)) ;
        canBeJohn : VP = mkVP vv beJohn ;
     in mkS (mkCl it_NP canBeJohn) ;


  -- : VV -> Adv -> S -> S  ;  -- variant of CleftAdv: "it can be here she slept" -- "it seems to be here she slept"
  ModCleftAdv vv adv s = 
    let beHere : VP = StemFin.insertExtrapos ("kun" ++ s.s) (mkVP (mkComp adv));
        canBeHere : VP = mkVP vv beHere ;
     in mkS (mkCl it_NP canBeHere) ;
      

  -- : Utt -> Utt -> Utt ;
  MultiUtt utt1 utt2 = { s = utt1.s ++ utt2.s } ;

  -- : ListNP -> NP ;          -- "(Aarne Ranta) (Phone: abc) (University of Gothenburg)"
  LooseListNP = ConjNP (mkConj [] [] singular) ;

  -- : ListAdv -> Adv ;        -- same as above but for Advs
  LooseListAdv = ConjAdv (mkConj [] [] singular) ;

  -- : V2 -> ListNP -> ListNP -> VPS ;  -- "Mary went to Prague and John to London" -- go+to:V2 (Mary, John: ListNP) (Prague,London: ListNP) -- different sizes of ListNP? 
--ElidedV2 v2 nps1 nps2 = TODO


}

