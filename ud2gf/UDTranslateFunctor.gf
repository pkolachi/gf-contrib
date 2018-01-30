incomplete concrete UDTranslateFunctor of UDTranslate =
  Translate,
  Verb [UseCopula],
  Extensions [GenNP,ComplVPIVV,UseQuantPN],
  UDDictionary
     ** open Syntax, Symbolic, Extensions, Prelude in {

lincat
  Top   = {s : Str} ;
  Punct = {s : Str} ;

lin
  
  TopPhr utt = utt ;
  TopPhrPunct utt punct = {s = utt.s ++ punct.s} ;
  
  StringPunct s = {s = s.s} ;
  StringCard s  = {s = \\_ => s.s ; n = plural} ;

  PrefixSymb s1 s2 = {s = s1.s ++ s2.s} ;
-- backup functions for unknown dependents

lincat
  Backups = {s1,s2 : Str} ;
  Backup = {s : Str} ;
  [Backup] = {s : Str} ;

  QVPS = Extensions.VPS ;

oper
  backupsAdv : Backups -> Adv = \b -> lin Adv {s = bracket b.s2} ;
  bracket : Str -> Str = \s -> "[" ++ s ++ "]" ;

lin
  MkBackups b1 b2 = {s1 = b1.s ; s2 = b2.s} ;
  BaseBackup = {s = []} ;
  ConsBackup b bs = {s = b.s ++ bs.s} ;


lin
  BackupAdV adv b = lin AdV {s = bracket b.s1 ++ adv.s ++ bracket b.s2} ;
  BackupAdv adv b = lin Adv {s = bracket b.s1 ++ adv.s ++ bracket b.s2} ;
  BackupAP  xp  b = mkAP (lin AdA {s = bracket b.s1}) (mkAP xp (backupsAdv b)) ;
  BackupNP  np  b = np ** {s = \\c => bracket b.s1 ++ np.s ! c ++ bracket b.s2} ;
  BackupVP  xp  b = mkVP (lin AdV {s = bracket b.s1}) (mkVP xp (backupsAdv b)) ; ---- AdV not always the right place
  BackupVPS vps b = vps ** {s = \\c => bracket b.s1 ++ vps.s ! c ++ bracket b.s2} ;
  BackupVPI xp  b = xp ** {s = \\c,a => bracket b.s1 ++ xp.s ! c ! a ++ bracket b.s2} ;
  BackupVPSlash xp b = xp ** mkVP (lin AdV {s = bracket b.s1}) (mkVP <xp : VP> (backupsAdv b)) ; ---- AdV not always the right place
  BackupV2V v b = v ** {s = \\c => bracket b.s1 ++ v.s ! c ++ bracket b.s2} ;
  BackupQCl cl b = {s = \\t,a,p,o => bracket b.s1 ++ cl.s ! t ! a ! p ! o ++ bracket b.s2} ;
  BackupCl cl b = {s = \\t,a,p,o => bracket b.s1 ++ cl.s ! t ! a ! p ! o ++ bracket b.s2} ;
  BackupClSlash cl b = cl ** {s = \\t,a,p,o => bracket b.s1 ++ cl.s ! t ! a ! p ! o ++ bracket b.s2} ;
  BackupS s b = {s = bracket b.s1 ++ s.s ++ bracket b.s2} ;
  BackupQS s b = {s = \\o => bracket b.s1 ++ s.s ! o ++ bracket b.s2} ;
  BackupSC sc b = lin Utt {s = bracket b.s1 ++ sc.s ++ bracket b.s2} ;
  BackupSubj adv b = lin Subj {s = bracket b.s1 ++ adv.s ++ bracket b.s2} ;
  BackupUtt utt b = lin Utt {s = bracket b.s1 ++ utt.s ++ bracket b.s2} ;
  BackupPhr utt b = lin Phr {s = bracket b.s1 ++ utt.s ++ bracket b.s2} ;
  BackupTop top b = lin Top {s = bracket b.s1 ++ top.s ++ bracket b.s2} ;

  AdvBackup adv = mkUtt adv ;
  APBackup  ap  = mkUtt ap ;
  NPBackup  np  = mkUtt np ;
  VPBackup  vp  = mkUtt vp ;
--  VPIBackup vpi = mkUtt <vpi.s : VP> ;
--  VPSBackup vps = {s = vp.s} ;
  VPSlashBackup vp = mkUtt <vp : VP> ;
  DetBackup det = mkUtt (mkNP det) ;
  QuantBackup q = mkUtt (mkNP (mkDet q singularNum)) ;
  InterjBackup i = mkUtt i ;
  OrdBackup ord = mkUtt (mkAP ord) ;
  PNBackup pn = mkUtt (mkNP pn) ;
  PrepBackup p = p ;
  PunctBackup p = p ;
  ConjBackup conj = mkUtt (mkNP conj (symb []) (symb [])) ; --- to get both discontinuous parts
  SymbBackup sy = sy ;
  SBackup s = mkUtt s ;
  QSBackup qs = mkUtt qs ;
  SCBackup sc = mkUtt sc ;
  SubjBackup subj = subj ;
  UttBackup utt = utt ;
  PhrBackup phr = phr ;
  TopBackup top = top ;



  these_Det = mkDet this_Quant pluralNum ;
  this_Det = mkDet this_Quant singularNum ;
  that_Det = mkDet that_Quant singularNum ;
  those_Det = mkDet that_Quant pluralNum ;
  these_NP = mkNP (mkDet this_Quant pluralNum) ;
  this_NP = mkNP (mkDet this_Quant singularNum) ;
  that_NP = mkNP (mkDet that_Quant singularNum) ;
  those_NP = mkNP (mkDet that_Quant pluralNum) ;
  which_Det = mkIDet which_IQuant ;
  no_Det = mkDet no_Quant ;
  all_Det = every_Det ; ---- used in translations, excluded in Eng



---- TODO: treat numbers with the Numeral grammar
lin

  one_Card = mkCard "1" ;
  two_Card = mkCard "2" ;
  three_Card = mkCard "3" ;
  four_Card = mkCard "4" ;
  five_Card = mkCard "5" ;
  ten_Card = mkCard "10" ;


  PredSCVPS sc vps = Extensions.PredVPS (symb (mkSymb sc.s)) vps ;
  ExplPredSCVPS sc vps = Extensions.PredVPS it_NP (AdvVPS vps (lin Adv sc)) ;

oper 
  createPunct : Str -> SS = \s -> ss s ;

lin
-- Punct
  dash_Punct = createPunct "-" ;
  ellipsis_Punct = createPunct "..." ;
  exclmark_Punct = createPunct "!" ; 
  colon_Punct = createPunct ":" ;
  semicolon_Punct = createPunct ";" ;
  lpar_Punct = createPunct "(" ;
  rpar_Punct = createPunct ")" ;
  lsb_Punct = createPunct "[" ;
  rsb_Punct = createPunct "]" ;
  squote_Punct = createPunct "'" ;
  quote_Punct = createPunct "\"" ;
  questmark_Punct = createPunct "?" ;
  comma_Punct = createPunct "," ;
  fullstop_Punct = createPunct "." ;

  ParaTaxis utt1 utt2 = {s = utt1.s ++ "-" ++ utt2.s} ;

  InterjectedS interj utt = { s = interj.s ++ SOFT_BIND ++ "," ++ utt.s } ;
}
