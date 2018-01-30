abstract UDTranslate =
  Translate,
  Verb [UseCopula],
  Extensions [GenNP,ComplVPIVV,UseQuantPN],
  Extend [VPI2,VPS2,ListVPI2,ListVPS2,MkVPS2,ConjVPS2,ComplVPS2,MkVPI2,ConjVPI2,ComplVPI2],
  UDDictionary
     ** {

flags startcat = Top ;

cat
  Top ;
  Punct ;
  Numer ;

fun
  TopPhr : Phr -> Top ;
  TopPhrPunct : Phr -> Punct -> Top ;
  
  
  StringPunct : String -> Punct ;
  StringCard  : String -> Card ;    ---- TODO proper numerals
  StringPN    : String -> PN ;      ---- word-based proper nouns

  PrefixPN    : PN -> PN -> PN ;
  PrefixCard  : Card -> Card -> Card ;
  PrefixSymb  : Symb -> Symb -> Symb ;

-- backup functions for unknown dependents

cat
  Backups ;
  Backup ;
  [Backup] {0} ;

  QVPS ;  -- VPS with IPs inside

fun
  MkBackups : [Backup] -> [Backup] -> Backups ; -- pre- and post-backups

  BackupAdV: AdV -> Backups -> AdV ;
  BackupAdv: Adv -> Backups -> Adv ;
  BackupAP : AP  -> Backups -> AP ;
  BackupNP : NP  -> Backups -> NP ;
  BackupVP : VP  -> Backups -> VP ;
  BackupVPS : VPS  -> Backups -> VPS ;
  BackupVPI : VPI -> Backups -> VPI ; 
  BackupVPSlash : VPSlash  -> Backups -> VPSlash ;
  BackupV2V : V2V  -> Backups -> V2V ;
  BackupQCl : QCl  -> Backups -> QCl ;
  BackupCl : Cl  -> Backups -> Cl ;
  BackupClSlash : ClSlash  -> Backups -> ClSlash ;
  BackupS  : S   -> Backups -> S ;
  BackupQS : QS  -> Backups -> QS ;
  BackupSC : SC  -> Backups -> SC ;
  BackupSubj : Subj -> Backups -> Subj ;
  BackupUtt : Utt -> Backups -> Utt ;
  BackupPhr : Phr -> Backups -> Phr ;
  BackupTop : Top -> Backups -> Top ;

  AdvBackup   : Adv -> Backup ;
  APBackup    : AP -> Backup ;
  NPBackup    : NP -> Backup ;
  VPBackup    : VP -> Backup ;
  VPIBackup   : VPI -> Backup ;
  VPSBackup   : VPS -> Backup ;
  VPSlashBackup : VPSlash -> Backup ;
  DetBackup   : Det -> Backup ;
  QuantBackup : Quant -> Backup ;
  InterjBackup : Interj -> Backup ;
  OrdBackup   : Ord -> Backup ;
  PNBackup    : PN -> Backup ;
  PrepBackup  : Prep -> Backup ;
  PunctBackup : Punct -> Backup ;
  ConjBackup  : Conj -> Backup ;
  SymbBackup  : Symb -> Backup ;
  SBackup     : S -> Backup ;
  QSBackup    : QS -> Backup ;
  SCBackup    : SC -> Backup ;
  SubjBackup  : Subj -> Backup ;
  UttBackup   : Utt -> Backup ;
  PhrBackup   : Phr -> Backup ;
  TopBackup   : Top -> Backup ;


-- extra lexicon to make it easier to connect to word-based dep trees
  all_Det   : Det ;
  no_Det    : Det ;
  these_Det : Det ;
  this_Det  : Det ;
  that_Det  : Det ;
  those_Det : Det ;
  these_NP  : NP ;
  this_NP   : NP ;
  that_NP   : NP ;
  those_NP  : NP ;
  which_Det : IDet ;

---- TODO: treat numbers with the Numeral grammar
  one_Card   : Card ;
  two_Card   : Card ;
  three_Card : Card ;
  four_Card  : Card ;
  five_Card  : Card ;
  ten_Card   : Card ;


-- PUNCT
  dash_Punct: Punct ;            -- -:1332
  ellipsis_Punct : Punct ;       -- ...:227
  exclmark_Punct : Punct ;       -- !:529
  colon_Punct : Punct ;          -- ::592
  semicolon_Punct : Punct ;      -- ;:101
  lpar_Punct : Punct ;           -- (:848
  rpar_Punct : Punct ;           -- ):882
  lsb_Punct : Punct ;            -- [:34
  rsb_Punct : Punct ;            -- ]:34
  quote_Punct : Punct ;          -- ":1352
  squote_Punct : Punct ;         
  questmark_Punct : Punct ;      -- ?:764
  comma_Punct : Punct ;          -- ,:7021
  fullstop_Punct : Punct ;       -- .:8645

---------- for tenses
  -- only check if linearizations exist for these functions

  PredSCVPS     : SC -> VPS -> S ;                -- that he grows is evident
  ExplPredSCVPS : SC -> VPS -> S ;                -- it is evident that he grows

  SlashVPS    : Temp -> Pol -> NP -> VPSlash -> SSlash ; 
  AdvVPS      : VPS -> Adv -> VPS   ;
  AddAdvQVPS  : QVPS -> IAdv -> QVPS ;
  RelVPS      : RP -> VPS -> RS      ;
  RelSSlash   : RP -> SSlash  -> RS  ;
  QuestVPS    : IP -> VPS -> QS ; 
  QuestSSlash : IP -> SSlash -> QS ;
  QuestSIAdv  : IAdv -> S -> QS ;   
  MkQVPS      : Temp -> Pol -> QVP -> QVPS ;
  UttVPS      : VPS -> Utt ; 

  -- to be added in the linearization 
  ImpersS     : VPS -> S ;                -- variant of ImpersCl               -- "it rains today"
  GenericS    : VPS -> S ;                -- variant of GenericCl              -- "One goes to movies for fun"
  ModExistNP  : VV -> NP -> S ;           -- variant of ExistNP with VV type   -- "there can be a cat"
  ModExistNPAdv : VV -> NP -> Adv -> S ;  -- variant of ExistNPAdv with VV type-- "there can be a cat on the hill"
  ModCleftNP  : VV -> NP -> RS -> S ;    -- variant of CleftNP                 -- "it can be a long  wait that results in nothing"
  ModCleftAdv : VV -> Adv -> S -> S  ;    -- variant of CleftAdv               -- "it can be here she slept" -- "it seems to be here she slept"

  -- to address the UD labels for ungrammatical texts
  ParaTaxis    : Utt -> Utt -> Utt ;     -- "(Fast and friendly service), (they know my order when I walk in the door)"
  -- ParaTaxis2 : Phr -> Phr -> Phr ;    -- decide between Phr and Utt (which is best)
  InterjectedS : Interj -> Utt -> Phr ;  -- "sorry, I went home" -- alternatively, this can also be same signature as ParaTaxis (Use UttInterj to get same signature)
  MultiUtt     : Utt -> Utt -> Utt ;      -- covers parataxis, interjected sentences and may be others
  LooseListNP  : ListNP -> NP ;          -- "(Aarne Ranta) (Phone: abc) (University of Gothenburg)"
  LooseListAdv : ListAdv -> Adv ;        -- same as above but for Advs
  
{- imported from Extend.gf
  MkVPS2    : Temp -> Pol -> VPSlash -> VPS2 ;  -- has loved                  
  ConjVPS2  : Conj -> [VPS2] -> VPS2 ;          -- has loved and now hates    
  ComplVPS2 : VPS2 -> NP -> VPS ;               -- has loved and now hates that person
  
  MkVPI2    : VPSlash -> VPI2 ;                 -- to love                    
  ConjVPI2  : Conj -> [VPI2] -> VPI2 ;          -- to love and hate           
  ComplVPI2 : VPI2 -> NP -> VPI ;               -- to love and hate that person
-}


cat 
  SO ;   -- record field for NP subject and NP object  {subj=..., obj=...} 
  ListSO ; 

fun 
  BaseSO : SO -> SO -> ListSO ; 
  ConsSO : SO -> ListSO -> ListSO ; 
  ElidedPredVP : NP -> VPS2 -> NP -> ListSO -> S ;  -- "Mary went to Prague and John  to London" -- go+to:V2 (Mary, John: ListNP) (Prague,London: ListNP) -- different sizes of ListNP? 

}
