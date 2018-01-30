--# -path=.:alltenses:gflibsrc/translator:gflibsrc/chunk
-- you need a symlink gflibsrc to your GF/lib/src

concrete UDTranslateEng of UDTranslate =
  TranslateEng,
  VerbEng [UseCopula],
  ExtensionsEng [GenNP,UseQuantPN,ComplVPIVV],
  ExtendEng [VPI2,VPS2,ListVPI2,ListVPS2,MkVPS2,ConjVPS2,ComplVPS2,MkVPI2,ConjVPI2,ComplVPI2],
  UDDictionaryEng **
 UDTranslateFunctor - [all_Det] with
  (Syntax = SyntaxEng),
  (Symbolic = SymbolicEng),
  (Extensions = ExtensionsEng)
  ** open ParadigmsEng, (M=MorphoEng) in {

lin
  
  StringPN pn = {s = \\_ => pn.s ; g = human} ;
  PrefixPN pn1 pn2 = {s = \\c => pn1.s ! c ++ pn2.s ! c ; g = pn2.g} ; --- Thomas+Mann

  all_Det = M.mkDeterminer plural "all" ;


  SlashVPS t p np vps = ExtensionsEng.PredVPS np (ExtensionsEng.MkVPS t p vps) ** {c2 = vps.c2} ;
  AdvVPS vps adv = lin VPS {s = \\a => vps.s ! a ++ adv.s} ;

  UttVPS vps = lin Utt {s = vps.s ! it_NP.a} ;

  MultiUtt utt1 utt2 = {s = utt1.s ++ utt2.s} ;
}

