--# -path=.:alltenses:gflibsrc/finnish/stemmed:gflibsrc/finnish:gflibsrc/abstract:gflibsrc/common:gflibsrc/finnish/kotus:gflibsrc/translator:gflibsrc/api
-- you need a symlink gflibsrc to your GF/lib/src

concrete UDDictionaryFin of UDDictionary = 
  CatFin,
  DictionaryFin
  ** open ParadigmsFin, MorphoFin in {

lin

-- Conj 
  not_Conj = mkConj "ei" ;
  yet_Conj = mkConj "yhä" ;
}
