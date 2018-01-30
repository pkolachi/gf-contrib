--# -path=.:alltenses:gflibsrc/translator:gflibsrc/chunk
-- you need a symlink gflibsrc to your GF/lib/src

concrete UDDictionarySwe of UDDictionary = 
  CatSwe,
  DictionarySwe 
  ** open ParadigmsSwe, MorphoSwe, (M=MakeStructuralSwe) in {

lin
  
-- Conj
  not_Conj = M.mkConj "inte" ; -- TO CHECK
  yet_Conj = M.mkConj "ännu" ; -- TO CHECK
}
