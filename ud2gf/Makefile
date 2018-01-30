SAMPLE = -N1000
OPTS   = -a1 -g10000 -n30 -Dscamfptin   # add beam size of 30  
PROFOPTS = +RTS -sstderr 
# mystery of display opts solved
# -Dg is what allows the dictionary to be read, either from the PGF file/
# external lexicon. SO, removing that makes the entire conversion 
# work with only an emptyDictionary. Hence, the StringPN function being
# used. Similarly, -Dg or -Dt is what reads the PGF file. So, the 
# conversion output is sensitive to what is and is not included in the
# display options
# Now -Dp is treated similar to -Dg except the final GF trees are printed
# in oneline .
GR       = -GUDTranslate.pgf
enOPTS      = $(OPTS) $(GR) -lEng -CUDTranslate.labels,VPS.labels,UDTranslateEng.labels
fiOPTS      = $(OPTS) $(GR) -lFin -CUDTranslate.labels,VPS.labels,UDTranslateFin.labels
svOPTS      = $(OPTS) $(GR) -lSwe -CUDTranslate.labels,VPS.labels,UDTranslateSwe.labels
etOPTS      = $(OPTS) $(GR) -lEst -CUDTranslate.labels,VPS.labels,UDTranslateEst.labels
env2OPTS    = $(OPTS) $(GR) -lEng -CUDv2Translate.labels,UDv2TranslateEng.labels
fiv2OPTS    = $(OPTS) $(GR) -lFin -CUDv2Translate.labels,UDv2TranslateFin.labels
svv2OPTS    = $(OPTS) $(GR) -lSwe -CUDv2Translate.labels,UDv2TranslateSwe.labels
etv2OPTS    = $(OPTS) $(GR) -lEst -CUDv2Translate.labels,UDv2TranslateEst.labels
newenOPTS   = $(OPTS) $(GR) -lEng -CNewUDTranslate.labels,NewUDTranslateEng.labels
newfiOPTS   = $(OPTS) $(GR) -lFin -CNewUDTranslate.labels,NewUDTranslateFin.labels
newsvOPTS   = $(OPTS) $(GR) -lSwe -CNewUDTranslate.labels,NewUDTranslateSwe.labels
newetOPTS   = $(OPTS) $(GR) -lEst -CNewUDTranslate.labels,NewUDTranslateEst.labels
newenv2OPTS = $(OPTS) $(GR) -lEng -CNewUDv2Translate.labels,NewUDv2TranslateEng.labels
newfiv2OPTS = $(OPTS) $(GR) -lFin -CNewUDv2Translate.labels,NewUDv2TranslateFin.labels
newsvv2OPTS = $(OPTS) $(GR) -lSwe -CNewUDv2Translate.labels,NewUDv2TranslateSwe.labels
newetv2OPTS = $(OPTS) $(GR) -lEst -CNewUDv2Translate.labels,NewUDv2TranslateEst.labels

SHELL = /bin/bash

all: grammar expgrammar compile testEng testSwe testFin testEst

compile:
	ghc --make -O2 -o ud2gf UD2GF.hs  -fforce-recomp

devcompile:
	ghc --make -O2 -o ud2gf UD2GF.hs -rtsopts -fforce-recomp

grammar:
	gf -make -name=UDTranslateEng  UDTranslateEng.gf 
	gf -make -name=UDTranslateSwe  UDTranslateSwe.gf 
	gf -make -name=UDTranslateFin  UDTranslateFin.gf
	gf -make -name=UDTranslate  \
	    UDTranslateEng.pgf  \
	    UDTranslateSwe.pgf  \
	    UDTranslateFin.pgf
	rm -v UDTranslateEng.pgf UDTranslateSwe.pgf UDTranslateFin.pgf

expgrammar:
	gf -make -name=UDTranslateEng  UDTranslateEng.gf 
	gf -make -name=UDTranslateSwe  UDTranslateSwe.gf 
	gf -make -name=UDTranslateFin  UDTranslateFin.gf
	gf -make -name=UDTranslateEst  UDTranslateEst.gf
	gf -make -name=UDTranslate  \
	    UDTranslateEng.pgf  \
	    UDTranslateSwe.pgf  \
	    UDTranslateFin.pgf
	rm -v UDTranslateEng.pgf UDTranslateSwe.pgf UDTranslateFin.pgf \
	    UDTranslateEst.pgf 

enggrammar:
	gf -make -name=UDTranslateEng UDTranslateEng.gf

test:
	time ./ud2gf -lEng -Dscamifgtn -GUDTranslate.pgf -CUDTranslate.labels,UDTranslateEng.labels doc/cat.conll

profile:
	time ./ud2gf $(enOPTS) handexampleseng.conllu $(PROFOPTS) > latest-handegs-en.tmp

sample:
	time ./ud2gf $(enOPTS) handexampleseng.conllu > latest-handegs-en.tmp
	tail -2 latest-handegs-en.tmp                                             
	grep "TRANSL" latest-handegs-en.tmp | wc                                  
	grep "ERROR"  latest-handegs-en.tmp | wc                   

sample2:
	time ./ud2gf $(env2OPTS) handexamplesv2eng.conllu > latest-handegs-v2en.tmp
	tail -2 latest-handegs-v2en.tmp                                             
	grep "TRANSL" latest-handegs-v2en.tmp | wc                                  
	grep "ERROR"  latest-handegs-v2en.tmp | wc                                   

newsample:
	time ./ud2gf $(newenOPTS) handexampleseng.conllu > latest-handegs-en-new.tmp
	tail -2 latest-handegs-en-new.tmp                                             
	grep "TRANSL" latest-handegs-en-new.tmp | wc                                  
	grep "ERROR"  latest-handegs-en-new.tmp | wc                                   

newsample2:
	time ./ud2gf $(newenv2OPTS) handexamplesv2eng.conllu > latest-handegs-v2en-new.tmp
	tail -2 latest-handegs-v2en-new.tmp                                             
	grep "TRANSL" latest-handegs-v2en-new.tmp | wc                                  
	grep "ERROR"  latest-handegs-v2en-new.tmp | wc                                   

testFin:
	time ./ud2gf -Fdict-fin-eng.tsv $(SAMPLE) $(fiOPTS) treebanks/UD_Finnish/fi-ud-train.conllu >latest-train-fi.tmp
	tail -2 latest-train-fi.tmp
	grep "TRANSL" latest-train-fi.tmp | wc
	grep "ERROR" latest-train-fi.tmp | wc

testEng:
	time ./ud2gf $(SAMPLE) $(enOPTS) treebanks/UD_English/en-ud-train.conllu >latest-train-en.tmp
	tail -2 latest-train-en.tmp
	grep "TRANSL" latest-train-en.tmp | wc
	grep "ERROR" latest-train-en.tmp | wc

testEngOnly:
	time ./ud2gf $(SAMPLE) $(enOPTS) treebanks/UD_English/en-ud-train.conllu >latest-train-only-en.tmp
	tail -2 latest-train-only-en.tmp
	grep "TRANSL" latest-train-only-en.tmp | wc
	grep "ERROR" latest-train-only-en.tmp | wc

testSwe:
	time ./ud2gf $(SAMPLE) $(svOPTS) treebanks/UD_Swedish/sv-ud-train.conllu >latest-train-sv.tmp
	tail -2 latest-train-sv.tmp
	grep "TRANSL" latest-train-sv.tmp | wc
	grep "ERROR" latest-train-sv.tmp | wc

testNodalida:
	time ./ud2gf -lEng -t10000 -k3000 -a1 -g1 -Dscaifgtn -CUDTranslate.labels,UDTranslateEng.labels treebanks/UD_English/en-ud-test.conllu >latest-test-en.tmp
	time ./ud2gf -lSwe -t10000 -k3000 -a1 -g1 -Dscaifgtn -CUDTranslate.labels,UDTranslateSwe.labels treebanks/UD_Swedish/sv-ud-test.conllu >latest-test-sv.tmp
	time ./ud2gf -lFin -Fdict-fin-eng.tsv -t10000 -k3000 -a1 -g1 -Dscaifgtn -CUDTranslate.labels,UDTranslateFin.labels treebanks/UD_Finnish/fi-ud-test.conllu >latest-test-fi.tmp
	time ./ud2gf -lSwe -t10000 -k3000 -a1 -g1 -Dscaifgtn -CUDTranslate.labels treebanks/UD_Swedish/sv-ud-test.conllu >latest-test-sv-absonly.tmp
	time ./ud2gf -lFin -Fdict-fin-eng.tsv -t10000 -k3000 -a1 -g1 -Dscaifgtn -CUDTranslate.labels treebanks/UD_Finnish/fi-ud-test.conllu >latest-test-fi-absonly.tmp

testEst:
	time ./ud2gf $(SAMPLE) $(etOPTS) treebanks/UD_Estonian/et-ud-train.conllu >latest-train-et.tmp
	tail -2 latest-train-et.tmp
	grep "TRANSL" latest-train-et.tmp | wc
	grep "ERROR" latest-train-et.tmp | wc

testLexEng:
	time ./ud2gf -a1 -g0 -Dsca -GUDTranslate.pgf -lEng -CUDTranslate.labels,UDTranslateEng.labels treebanks/UD_English/en-ud-train.conllu >latest-train-en-lexannot.tmp
	grep -e "([0-9]*)" latest-train-en-lexannot.tmp | sed -e 's/^[ ]*//g' | cut -f1 -d{ | sort | uniq -c | grep "String" | wc -l

testLexFin:
	time ./ud2gf -a1 -g0 -Dsca -Fdict-fin-eng.tsv -GUDTranslate.pgf -lFin -CUDTranslate.labels,UDTranslateFin.labels treebanks/UD_Finnish/fi-ud-train.conllu >latest-train-fi-lexannot.tmp
	grep -e "([0-9]*)" latest-train-fi-lexannot.tmp | sed -e 's/^[ ]*//g' | cut -f1 -d{ | sort | uniq -c | grep "String" | wc -l

testLexSwe:
	time ./ud2gf -a1 -g0 -Dsca -GUDTranslate.pgf -lSwe -CUDTranslate.labels,UDTranslateSwe.labels treebanks/UD_Swedish/sv-ud-train.conllu >latest-train-sv-lexannot.tmp
	grep -e "([0-9]*)" latest-train-sv-lexannot.tmp | sed -e 's/^[ ]*//g' | cut -f1 -d{ | sort | uniq -c | grep "String" | wc -l

