typecheckSources = src/Frontend/Typecheck/Ast.hs
parserSources = src/Frontend/Parser/AbsLatte.hs src/Frontend/Parser/LexLatte.hs src/Frontend/Parser/ParLatte.hs

all: compiler

compiler: .\src\Main.hs ${typecheckSources} ${parserSources}
	ghc --make -isrc/Frontend/Parser:src/Frontend/Synthesis .\src\Main.hs -o latc

clean:
	rm -f *.hi
	rm -f Frontend/Parser/*.hi
	rm -f Frontend/Typecheck/*.hi
	rm -f *.o
	rm -f Frontend/Parser/*.o
	rm -f Frontend/Typecheck/*.o
	rm -f latc