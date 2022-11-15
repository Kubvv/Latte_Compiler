synthesisSources = src/Frontend/Synthesis/Position.hs \
	src/Frontend/Synthesis/TypeCheckData.hs \
	src/Frontend/Synthesis/TypeCheck.hs \
	src/Frontend/Synthesis/Ast.hs \
	src/Frontend/Synthesis/FrontExceptions.hs \
	src/Frontend/Synthesis/Optimizer.hs \
	src/Frontend/Synthesis/OptimizerData.hs
parserSources = src/Frontend/Parser/AbsLatte.hs \
	src/Frontend/Parser/LexLatte.hs \
	src/Frontend/Parser/ParLatte.hs 

all: compiler

parser:
	/home/students/inf/PUBLIC/MRJP/bin/bnfc src/Frontend/Lang/Instant.cf -o src/Frontend/Parser
	happy -gcai src/Frontend/Parser/ParInstant.y
	alex -g src/Frontend/Parser/LexInstant.x

compiler: .\src\Main.hs ${synthesisSources} ${parserSources}
	ghc --make -isrc/Frontend/Parser:src/Frontend/Synthesis .\src\Main.hs -o latc

clean:
	rm -f *.hi
	rm -f Frontend/Parser/*.hi
	rm -f Frontend/Synthesis/*.hi
	rm -f *.o
	rm -f Frontend/Parser/*.o
	rm -f Frontend/Synthesis/*.o
	rm -f latc