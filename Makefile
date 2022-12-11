synthesisSources = ./src/Frontend/Synthesis/Position.hs \
	./src/Frontend/Synthesis/TypeCheckData.hs \
	./src/Frontend/Synthesis/TypeCheck.hs \
	./src/Frontend/Synthesis/Ast.hs \
	./src/Frontend/Synthesis/FrontExceptions.hs \
	./src/Frontend/Synthesis/Optimizer.hs \
	./src/Frontend/Synthesis/OptimizerData.hs
parserSources = ./src/Frontend/Parser/AbsLatte.hs \
	./src/Frontend/Parser/LexLatte.hs \
	./src/Frontend/Parser/ParLatte.hs
interludeSources = ./src/Interlude/Ssa.hs \
	./src/Interlude/SsaData.hs \
	./src/Interlude/Revamper.hs \
	./src/Interlude/RevamperData.hs
backendSources = ./src/Backend/LivenessCheck.hs \
	./src/Backend/LivenessCheckData.hs \
	./src/Backend/Assembler.hs \
	./src/Backend/RegisterAlloc.hs \
	./src/Backend/RegisterAllocData.hs \
	./src/Backend/Generator.hs \
	./src/Backend/Extractor.hs

all: native_runtime compiler

parser:
	# /home/students/inf/PUBLIC/MRJP/bin/bnfc ./src/Frontend/Lang/Latte.cf --functor -o ./src/Frontend/Parser
	happy -gcai ./src/Frontend/Parser/ParLatte.y -o ./src/Frontend/Parser/ParLatte.hs
	alex -g ./src/Frontend/Parser/LexLatte.x

native_runtime: src/Library/runtime.h src/Library/runtime.c
	gcc -O2 -c src/Library/runtime.c -o src/Library/runtime.o
	mkdir -p lib
	cp src/Library/runtime.o lib/runtime
	cp src/Library/externs lib/runtime.ext 

compiler: ./src/Main.hs ${synthesisSources} ${parserSources} ${interludeSources} ${backendSources}
	ghc --make -isrc/Frontend/Parser:src/Frontend/Synthesis:src/Interlude:src/Backend ./src/Main.hs -o latc

clean:
	# rm -f src/Frontend/Parser/*
	rm -f src/*.hi
	rm -f src/Frontend/Synthesis/*.hi
	rm -f src/Interlude/*.hi
	rm -f src/Backend/*.hi
	rm -f src/*.o
	rm -f src/Frontend/Synthesis/*.o
	rm -f src/Interlude/*.o
	rm -f src/Backend/*.o
	rm -f src/lib/*.o
	rm -f latc
	rm -r lib