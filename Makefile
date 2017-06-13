run:
	happy grammar.y
	ghc main.hs line.hs matrix.hs parser.hs display.hs curve.hs types.hs grammar.hs lexer.hs
	./main

prof:
	happy grammar.y
	ghc main.hs line.hs matrix.hs parser.hs display.hs curve.hs types.hs grammar.hs lexer.hs -prof -fprof-auto -rtsopts
	./main +RTS -p

clean:
	rm -f *.ppm *.png *.gif
	rm -f */*.ppm
	-rmdir */
	rm -f main main.o main.hi
	rm -f line.o line.hi
	rm -f matrix.o matrix.hi
	rm -f parser.o parser.hi
	rm -f display.o display.hi
	rm -f types.o types.hi
	rm -f curve.o curve.hi
	rm -f grammar.o grammar.hi grammar.hs
	rm -f lexer.o lexer.hi
