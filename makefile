compile_parser: lexer.x parser.y convert.out

convert.out: ./parser/lexer.hs ./parser/parser.hs ./parser/main.hs
	ghc -O2 -o convert.out $^

lexer.x: ./parser/lexer.x
	alex $^

parser.y: ./parser/parser.y
	happy $^
