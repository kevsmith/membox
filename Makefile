all: compile

compile: ebin special
	@cd src;erl -make
	@rm -f src/membox_parser.erl src/membox_lexer.erl

ebin:
	@mkdir ebin

clean:
	@rm -rf ebin

tests: compile
	@rm -rf test_db
	@cd tests;erl -make
	@erl -noshell -pa ebin -b start_sasl -eval 'membox_suite:test().' -s init stop

special: src/membox_lexer.erl src/membox_parser.erl

src/membox_lexer.erl:
	cd src;erl -noshell -s init stop -eval 'leex:file("membox_lexer.xrl")'

src/membox_parser.erl:
	erl -noshell -s init stop -eval 'yecc:file("src/membox_parser.yrl")'
