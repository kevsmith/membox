all: compile

compile: ebin special
	@cd src;erl -make
	@cp src/membox.app ebin

ebin:
	@mkdir ebin

clean:
	@rm -rf ebin
	@rm -f src/membox_lexer.erl src/membox_parser.erl

tests: compile
	@rm -rf test_db
	@cd tests;erl -make
	@echo "Running test suite"
	@erl -noshell -pa ebin -b start_sasl -s crypto -eval 'membox_suite:test().' -s init stop

special: lexer parser

lexer: src/membox_lexer.erl

parser: src/membox_parser.erl

src/membox_lexer.erl: src/membox_lexer.xrl
	@echo "Generating lexer"
	@cd src;erl -noshell -s init stop -eval 'leex:file("membox_lexer.xrl")'


src/membox_parser.erl: src/membox_parser.yrl
	@echo "Generating parser"
	@erl -noshell -s init stop -eval 'yecc:file("src/membox_parser.yrl")'
