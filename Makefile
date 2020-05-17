HINDENT = hindent --sort-imports

.PHONY: test
test : build
	find test -name '*~' -delete
	find test -name '#*' -delete
	stack test

.PHONY: build
build :
	stack build

.PHONY: hindent
hindent :
	find app src test -name '*.hs' -exec $(HINDENT) \{} \;

.PHONY: hlint
hlint :
	hlint app src test | more

.PHONY: clean
clean :
	find . -name '*~' -delete
	find . -name '#*' -delete
	find . -name '.DS_Store' -delete
	stack clean
