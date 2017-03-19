all: build prepare compile

prepare:
	@(cd posts/ && \
		mkdir -p meta; \
		for f in *.md; do \
			csplit "$$f" "%---%" > /dev/null; \
			mv xx00 "meta/$$f"; \
			echo "Generated posts/meta/$$f"; \
		done);

compile:
	stack exec wyc build

build:
	stack build

clean:
	stack exec wyc clean
	rm -rf posts/meta

.PHONY: all prepare compile build clean
