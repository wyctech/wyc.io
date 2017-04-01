all: build prepare compile

prepare:
	@(cd posts/; \
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

watch:
	stack exec wyc watch

clean:
	stack exec wyc clean
	rm -rf posts/meta/

deploy: clean all
	(cd _deploy; \
		git checkout gh-pages && \
		cp -r ../_site/* . && \
		git add . && \
		git commit -m'deploy' && \
		git push origin gh-pages);


.PHONY: all prepare compile build clean deploy
