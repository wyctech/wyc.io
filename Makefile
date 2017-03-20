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

clean:
	stack exec wyc clean
	rm -rf posts/meta/
	rm -rf _deploy/

deploy: clean all
	mkdir -p _deploy/; \
	if [ -d "_deploy/.git" ]; then \
		git clone git@github.com:wyctech/wyc.io.git _deploy; \
		git checkout -b gh-pages --track origin/gh-pages; \
	fi; \
	(cd _deploy; \
		git checkout gh-pages && \
		cp -r ../_site/* . && \
		git add . && \
		git commit -m'deploy' && \
		git push origin gh-pages);


.PHONY: all prepare compile build clean deploy
