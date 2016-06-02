GHC=ghc
DOC=haddock

all:
	$(GHC) main.hs

doc:
	$(DOC) --latex main.hs

html:
	$(DOC) --html main.hs

clean:
	rm *.o *.hi main *.html *.tex *.gif *.png *.jpg *.sty *.aux *.toc *.out *.js *.css
