build:
	stack build dyna-gloss

run:
	stack exec -- ghc -O2 -threaded dyna-gloss/examples/SolarSystem.hs
	./dyna-gloss/examples/SolarSystem
	
#	stack exec -- runhaskell dyna-brick/examples/Hello.hs
