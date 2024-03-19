build:
	stack build 

run:
	stack exec -- ghc -O2 -threaded dyna-gloss/examples/Ball01.hs
	# stack exec -- runhaskell dyna-brick/examples/ReadChars.hs
	# stack exec -- runhaskell dyna-brick/examples/Puzzle15.hs
	# stack exec -- runhaskell dyna/examples/RockPaperScissors.hs
	# ./dyna-gloss/examples/Ball	
	# stack exec -- runhaskell dyna/examples/InputForm.hs
# stack exec -- runhaskell dyna-processing/examples/Ball.hs


run:
	./dyna-gloss/examples/Ball

run-1:
	./dyna-gloss/examples/Ball01

run-2:
	./dyna-gloss/examples/Ball02

run-3:
	./dyna-gloss/examples/Ball03

run-4:
	./dyna-gloss/examples/Ball

