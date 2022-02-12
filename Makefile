build:
	stack build dyna-processing

run:
	stack exec -- runhaskell dyna-processing/examples/Ball.hs
	# stack exec -- runhaskell dyna/examples/RockPaperScissors.hs
	# stack exec -- ghc -O2 -threaded dyna-gloss/examples/Ball.hs
	# ./dyna-gloss/examples/Ball	
	# stack exec -- runhaskell dyna/examples/InputForm.hs
