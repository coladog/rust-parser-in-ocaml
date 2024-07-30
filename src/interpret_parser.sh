mkdir ./_build_interp
cd ./_build_interp
cp ../parser.mly .
cp ../tokens.mly . 
menhir ./tokens.mly --only-tokens
menhir ./parser.mly ./tokens.mly --external-tokens Tokens --base parser --interpret-show-cst