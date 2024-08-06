mkdir ./_build_interp
cd ./_build_interp
cp ../RSparser.mly .
cp ../RStokens.mly . 
menhir ./RStokens.mly --only-tokens
menhir ./RSparser.mly ./RStokens.mly --external-tokens RStokens --base RSparser --interpret-show-cst