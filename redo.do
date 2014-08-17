redo-ifchange redo.hs redo.do redo.cabal
cabal-dev -v0 install
cp ./cabal-dev/bin/redo ./redo
ln -s -f redo redo-ifchange
ln -s -f redo redo-ifcreate
