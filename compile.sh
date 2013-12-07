rm -r build
rm -r cache
elm -m --src-dir=./src --runtime=elm-runtime.js src/Main.elm
cp $HOME/.cabal/share/Elm-0.10/elm-runtime.js ./build
mv ./build/src/Main.html ./build/index.html
rm -r ./build/src