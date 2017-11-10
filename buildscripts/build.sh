rm -rf target
mkdir target
elm-make src/Main.elm --output=target/main.js
cp src/index.html target