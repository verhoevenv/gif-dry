#!/bin/bash
rm -rf target
mkdir target
if [ "${TRAVIS}" = "true" ]; then
    $TRAVIS_BUILD_DIR/sysconfcpus/bin/sysconfcpus -n 2 elm-make src/Main.elm --output=target/main.js
else
    elm-make src/Main.elm --output=target/main.js
fi
cp src/index.html target