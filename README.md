# `elm-salsa20` [![Build Status](https://github.com/miniBill/elm-salsa20/workflows/CI/badge.svg)](https://github.com/miniBill/elm-salsa20/actions?query=branch%3Amain)

`elm-salsa20` is a pure Elm implementation of the Salsa20 encryption algorithm. It can be used both for encryption/decryption (using the `Salsa20` module) and cryptographically secure random number generation.

It is mostly meant to be used in [lamdera](https://lamdera.com/), on the backend, for cryptographically secure random number generation.

If you need cryptographically secure random numbers on the frontend you should use `window.crypto.getRandomValues()`, possibly via [billstclair/elm-dev-random](https://package.elm-lang.org/packages/billstclair/elm-dev-random/latest/).

If you need encryption and decryption, you should libsodium, possibly using [libsodium.js](https://github.com/jedisct1/libsodium.js/). Using encryption and decryption primitives directly is a recipe for insecurity. See, e.g., https://stackoverflow.com/a/58360477.
