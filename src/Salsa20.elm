module Salsa20 exposing
    ( salsa20
    , Key
    , init
    )

{-|

@docs salsa20


# Key management

@docs Key, toKey

-}

import Bytes exposing (Bytes)
import Internal.Salsa20 exposing (Int32_16)


{-| Builds a `Key` out of `Bytes`.

The key must be 16 or 32 bytes long, the nonce must be 16 bytes long.

Returns `Nothing` if any of the lengths are wrong.

-}
init : { key : Bytes, nonce : Bytes } -> Maybe Key
init { key, nonce } =
    Internal.Salsa20.init { key = key, nonce = nonce }
        |> Maybe.map Key


{-| A key used for encoding and decoding.
-}
type Key
    = Key Int32_16


{-| Run the Salsa20 function.
-}
salsa20 : a -> a
salsa20 input =
    input
