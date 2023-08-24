module Salsa20 exposing
    ( salsa20
    , Key, toKey, Nonce, toNonce
    )

{-|

@docs salsa20


# Key management

@docs Key, toKey, Nonce, toNonce

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Internal.Salsa20 exposing (Int32_2, Int32_4, Key(..), Nonce(..))


type alias Key =
    Internal.Salsa20.Key


type alias Nonce =
    Internal.Salsa20.Nonce


{-| Build a nonce out of bytes. The nonce must be either 8 bytes long.
-}
toNonce : Bytes -> Maybe Nonce
toNonce bytes =
    bytes
        |> Bytes.Decode.decode get8
        |> Maybe.map Nonce


{-| Build a key out of bytes. The key must be either 16 or 32 bytes long.
-}
toKey : Bytes -> Maybe Key
toKey bytes =
    case Bytes.width bytes of
        16 ->
            Bytes.Decode.decode get16 bytes
                |> Maybe.map Key16

        32 ->
            Bytes.Decode.decode get32 bytes
                |> Maybe.map (\( k0, k1 ) -> Key32 k0 k1)

        _ ->
            Nothing


get8 : Bytes.Decode.Decoder Int32_2
get8 =
    let
        u32 =
            Bytes.Decode.unsignedInt32 Bytes.LE
    in
    Bytes.Decode.map2 Int32_2 u32 u32


get16 : Bytes.Decode.Decoder Int32_4
get16 =
    let
        u32 =
            Bytes.Decode.unsignedInt32 Bytes.LE
    in
    Bytes.Decode.map4 Int32_4 u32 u32 u32 u32


get32 : Bytes.Decode.Decoder ( Int32_4, Int32_4 )
get32 =
    Bytes.Decode.map2 Tuple.pair get16 get16


{-| Run the Salsa20 function.
-}
salsa20 : Key -> Nonce -> Bytes -> Bytes
salsa20 key nonce input =
    let
        keyBlock counter =
            Internal.Salsa20.expand key
                (Internal.Salsa20.nonceAndCounter
                    nonce
                    counter
                )
                |> Internal.Salsa20.salsa20
    in
    keyBlock 0
        |> Debug.todo "salsa20"
