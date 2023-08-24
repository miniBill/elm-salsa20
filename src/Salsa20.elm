module Salsa20 exposing
    ( salsa20
    , Key, toKey, Nonce, toNonce
    )

{-|

@docs salsa20


# Key management

@docs Key, toKey, Nonce, toNonce

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Internal.Salsa20 exposing (Counter(..), Int32_16, Int32_2, Int32_4, Key(..), Nonce(..))


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


getBlock : Bytes.Decode.Decoder Int32_16
getBlock =
    Bytes.Decode.map4
        (\b0 b1 b2 b3 ->
            { y0 = b0.z0
            , y1 = b0.z1
            , y2 = b0.z2
            , y3 = b0.z3
            , y4 = b1.z0
            , y5 = b1.z1
            , y6 = b1.z2
            , y7 = b1.z3
            , y8 = b2.z0
            , y9 = b2.z1
            , y10 = b2.z2
            , y11 = b2.z3
            , y12 = b3.z0
            , y13 = b3.z1
            , y14 = b3.z2
            , y15 = b3.z3
            }
        )
        get16
        get16
        get16
        get16


putBlock : Int32_16 -> Bytes.Encode.Encoder
putBlock block =
    let
        u32 : Int -> Bytes.Encode.Encoder
        u32 =
            Bytes.Encode.unsignedInt32 Bytes.LE
    in
    Bytes.Encode.sequence
        [ u32 block.y0
        , u32 block.y1
        , u32 block.y2
        , u32 block.y3
        , u32 block.y4
        , u32 block.y5
        , u32 block.y6
        , u32 block.y7
        , u32 block.y8
        , u32 block.y9
        , u32 block.y10
        , u32 block.y11
        , u32 block.y12
        , u32 block.y13
        , u32 block.y14
        , u32 block.y15
        ]


{-| Encrypt/decrypt data using the Salsa20 function.

This function is symmetric, so it can be used for both encryption and decryption.

-}
salsa20 : Key -> Nonce -> Bytes -> Bytes
salsa20 key nonce input =
    let
        keyBlock : Int -> Internal.Salsa20.Int32_16
        keyBlock counter =
            Internal.Salsa20.expand key
                (Internal.Salsa20.nonceAndCounter
                    nonce
                    (toCounter counter)
                )
                |> Internal.Salsa20.salsa20

        width : Int
        width =
            Bytes.width input

        blocks : Int
        blocks =
            width // 64

        leftover : Int
        leftover =
            modBy 64 width

        fullBlock :
            Int
            -> List Bytes.Encode.Encoder
            -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List Bytes.Encode.Encoder ) Bytes)
        fullBlock counter acc =
            getBlock
                |> Bytes.Decode.map
                    (\block ->
                        Bytes.Decode.Loop
                            ( counter + 1
                            , putBlock (Internal.Salsa20.xor_16 (keyBlock counter) block)
                                :: acc
                            )
                    )

        partialBlock :
            Int
            -> List Bytes.Encode.Encoder
            -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List Bytes.Encode.Encoder ) Bytes)
        partialBlock counter acc =
            let
                result =
                    if leftover == 0 then
                        Bytes.Decode.succeed acc

                    else
                        let
                            xor : Int32_16
                            xor =
                                keyBlock counter

                            xorList : List Int
                            xorList =
                                [ xor.y0, xor.y1, xor.y2, xor.y3, xor.y4, xor.y5, xor.y6, xor.y7, xor.y8, xor.y9, xor.y10, xor.y11, xor.y12, xor.y13, xor.y14, xor.y15 ]

                            decoded =
                                Bytes.Decode.loop ( leftover, [] )
                                    (\( i, bacc ) ->
                                        if i == 0 then
                                            List.map2
                                                Internal.Salsa20.xor
                                                (List.reverse bacc)
                                                xorList
                                                |> List.map Bytes.Encode.unsignedInt8
                                                |> Bytes.Encode.sequence
                                                |> Bytes.Decode.Done
                                                |> Bytes.Decode.succeed

                                        else
                                            Bytes.Decode.map
                                                (\b -> Bytes.Decode.Loop ( i - 1, b :: bacc ))
                                                Bytes.Decode.unsignedInt8
                                    )
                        in
                        decoded
                            |> Bytes.Decode.map (\d -> d :: acc)
            in
            result
                |> Bytes.Decode.map
                    (\encrypted ->
                        encrypted
                            |> List.reverse
                            |> Bytes.Encode.sequence
                            |> Bytes.Encode.encode
                            |> Bytes.Decode.Done
                    )

        decoder =
            Bytes.Decode.loop ( 0, [] )
                (\( counter, acc ) ->
                    if counter == blocks then
                        partialBlock counter acc

                    else
                        fullBlock counter acc
                )
    in
    Bytes.Decode.decode decoder input
        -- This should never happen
        |> Maybe.withDefault (Bytes.Encode.encode <| Bytes.Encode.sequence [])


toCounter : Int -> Counter
toCounter counter =
    Counter
        { z0 = counter |> Bitwise.shiftRightZfBy 0
        , z1 = counter // 0x0000000100000000
        }
