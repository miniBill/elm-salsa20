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


{-| Encryption key.
-}
type alias Key =
    Internal.Salsa20.Key


{-| Nonce. A random unique code that should be unique for each message.
-}
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


encodeList : List Int -> Bytes.Encode.Encoder
encodeList list =
    list
        |> List.map Bytes.Encode.unsignedInt8
        |> Bytes.Encode.sequence


encodeBlock : Int32_16 -> Bytes.Encode.Encoder
encodeBlock block =
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
        width : Int
        width =
            Bytes.width input

        blockCount : Int
        blockCount =
            width // 64

        leftover : Int
        leftover =
            modBy 64 width

        fullBlock :
            Internal.Salsa20.Int32_16
            -> Bytes.Decode.Decoder Bytes.Encode.Encoder
        fullBlock keyBlock =
            Bytes.Decode.map
                (\block ->
                    encodeBlock (Internal.Salsa20.xor_16 keyBlock block)
                )
                getBlock

        partialBlock :
            Internal.Salsa20.Int32_16
            -> Bytes.Decode.Decoder Bytes.Encode.Encoder
        partialBlock keyBlock =
            if leftover == 0 then
                Bytes.Decode.succeed (Bytes.Encode.sequence [])

            else
                let
                    xorList : List Int
                    xorList =
                        [ keyBlock.y0, keyBlock.y1, keyBlock.y2, keyBlock.y3, keyBlock.y4, keyBlock.y5, keyBlock.y6, keyBlock.y7, keyBlock.y8, keyBlock.y9, keyBlock.y10, keyBlock.y11, keyBlock.y12, keyBlock.y13, keyBlock.y14, keyBlock.y15 ]
                in
                Bytes.Decode.loop ( leftover, [] )
                    (\( i, bacc ) ->
                        if i == 0 then
                            List.map2
                                Internal.Salsa20.xor
                                (List.reverse bacc)
                                xorList
                                |> encodeList
                                |> Bytes.Decode.Done
                                |> Bytes.Decode.succeed

                        else
                            Bytes.Decode.map
                                (\b -> Bytes.Decode.Loop ( i - 1, b :: bacc ))
                                Bytes.Decode.unsignedInt8
                    )

        decoder : Bytes.Decode.Decoder Bytes
        decoder =
            Bytes.Decode.loop ( 0, [] )
                (\( counter, acc ) ->
                    let
                        keyBlock : Int32_16
                        keyBlock =
                            Internal.Salsa20.expand key
                                (Internal.Salsa20.nonceAndCounter
                                    nonce
                                    (toCounter counter)
                                )
                                |> Internal.Salsa20.salsa20
                    in
                    if counter == blockCount then
                        partialBlock keyBlock
                            |> Bytes.Decode.map
                                (\block -> Bytes.Decode.Done <| block :: acc)

                    else
                        fullBlock keyBlock
                            |> Bytes.Decode.map
                                (\block ->
                                    Bytes.Decode.Loop ( counter + 1, block :: acc )
                                )
                )
                |> Bytes.Decode.map
                    (\blocks ->
                        blocks
                            |> List.reverse
                            |> Bytes.Encode.sequence
                            |> Bytes.Encode.encode
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
