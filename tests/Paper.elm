module Paper exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Encode
import Expect
import Fuzz exposing (Fuzzer)
import Hex
import Internal.Salsa20 exposing (Int32_16, doubleround, init, littleendian, littleendianInverse, salsa20)
import Test exposing (Test, describe, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "Test vectors in the paper"
        [ test "hex" <|
            \_ ->
                0xC0A8787E
                    |> Expect.equal 3232266366
        , test "plus" <|
            \_ ->
                Internal.Salsa20.plus 0xC0A8787E 0x9FD1161D
                    |> equalHex 0x60798E9B
        , test "xor" <|
            \_ ->
                Internal.Salsa20.xor 0xC0A8787E 0x9FD1161D
                    |> equalHex 0x5F796E63
        , test "xor (high bit set)" <|
            \_ ->
                Internal.Salsa20.xor 0xC0A8787E 0x1FD1161D
                    |> equalHex 0xDF796E63
        , describe "Left rotation"
            [ test "0xC0A8787E <<< 5" <|
                \_ ->
                    0xC0A8787E
                        |> Internal.Salsa20.rotateLeftBy 5
                        |> equalHex 0x150F0FD8
            , test "0x0C0A878E <<< 5" <|
                \_ ->
                    0x0C0A878E
                        |> Internal.Salsa20.rotateLeftBy 5
                        |> equalHex 0x8150F1C1
            , fuzz2
                (Fuzz.intRange 0 0xFFFFFFFF)
                (Fuzz.intRange 1 31)
                "n <<< a <<< (32 - b) == n"
              <|
                \u c ->
                    u
                        |> Internal.Salsa20.rotateLeftBy c
                        |> Internal.Salsa20.rotateLeftBy (32 - c)
                        |> equalHex u
            , fuzz3
                (Fuzz.intRange 0 0xFFFFFFFF)
                (Fuzz.intRange 0 0xFFFFFFFF)
                (Fuzz.intRange 1 31)
                "(a ^ b) <<< n == (a <<< n) ^ (b <<< n)"
              <|
                \a b c ->
                    Internal.Salsa20.xor a b
                        |> Internal.Salsa20.rotateLeftBy c
                        |> equalHex
                            (Internal.Salsa20.xor
                                (a |> Internal.Salsa20.rotateLeftBy c)
                                (b |> Internal.Salsa20.rotateLeftBy c)
                            )
            ]
        , describe "quarterround"
            [ test "quarterround(0x00000000, 0x00000000, 0x00000000, 0x00000000) = (0x00000000, 0x00000000, 0x00000000, 0x00000000)." <|
                \_ ->
                    Internal.Salsa20.quarterround 0x00 0x00 0x00 0x00
                        |> Expect.equal { z0 = 0, z1 = 0, z2 = 0, z3 = 0x00 }
            , test "quarterround(0x00000001, 0x00000000, 0x00000000, 0x00000000) = (0x08008145, 0x00000080, 0x00010200, 0x20500000)." <|
                \_ ->
                    Internal.Salsa20.quarterround 0x01 0x00 0x00 0x00
                        |> Expect.equal { z0 = 0x08008145, z1 = 0x80, z2 = 0x00010200, z3 = 0x20500000 }
            , test "quarterround(0x00000000, 0x00000001, 0x00000000, 0x00000000) = (0x88000100, 0x00000001, 0x00000200, 0x00402000)." <|
                \_ ->
                    Internal.Salsa20.quarterround 0x00 0x01 0x00 0x00
                        |> Expect.equal { z0 = 0x88000100, z1 = 0x01, z2 = 0x0200, z3 = 0x00402000 }
            , test "quarterround(0x00000000, 0x00000000, 0x00000001, 0x00000000) = (0x80040000, 0x00000000, 0x00000001, 0x00002000)." <|
                \_ ->
                    Internal.Salsa20.quarterround 0x00 0x00 0x01 0x00
                        |> Expect.equal { z0 = 0x80040000, z1 = 0, z2 = 0x01, z3 = 0x2000 }
            , test "quarterround(0x00000000, 0x00000000, 0x00000000, 0x00000001) = (0x00048044, 0x00000080, 0x00010000, 0x20100001)." <|
                \_ ->
                    Internal.Salsa20.quarterround 0x00 0x00 0x00 0x01
                        |> Expect.equal { z0 = 0x00048044, z1 = 0x80, z2 = 0x00010000, z3 = 0x20100001 }
            , test "quarterround(0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137) = (0xe876d72b, 0x9361dfd5, 0xf1460244, 0x948541a3)." <|
                \_ ->
                    Internal.Salsa20.quarterround 0xE7E8C006 0xC4F9417D 0x6479B4B2 0x68C67137
                        |> Expect.equal { z0 = 0xE876D72B, z1 = 0x9361DFD5, z2 = 0xF1460244, z3 = 0x948541A3 }
            , test "quarterround(0xd3917c5b, 0x55f1c407, 0x52a58a7a, 0x8f887a3b) = (0x3e2f308c, 0xd90a8f36, 0x6ab2a923, 0x2883524c)." <|
                \_ ->
                    Internal.Salsa20.quarterround 0xD3917C5B 0x55F1C407 0x52A58A7A 0x8F887A3B
                        |> Expect.equal { z0 = 0x3E2F308C, z1 = 0xD90A8F36, z2 = 0x6AB2A923, z3 = 0x2883524C }
            ]
        , describe "rowround"
            [ test "rowround1" <|
                \_ ->
                    Internal.Salsa20.rowround { y0 = 0x01, y1 = 0, y2 = 0, y3 = 0, y4 = 0x01, y5 = 0, y6 = 0, y7 = 0, y8 = 0x01, y9 = 0, y10 = 0, y11 = 0, y12 = 0x01, y13 = 0, y14 = 0, y15 = 0x00 }
                        |> Expect.equal { y0 = 0x08008145, y1 = 0x80, y2 = 0x00010200, y3 = 0x20500000, y4 = 0x20100001, y5 = 0x00048044, y6 = 0x80, y7 = 0x00010000, y8 = 0x01, y9 = 0x2000, y10 = 0x80040000, y11 = 0, y12 = 0x01, y13 = 0x0200, y14 = 0x00402000, y15 = 0x88000100 }
            , test "rowround2" <|
                \_ ->
                    Internal.Salsa20.rowround { y0 = 0x08521BD6, y1 = 0x1FE88837, y2 = 0xBB2AA576, y3 = 0x3AA26365, y4 = 0xC54C6A5B, y5 = 0x2FC74C2F, y6 = 0x6DD39CC3, y7 = 0xDA0A64F6, y8 = 0x90A2F23D, y9 = 0x067F95A6, y10 = 0x06B35F61, y11 = 0x41E4732E, y12 = 0xE859C100, y13 = 0xEA4D84B7, y14 = 0x0F619BFF, y15 = 0xBC6E965A }
                        |> Expect.equal { y0 = 0xA890D39D, y1 = 0x65D71596, y2 = 0xE9487DAA, y3 = 0xC8CA6A86, y4 = 0x949D2192, y5 = 0x764B7754, y6 = 0xE408D9B9, y7 = 0x7A41B4D1, y8 = 0x3402E183, y9 = 0x3C3AF432, y10 = 0x50669F96, y11 = 0xD89EF0A8, y12 = 0x0040EDE5, y13 = 0xB545FBCE, y14 = 0xD257ED4F, y15 = 0x1818882D }
            ]
        , describe "columnround"
            [ test "columnround1" <|
                \_ ->
                    Internal.Salsa20.columnround { y0 = 0x01, y1 = 0, y2 = 0, y3 = 0, y4 = 0x01, y5 = 0, y6 = 0, y7 = 0, y8 = 0x01, y9 = 0, y10 = 0, y11 = 0, y12 = 0x01, y13 = 0, y14 = 0, y15 = 0x00 }
                        |> Expect.equal { y0 = 0x10090288, y1 = 0, y2 = 0, y3 = 0, y4 = 0x0101, y5 = 0, y6 = 0, y7 = 0, y8 = 0x00020401, y9 = 0, y10 = 0, y11 = 0, y12 = 0x40A04001, y13 = 0, y14 = 0, y15 = 0 }
            , test "columnround2" <|
                \_ ->
                    Internal.Salsa20.columnround { y0 = 0x08521BD6, y1 = 0x1FE88837, y2 = 0xBB2AA576, y3 = 0x3AA26365, y4 = 0xC54C6A5B, y5 = 0x2FC74C2F, y6 = 0x6DD39CC3, y7 = 0xDA0A64F6, y8 = 0x90A2F23D, y9 = 0x067F95A6, y10 = 0x06B35F61, y11 = 0x41E4732E, y12 = 0xE859C100, y13 = 0xEA4D84B7, y14 = 0x0F619BFF, y15 = 0xBC6E965A }
                        |> Expect.equal { y0 = 0x8C9D190A, y1 = 0xCE8E4C90, y2 = 0x1EF8E9D3, y3 = 0x1326A71A, y4 = 0x90A20123, y5 = 0xEAD3C4F3, y6 = 0x63A091A0, y7 = 0xF0708D69, y8 = 0x789B010C, y9 = 0xD195A681, y10 = 0xEB7D5504, y11 = 0xA774135C, y12 = 0x481C2027, y13 = 0x53A8E4B5, y14 = 0x4C1F89C5, y15 = 0x3F78C9C8 }
            ]
        , describe "doubleround"
            [ test "doubleround1" <|
                \_ ->
                    doubleround { y0 = 0x01, y1 = 0, y2 = 0, y3 = 0, y4 = 0, y5 = 0, y6 = 0, y7 = 0, y8 = 0, y9 = 0, y10 = 0, y11 = 0, y12 = 0, y13 = 0, y14 = 0, y15 = 0x00 }
                        |> Expect.equal { y0 = 0x8186A22D, y1 = 0x0040A284, y2 = 0x82479210, y3 = 0x06929051, y4 = 0x08000090, y5 = 0x02402200, y6 = 0x4000, y7 = 0x00800000, y8 = 0x00010200, y9 = 0x20400000, y10 = 0x08008104, y11 = 0x00, y12 = 0x20500000, y13 = 0xA0000040, y14 = 0x0008180A, y15 = 0x612A8020 }
            , test "doubleround2" <|
                \_ ->
                    doubleround { y0 = 0xDE501066, y1 = 0x6F9EB8F7, y2 = 0xE4FBBD9B, y3 = 0x454E3F57, y4 = 0xB75540D3, y5 = 0x43E93A4C, y6 = 0x3A6F2AA0, y7 = 0x726D6B36, y8 = 0x9243F484, y9 = 0x9145D1E8, y10 = 0x4FA9D247, y11 = 0xDC8DEE11, y12 = 0x054BF545, y13 = 0x254DD653, y14 = 0xD9421B6D, y15 = 0x67B276C1 }
                        |> Expect.equal { y0 = 0xCCAAF672, y1 = 0x23D960F7, y2 = 0x9153E63A, y3 = 0xCD9A60D0, y4 = 0x50440492, y5 = 0xF07CAD19, y6 = 0xAE344AA0, y7 = 0xDF4CFDFC, y8 = 0xCA531C29, y9 = 0x8E7943DB, y10 = 0xAC1680CD, y11 = 0xD503CA00, y12 = 0xA74B2AD6, y13 = 0xBC331C5C, y14 = 0x1DDA24C7, y15 = 0xEE928277 }
            ]
        , describe "littleendian"
            [ test "0 0 0 0" <|
                \_ ->
                    littleendian 0 0 0 0
                        |> equalHex 0
            , test "86 75 30 9" <|
                \_ ->
                    littleendian 86 75 30 9
                        |> equalHex 0x091E4B56
            , test "255 255 255 250" <|
                \_ ->
                    littleendian 255 255 255 250
                        |> equalHex 0xFAFFFFFF
            ]
        , let
            byte : Fuzzer Int
            byte =
                Fuzz.intRange 0 255
          in
          fuzz2
            (Fuzz.pair byte byte)
            (Fuzz.pair byte byte)
            "littleendian⁻¹"
          <|
            \( a, b ) ( c, d ) ->
                littleendian a b c d
                    |> littleendianInverse
                    |> Expect.equal { z0 = a, z1 = b, z2 = c, z3 = d }
        , describe "Salsa20"
            [ test "0 0 0 ..." <|
                \_ ->
                    let
                        zero : SalsaBlock
                        zero =
                            SalsaBlock 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    in
                    zero
                        |> fromBlock
                        |> salsa20
                        |> toBlock
                        |> Expect.equal zero
            , test "211 159 13 ..." <|
                \_ ->
                    SalsaBlock 211 159 13 115 76 55 82 183 3 117 222 37 191 187 234 136 49 237 179 48 1 106 178 219 175 199 166 48 86 16 179 207 31 240 32 63 15 83 93 161 116 147 48 113 238 55 204 36 79 201 235 79 3 81 156 47 203 26 244 243 88 118 104 54
                        |> fromBlock
                        |> salsa20
                        |> toBlock
                        |> Expect.equal (SalsaBlock 109 42 178 168 156 240 248 238 168 196 190 203 26 110 170 154 29 29 150 26 150 30 235 249 190 163 251 48 69 144 51 57 118 40 152 157 180 57 27 94 107 42 236 35 27 111 114 114 219 236 232 135 111 155 110 18 24 232 95 158 179 19 48 202)

            -- This test takes 4 seconds on good hardware
            -- , test "6 124 83 ..." <|
            --     \_ ->
            --         let
            --             repeat : Int -> (a -> a) -> a -> a
            --             repeat n f x =
            --                 if n <= 0 then
            --                     x
            --                 else
            --                     repeat (n - 1) f (f x)
            --         in
            --         SalsaBlock 6 124 83 146 38 191 9 50 4 161 47 222 122 182 223 185 75 27 0 216 16 122 7 89 162 104 101 147 213 21 54 95 225 253 139 176 105 132 23 116 76 41 176 207 221 34 157 108 94 94 99 52 90 117 91 220 146 190 239 143 196 176 130 186
            --             |> fromBlock
            --             |> repeat 1000000 salsa20
            --             |> toBlock
            --             |> Expect.equal (SalsaBlock 8 18 38 199 119 76 215 67 173 127 144 162 103 212 176 217 192 19 233 33 159 197 154 160 128 243 219 65 171 136 135 225 123 11 68 86 237 82 20 155 133 189 9 83 167 116 194 78 122 127 195 185 185 204 188 90 245 9 183 248 226 85 245 104)
            ]
        , describe "Salsa20_k"
            [ test "salsa20_{1...16, 210...216)(101...116)" <|
                \_ ->
                    let
                        encodeRange : Int -> Int -> Bytes
                        encodeRange from to =
                            List.range from to
                                |> List.map Bytes.Encode.unsignedInt8
                                |> Bytes.Encode.sequence
                                |> Bytes.Encode.encode

                        k0 : Bytes
                        k0 =
                            encodeRange 1 16

                        k1 : Bytes
                        k1 =
                            encodeRange 201 216

                        n : Bytes
                        n =
                            encodeRange 101 116
                    in
                    init
                        { key =
                            [ k0, k1 ]
                                |> List.map Bytes.Encode.bytes
                                |> Bytes.Encode.sequence
                                |> Bytes.Encode.encode
                        , nonce = n
                        }
                        |> Maybe.map toBlock
                        |> Expect.equal (Just <| SalsaBlock 101 120 112 97 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 110 100 32 51 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 50 45 98 121 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 116 101 32 107)
            ]
        ]


equalHex : Int -> Int -> Expect.Expectation
equalHex expected actual =
    actual
        |> Hex.toString
        |> Expect.equal (Hex.toString expected)


toBlock : Int32_16 -> SalsaBlock
toBlock input =
    let
        z0 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z0 =
            littleendianInverse input.y0

        z1 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z1 =
            littleendianInverse input.y1

        z2 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z2 =
            littleendianInverse input.y2

        z3 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z3 =
            littleendianInverse input.y3

        z4 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z4 =
            littleendianInverse input.y4

        z5 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z5 =
            littleendianInverse input.y5

        z6 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z6 =
            littleendianInverse input.y6

        z7 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z7 =
            littleendianInverse input.y7

        z8 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z8 =
            littleendianInverse input.y8

        z9 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z9 =
            littleendianInverse input.y9

        z10 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z10 =
            littleendianInverse input.y10

        z11 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z11 =
            littleendianInverse input.y11

        z12 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z12 =
            littleendianInverse input.y12

        z13 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z13 =
            littleendianInverse input.y13

        z14 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z14 =
            littleendianInverse input.y14

        z15 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z15 =
            littleendianInverse input.y15
    in
    { x0 = z0.z0
    , x1 = z0.z1
    , x2 = z0.z2
    , x3 = z0.z3
    , x4 = z1.z0
    , x5 = z1.z1
    , x6 = z1.z2
    , x7 = z1.z3
    , x8 = z2.z0
    , x9 = z2.z1
    , x10 = z2.z2
    , x11 = z2.z3
    , x12 = z3.z0
    , x13 = z3.z1
    , x14 = z3.z2
    , x15 = z3.z3
    , x16 = z4.z0
    , x17 = z4.z1
    , x18 = z4.z2
    , x19 = z4.z3
    , x20 = z5.z0
    , x21 = z5.z1
    , x22 = z5.z2
    , x23 = z5.z3
    , x24 = z6.z0
    , x25 = z6.z1
    , x26 = z6.z2
    , x27 = z6.z3
    , x28 = z7.z0
    , x29 = z7.z1
    , x30 = z7.z2
    , x31 = z7.z3
    , x32 = z8.z0
    , x33 = z8.z1
    , x34 = z8.z2
    , x35 = z8.z3
    , x36 = z9.z0
    , x37 = z9.z1
    , x38 = z9.z2
    , x39 = z9.z3
    , x40 = z10.z0
    , x41 = z10.z1
    , x42 = z10.z2
    , x43 = z10.z3
    , x44 = z11.z0
    , x45 = z11.z1
    , x46 = z11.z2
    , x47 = z11.z3
    , x48 = z12.z0
    , x49 = z12.z1
    , x50 = z12.z2
    , x51 = z12.z3
    , x52 = z13.z0
    , x53 = z13.z1
    , x54 = z13.z2
    , x55 = z13.z3
    , x56 = z14.z0
    , x57 = z14.z1
    , x58 = z14.z2
    , x59 = z14.z3
    , x60 = z15.z0
    , x61 = z15.z1
    , x62 = z15.z2
    , x63 = z15.z3
    }


fromBlock : SalsaBlock -> Int32_16
fromBlock input =
    { y0 = littleendian input.x0 input.x1 input.x2 input.x3
    , y1 = littleendian input.x4 input.x5 input.x6 input.x7
    , y2 = littleendian input.x8 input.x9 input.x10 input.x11
    , y3 = littleendian input.x12 input.x13 input.x14 input.x15
    , y4 = littleendian input.x16 input.x17 input.x18 input.x19
    , y5 = littleendian input.x20 input.x21 input.x22 input.x23
    , y6 = littleendian input.x24 input.x25 input.x26 input.x27
    , y7 = littleendian input.x28 input.x29 input.x30 input.x31
    , y8 = littleendian input.x32 input.x33 input.x34 input.x35
    , y9 = littleendian input.x36 input.x37 input.x38 input.x39
    , y10 = littleendian input.x40 input.x41 input.x42 input.x43
    , y11 = littleendian input.x44 input.x45 input.x46 input.x47
    , y12 = littleendian input.x48 input.x49 input.x50 input.x51
    , y13 = littleendian input.x52 input.x53 input.x54 input.x55
    , y14 = littleendian input.x56 input.x57 input.x58 input.x59
    , y15 = littleendian input.x60 input.x61 input.x62 input.x63
    }


{-| 64 bytes.
-}
type alias SalsaBlock =
    { x0 : Int
    , x1 : Int
    , x2 : Int
    , x3 : Int
    , x4 : Int
    , x5 : Int
    , x6 : Int
    , x7 : Int
    , x8 : Int
    , x9 : Int
    , x10 : Int
    , x11 : Int
    , x12 : Int
    , x13 : Int
    , x14 : Int
    , x15 : Int
    , x16 : Int
    , x17 : Int
    , x18 : Int
    , x19 : Int
    , x20 : Int
    , x21 : Int
    , x22 : Int
    , x23 : Int
    , x24 : Int
    , x25 : Int
    , x26 : Int
    , x27 : Int
    , x28 : Int
    , x29 : Int
    , x30 : Int
    , x31 : Int
    , x32 : Int
    , x33 : Int
    , x34 : Int
    , x35 : Int
    , x36 : Int
    , x37 : Int
    , x38 : Int
    , x39 : Int
    , x40 : Int
    , x41 : Int
    , x42 : Int
    , x43 : Int
    , x44 : Int
    , x45 : Int
    , x46 : Int
    , x47 : Int
    , x48 : Int
    , x49 : Int
    , x50 : Int
    , x51 : Int
    , x52 : Int
    , x53 : Int
    , x54 : Int
    , x55 : Int
    , x56 : Int
    , x57 : Int
    , x58 : Int
    , x59 : Int
    , x60 : Int
    , x61 : Int
    , x62 : Int
    , x63 : Int
    }
