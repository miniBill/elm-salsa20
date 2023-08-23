module Paper exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Hex
import Internal.Salsa20
import Salsa20 exposing (SalsaBlock)
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
                    Internal.Salsa20.doubleround { y0 = 0x01, y1 = 0, y2 = 0, y3 = 0, y4 = 0, y5 = 0, y6 = 0, y7 = 0, y8 = 0, y9 = 0, y10 = 0, y11 = 0, y12 = 0, y13 = 0, y14 = 0, y15 = 0x00 }
                        |> Expect.equal { y0 = 0x8186A22D, y1 = 0x0040A284, y2 = 0x82479210, y3 = 0x06929051, y4 = 0x08000090, y5 = 0x02402200, y6 = 0x4000, y7 = 0x00800000, y8 = 0x00010200, y9 = 0x20400000, y10 = 0x08008104, y11 = 0x00, y12 = 0x20500000, y13 = 0xA0000040, y14 = 0x0008180A, y15 = 0x612A8020 }
            , test "doubleround2" <|
                \_ ->
                    Internal.Salsa20.doubleround { y0 = 0xDE501066, y1 = 0x6F9EB8F7, y2 = 0xE4FBBD9B, y3 = 0x454E3F57, y4 = 0xB75540D3, y5 = 0x43E93A4C, y6 = 0x3A6F2AA0, y7 = 0x726D6B36, y8 = 0x9243F484, y9 = 0x9145D1E8, y10 = 0x4FA9D247, y11 = 0xDC8DEE11, y12 = 0x054BF545, y13 = 0x254DD653, y14 = 0xD9421B6D, y15 = 0x67B276C1 }
                        |> Expect.equal { y0 = 0xCCAAF672, y1 = 0x23D960F7, y2 = 0x9153E63A, y3 = 0xCD9A60D0, y4 = 0x50440492, y5 = 0xF07CAD19, y6 = 0xAE344AA0, y7 = 0xDF4CFDFC, y8 = 0xCA531C29, y9 = 0x8E7943DB, y10 = 0xAC1680CD, y11 = 0xD503CA00, y12 = 0xA74B2AD6, y13 = 0xBC331C5C, y14 = 0x1DDA24C7, y15 = 0xEE928277 }
            ]
        , describe "littleendian"
            [ test "0 0 0 0" <|
                \_ ->
                    Internal.Salsa20.littleendian 0 0 0 0
                        |> equalHex 0
            , test "86 75 30 9" <|
                \_ ->
                    Internal.Salsa20.littleendian 86 75 30 9
                        |> equalHex 0x091E4B56
            , test "255 255 255 250" <|
                \_ ->
                    Internal.Salsa20.littleendian 255 255 255 250
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
                Internal.Salsa20.littleendian a b c d
                    |> Internal.Salsa20.littleendianInverse
                    |> Expect.equal { z0 = a, z1 = b, z2 = c, z3 = d }
        , describe "Salsa20"
            [ test "0 0 0 ..." <|
                \_ ->
                    let
                        zero : SalsaBlock
                        zero =
                            { x0 = 0, x1 = 0, x2 = 0, x3 = 0, x4 = 0, x5 = 0, x6 = 0, x7 = 0, x8 = 0, x9 = 0, x10 = 0, x11 = 0, x12 = 0, x13 = 0, x14 = 0, x15 = 0, x16 = 0, x17 = 0, x18 = 0, x19 = 0, x20 = 0, x21 = 0, x22 = 0, x23 = 0, x24 = 0, x25 = 0, x26 = 0, x27 = 0, x28 = 0, x29 = 0, x30 = 0, x31 = 0, x32 = 0, x33 = 0, x34 = 0, x35 = 0, x36 = 0, x37 = 0, x38 = 0, x39 = 0, x40 = 0, x41 = 0, x42 = 0, x43 = 0, x44 = 0, x45 = 0, x46 = 0, x47 = 0, x48 = 0, x49 = 0, x50 = 0, x51 = 0, x52 = 0, x53 = 0, x54 = 0, x55 = 0, x56 = 0, x57 = 0, x58 = 0, x59 = 0, x60 = 0, x61 = 0, x62 = 0, x63 = 0 }
                    in
                    zero
                        |> Salsa20.salsa20
                        |> Expect.equal zero
            , test "211 159 13 ..." <|
                \_ ->
                    { x0 = 211, x1 = 159, x2 = 13, x3 = 115, x4 = 76, x5 = 55, x6 = 82, x7 = 183, x8 = 3, x9 = 117, x10 = 222, x11 = 37, x12 = 191, x13 = 187, x14 = 234, x15 = 136, x16 = 49, x17 = 237, x18 = 179, x19 = 48, x20 = 1, x21 = 106, x22 = 178, x23 = 219, x24 = 175, x25 = 199, x26 = 166, x27 = 48, x28 = 86, x29 = 16, x30 = 179, x31 = 207, x32 = 31, x33 = 240, x34 = 32, x35 = 63, x36 = 15, x37 = 83, x38 = 93, x39 = 161, x40 = 116, x41 = 147, x42 = 48, x43 = 113, x44 = 238, x45 = 55, x46 = 204, x47 = 36, x48 = 79, x49 = 201, x50 = 235, x51 = 79, x52 = 3, x53 = 81, x54 = 156, x55 = 47, x56 = 203, x57 = 26, x58 = 244, x59 = 243, x60 = 88, x61 = 118, x62 = 104, x63 = 54 }
                        |> Salsa20.salsa20
                        |> Expect.equal { x0 = 109, x1 = 42, x2 = 178, x3 = 168, x4 = 156, x5 = 240, x6 = 248, x7 = 238, x8 = 168, x9 = 196, x10 = 190, x11 = 203, x12 = 26, x13 = 110, x14 = 170, x15 = 154, x16 = 29, x17 = 29, x18 = 150, x19 = 26, x20 = 150, x21 = 30, x22 = 235, x23 = 249, x24 = 190, x25 = 163, x26 = 251, x27 = 48, x28 = 69, x29 = 144, x30 = 51, x31 = 57, x32 = 118, x33 = 40, x34 = 152, x35 = 157, x36 = 180, x37 = 57, x38 = 27, x39 = 94, x40 = 107, x41 = 42, x42 = 236, x43 = 35, x44 = 27, x45 = 111, x46 = 114, x47 = 114, x48 = 219, x49 = 236, x50 = 232, x51 = 135, x52 = 111, x53 = 155, x54 = 110, x55 = 18, x56 = 24, x57 = 232, x58 = 95, x59 = 158, x60 = 179, x61 = 19, x62 = 48, x63 = 202 }
            ]
        ]


equalHex : Int -> Int -> Expect.Expectation
equalHex expected actual =
    actual
        |> Hex.toString
        |> Expect.equal (Hex.toString expected)
