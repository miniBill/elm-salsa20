module Paper exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Hex
import Internal.Salsa20
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
        ]


equalHex : Int -> Int -> Expect.Expectation
equalHex expected actual =
    actual
        |> Hex.toString
        |> Expect.equal (Hex.toString expected)
