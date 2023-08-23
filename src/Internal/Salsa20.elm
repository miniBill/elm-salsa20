module Internal.Salsa20 exposing (columnround, doubleround, plus, quarterround, rotateLeftBy, rowround, salsa20, xor)

import Bitwise


plus : Int -> Int -> Int
plus a b =
    (a + b)
        |> Bitwise.shiftRightZfBy 0


xor : Int -> Int -> Int
xor a b =
    Bitwise.xor a b
        |> Bitwise.shiftRightZfBy 0


rotateLeftBy : Int -> Int -> Int
rotateLeftBy c v =
    let
        high : Int
        high =
            Bitwise.shiftLeftBy c v
                |> Bitwise.shiftRightZfBy 0

        low : Int
        low =
            Bitwise.shiftRightZfBy (32 - c) v
    in
    low + high


quarterround : Int -> Int -> Int -> Int -> { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
quarterround y0 y1 y2 y3 =
    let
        z1 : Int
        z1 =
            y1 |> xor (plus y0 y3 |> rotateLeftBy 7)

        z2 : Int
        z2 =
            y2 |> xor (plus z1 y0 |> rotateLeftBy 9)

        z3 : Int
        z3 =
            y3 |> xor (plus z2 z1 |> rotateLeftBy 13)

        z0 : Int
        z0 =
            y0 |> xor (plus z3 z2 |> rotateLeftBy 18)
    in
    { z0 = z0
    , z1 = z1
    , z2 = z2
    , z3 = z3
    }


type alias Int32_16 =
    { y0 : Int
    , y1 : Int
    , y2 : Int
    , y3 : Int
    , y4 : Int
    , y5 : Int
    , y6 : Int
    , y7 : Int
    , y8 : Int
    , y9 : Int
    , y10 : Int
    , y11 : Int
    , y12 : Int
    , y13 : Int
    , y14 : Int
    , y15 : Int
    }


rowround : Int32_16 -> Int32_16
rowround { y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15 } =
    let
        z0_3 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z0_3 =
            quarterround y0 y1 y2 y3

        z5_4 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z5_4 =
            quarterround y5 y6 y7 y4

        z10_9 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z10_9 =
            quarterround y10 y11 y8 y9

        z15_14 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z15_14 =
            quarterround y15 y12 y13 y14
    in
    { y0 = z0_3.z0
    , y1 = z0_3.z1
    , y2 = z0_3.z2
    , y3 = z0_3.z3
    , y5 = z5_4.z0
    , y6 = z5_4.z1
    , y7 = z5_4.z2
    , y4 = z5_4.z3
    , y10 = z10_9.z0
    , y11 = z10_9.z1
    , y8 = z10_9.z2
    , y9 = z10_9.z3
    , y15 = z15_14.z0
    , y12 = z15_14.z1
    , y13 = z15_14.z2
    , y14 = z15_14.z3
    }


columnround : Int32_16 -> Int32_16
columnround { y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15 } =
    let
        z0_12 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z0_12 =
            quarterround y0 y4 y8 y12

        z5_1 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z5_1 =
            quarterround y5 y9 y13 y1

        z10_6 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z10_6 =
            quarterround y10 y14 y2 y6

        z15_11 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z15_11 =
            quarterround y15 y3 y7 y11
    in
    { y0 = z0_12.z0
    , y4 = z0_12.z1
    , y8 = z0_12.z2
    , y12 = z0_12.z3
    , y5 = z5_1.z0
    , y9 = z5_1.z1
    , y13 = z5_1.z2
    , y1 = z5_1.z3
    , y10 = z10_6.z0
    , y14 = z10_6.z1
    , y2 = z10_6.z2
    , y6 = z10_6.z3
    , y15 = z15_11.z0
    , y3 = z15_11.z1
    , y7 = z15_11.z2
    , y11 = z15_11.z3
    }


doubleround : Int32_16 -> Int32_16
doubleround x =
    rowround (columnround x)


salsa20 : Int32_16 -> Int32_16
salsa20 x =
    plus32_16 x (doubleround_n 10 x)


doubleround_n : Int -> Int32_16 -> Int32_16
doubleround_n n x =
    if n <= 0 then
        x

    else
        doubleround_n (n - 1) (doubleround x)


plus32_16 : Int32_16 -> Int32_16 -> Int32_16
plus32_16 arg1 arg2 =
    { y0 = plus arg1.y0 arg2.y0
    , y1 = plus arg1.y1 arg2.y1
    , y2 = plus arg1.y2 arg2.y2
    , y3 = plus arg1.y3 arg2.y3
    , y4 = plus arg1.y4 arg2.y4
    , y5 = plus arg1.y5 arg2.y5
    , y6 = plus arg1.y6 arg2.y6
    , y7 = plus arg1.y7 arg2.y7
    , y8 = plus arg1.y8 arg2.y8
    , y9 = plus arg1.y9 arg2.y9
    , y10 = plus arg1.y10 arg2.y10
    , y11 = plus arg1.y11 arg2.y11
    , y12 = plus arg1.y12 arg2.y12
    , y13 = plus arg1.y13 arg2.y13
    , y14 = plus arg1.y14 arg2.y14
    , y15 = plus arg1.y15 arg2.y15
    }
