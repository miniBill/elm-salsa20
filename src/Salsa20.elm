module Salsa20 exposing
    ( salsa20
    , SalsaBlock
    )

{-|

@docs salsa20

#Types

@docs SalsaBloc

-}

import Internal.Salsa20 exposing (Int32_16)


{-| Run the Salsa20 function.
-}
salsa20 : SalsaBlock -> SalsaBlock
salsa20 input =
    input
        |> fromBlock
        |> Internal.Salsa20.salsa20_16
        |> toBlock


toBlock : Int32_16 -> SalsaBlock
toBlock input =
    let
        z0 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z0 =
            Internal.Salsa20.littleendianInverse input.y0

        z1 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z1 =
            Internal.Salsa20.littleendianInverse input.y1

        z2 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z2 =
            Internal.Salsa20.littleendianInverse input.y2

        z3 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z3 =
            Internal.Salsa20.littleendianInverse input.y3

        z4 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z4 =
            Internal.Salsa20.littleendianInverse input.y4

        z5 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z5 =
            Internal.Salsa20.littleendianInverse input.y5

        z6 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z6 =
            Internal.Salsa20.littleendianInverse input.y6

        z7 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z7 =
            Internal.Salsa20.littleendianInverse input.y7

        z8 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z8 =
            Internal.Salsa20.littleendianInverse input.y8

        z9 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z9 =
            Internal.Salsa20.littleendianInverse input.y9

        z10 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z10 =
            Internal.Salsa20.littleendianInverse input.y10

        z11 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z11 =
            Internal.Salsa20.littleendianInverse input.y11

        z12 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z12 =
            Internal.Salsa20.littleendianInverse input.y12

        z13 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z13 =
            Internal.Salsa20.littleendianInverse input.y13

        z14 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z14 =
            Internal.Salsa20.littleendianInverse input.y14

        z15 : { z0 : Int, z1 : Int, z2 : Int, z3 : Int }
        z15 =
            Internal.Salsa20.littleendianInverse input.y15
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
    { y0 = Internal.Salsa20.littleendian input.x0 input.x1 input.x2 input.x3
    , y1 = Internal.Salsa20.littleendian input.x4 input.x5 input.x6 input.x7
    , y2 = Internal.Salsa20.littleendian input.x8 input.x9 input.x10 input.x11
    , y3 = Internal.Salsa20.littleendian input.x12 input.x13 input.x14 input.x15
    , y4 = Internal.Salsa20.littleendian input.x16 input.x17 input.x18 input.x19
    , y5 = Internal.Salsa20.littleendian input.x20 input.x21 input.x22 input.x23
    , y6 = Internal.Salsa20.littleendian input.x24 input.x25 input.x26 input.x27
    , y7 = Internal.Salsa20.littleendian input.x28 input.x29 input.x30 input.x31
    , y8 = Internal.Salsa20.littleendian input.x32 input.x33 input.x34 input.x35
    , y9 = Internal.Salsa20.littleendian input.x36 input.x37 input.x38 input.x39
    , y10 = Internal.Salsa20.littleendian input.x40 input.x41 input.x42 input.x43
    , y11 = Internal.Salsa20.littleendian input.x44 input.x45 input.x46 input.x47
    , y12 = Internal.Salsa20.littleendian input.x48 input.x49 input.x50 input.x51
    , y13 = Internal.Salsa20.littleendian input.x52 input.x53 input.x54 input.x55
    , y14 = Internal.Salsa20.littleendian input.x56 input.x57 input.x58 input.x59
    , y15 = Internal.Salsa20.littleendian input.x60 input.x61 input.x62 input.x63
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
