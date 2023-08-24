module TestVectors exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Encode
import Expect
import Hex.Convert
import Salsa20
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        input : Bytes
        input =
            List.range 1 512
                |> List.map (\_ -> Bytes.Encode.unsignedInt8 0)
                |> Bytes.Encode.sequence
                |> Bytes.Encode.encode
    in
    [ { key = "8000000000000000000000000000000000000000000000000000000000000000"
      , iv = "0000000000000000"
      , output =
            [ "E3BE8FDD8BECA2E3EA8EF9475B29A6E7003951E1097A5C38D23B7A5FAD9F6844"
            , "B22C97559E2723C7CBBD3FE4FC8D9A0744652A83E72A9C461876AF4D7EF1A117"
            , "8DA2B74EEF1B6283E7E20166ABCAE538E9716E4669E2816B6B20C5C356802001"
            , "CC1403A9A117D12A2669F456366D6EBB0F1246F1265150F793CDB4B253E348AE"
            , "203D89BC025E802A7E0E00621D70AA36B7E07CB1E7D5B38D5E222B8B0E4B8407"
            , "0142B1E29504767D76824850320B5368129FDD74E861B498E3BE8D16F2D7D169"
            , "57BE81F47B17D9AE7C4FF15429A73E10ACF250ED3A90A93C711308A74C6216A9"
            , "ED84CD126DA7F28E8ABF8BB63517E1CA98E712F4FB2E1A6AED9FDC73291FAA17"
            , "958211C4BA2EBD5838C635EDB81F513A91A294E194F1C039AEEC657DCE40AA7E"
            , "7C0AF57CACEFA40C9F14B71A4B3456A63E162EC7D8D10B8FFB1810D71001B618"
            , "2F9F73DA53B85405C11F7B2D890FA8AE0C7F2E926D8A98C7EC4E91B65120E988"
            , "349631A700C6FACEC3471CB0413656E75E309456584084D7E12C5B43A41C43ED"
            , "9A048ABD9B880DA65F6A665A20FE7B77CD292FE62CAE644B7F7DF69F32BDB331"
            , "903E6505CE44FDC293920C6A9EC7057E23DF7DAD298F82DDF4EFB7FDC7BFC622"
            , "696AFCFD0CDDCC83C7E77F11A649D79ACDC3354E9635FF137E929933A0BD6F53"
            , "77EFA105A3A4266B7C0D089D08F1E855CC32B15B93784A36E56A76CC64BC8477"
            ]
      }
    ]
        |> List.map
            (\{ key, iv, output } ->
                test (key ++ "/" ++ iv) <|
                    \_ ->
                        case Maybe.andThen Salsa20.toKey (Hex.Convert.toBytes key) of
                            Nothing ->
                                Expect.fail "Could not parse key"

                            Just parsedKey ->
                                case Maybe.andThen Salsa20.toNonce (Hex.Convert.toBytes iv) of
                                    Nothing ->
                                        Expect.fail "Could not parse nonce"

                                    Just parsedNonce ->
                                        Salsa20.salsa20 parsedKey parsedNonce input
                                            |> Hex.Convert.toString
                                            |> Expect.equal (String.concat output)
            )
        |> describe "Salsa20 encryption"
