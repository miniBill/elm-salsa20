module Random.Salsa20 exposing
    ( init, peel
    , step
    )

{-| Random generator API for Salsa20. This should be used with `NoRedInk/elm-random-general` to have a nice API for generating random, cryptographically secure, numbers.

Warning: if you use this module directly, it is _your_ responsibility to make sure you don't call `peel` more than once on the same `State`

@docs State, init, peel

-}

import Internal.Salsa20 exposing (Counter(..), Int32_16)
import Salsa20 exposing (Key, Nonce)


{-| The internal state of the generator. Contains the key, nonce and a counter.
-}
type State
    = State
        { key : Key
        , nonce : Nonce
        , counter : Counter
        }


{-| Initialize the generator. This stores the key and nonce inside the state, and sets the counter to 0.
-}
init : Key -> Nonce -> State
init key nonce =
    State
        { key = key
        , nonce = nonce
        , counter =
            Counter
                { z0 = 0
                , z1 = 0
                }
        }


{-| Gets an `Int` out of the current state. The result will be an unsigned 32 bit integer.
-}
peel : State -> Int
peel (State { key, nonce, counter }) =
    let
        keyBlock : Int32_16
        keyBlock =
            Internal.Salsa20.expand key
                (Internal.Salsa20.nonceAndCounter
                    nonce
                    counter
                )
                |> Internal.Salsa20.salsa20
    in
    keyBlock.y0


{-| Increments the counter.
-}
step : State -> State
step (State { key, nonce, counter }) =
    State
        { key = key
        , nonce = nonce
        , counter = increment counter
        }


increment : Counter -> Counter
increment (Counter { z0, z1 }) =
    if z0 == 0xFFFFFFFF then
        Counter { z0 = 0, z1 = z1 + 1 }

    else
        Counter { z0 = z0 + 1, z1 = z1 }
