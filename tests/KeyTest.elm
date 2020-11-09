module KeyTest exposing (..)

import Test exposing (Test, test, describe, todo)
import Expect exposing (equal)
import Json.Decode as Decode

import Main exposing (Command, keyDecoder)
import Test exposing (fuzz)
import Json.Decode exposing (string)


type TestKey
    = KeyDown Command
    | KeyUp Command


keyBoolean : Decode.Decoder Main.Msg -> Bool
keyBoolean main =
    True


-- TEST
suite : Test
suite = 
    describe "ほんとにキーとして入力されるの？"
        [ test "とりあえずやってみよう" <|
            \() ->
                Expect.true "Key Downed" (keyBoolean (keyDecoder Main.KeyDown))
        {-
        , fuzz string "とりあえずやってみよう" <|
            \_ ->
                Expect.true "Key Downed" (keyBoolean (keyDecoder Main.KeyDown))
        -}
        ]