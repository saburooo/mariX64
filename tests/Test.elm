module Test exposing (..)

import Test.Html
import Test exposing (Test, describe)
import Main exposing (Command, keyDecoder)



-- TEST
suite : Test
suite = 
    describe "ほんとにキーとして入力されるの？"
        [ test "とりあえずやってみよう" <|
            keyDecoder 
        ]