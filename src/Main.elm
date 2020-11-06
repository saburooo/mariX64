module Main exposing (main)

-- まずは基盤を考えるところから・・・

import Browser
import Browser.Events

import Html exposing (Html)
import Html.Events
import Html.Attributes

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

import Array

import Block3d
import Acceleration
import Frame3d
import Mass

import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World exposing (World)

import Scene3d.Mesh

import Scene3d.Material as Material exposing (Material)
import Color
import Scene3d
import Sphere3d exposing (Sphere3d)
import Point3d exposing (Point3d)
import Camera3d exposing (Camera3d)
import Length exposing (Meters)
import Viewpoint3d exposing (Viewpoint3d)
import Angle exposing (Angle)
import Scene3d.Light exposing (Light)
import Quantity exposing (Quantity)
import Pixels exposing (Pixels)
import LuminousFlux
import Illuminance

import Direction3d
import Physics.World exposing (World)

import Json.Decode as Decode exposing (Decode, string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Data =
    { name : String
    }


type alias Model =
    { world : World Data
    , fps : List Float
    , camera : Camera3d Unit ViewPoint -- Two arguments
    , speeding : Float 
    , steeting : Float 
    }


type Unit
    = X Basics.Float 
    | Y Basics.Float
    | Z Basics.Float


type ViewPoint
    = ViewX Float
    | ViewY Float
    | ViewZ Float


type alias Position =
    { x : Float
    , y : Float
    , z : Float
    }



type Command
    = Speed Float
    | Steer Float
    | Jump Float


-- Msgには何がいるのか
type Msg
    = ForSettings 
    | Tick Float -- 恐らく時間
    | KeyDown Command
    | KeyUp Command


keyDecoder : (Command -> Msg) -> Decode.Decoder Msg
keyDecoder toMsg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ArrowLeft" ->
                        Decode.succeed (toMsg (Steer -1))

                    "ArrowRight" ->
                        Decode.succeed (toMsg (Steer 1))

                    "ArrowUp" ->
                        Decode.succeed (toMsg (Speed 1))

                    "ArrowDown" ->
                        Decode.succeed (toMsg (Speed -1))

                    "SpaceKey" ->
                        Decode.succeed (toMsg (Jump 1))
                    
                    _ ->
                        Decode.fail ("何ボタンですか？:" ++ string)

            )



-- UPDATE