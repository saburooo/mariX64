module Main exposing (..)

-- まずは基盤を考えるところから・・・

import Browser
import Browser.Events

import Html exposing (Html)
import Html.Events
import Html.Attributes

import Json.Encode as Encode

import Array

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

import Json.Decode as Decode exposing (Decoder, string)
import Html exposing (s)


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
    = Tick Float -- 恐らく時間
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



init : () -> ( Model, Cmd Msg )
init _ =
    Debug.todo "初期化するもの：プレイヤーの位置、カメラ、地面、敵"

-- UPDATE
update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            Debug.todo "時を動かす"

        KeyDown (Speed s) ->
            Debug.todo "キャラクターの足を動かす。"

        KeyUp (Speed s) ->
            Debug.todo "キャラクターのどこかを動かす。"

        KeyDown (Jump j) ->
            Debug.todo "キャラクターをジャンプさせる。"



-- VIEW
view : Model -> Html Msg
view model =
    Html.div []
        [ Debug.todo "Scene.viewを出す。"]