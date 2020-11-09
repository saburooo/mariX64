module Main exposing (..)

-- まずは基盤を考えるところから・・・

import Browser
import Browser.Events
import Browser.Dom

import Task

import Html exposing (Html)
import Html.Events
import Html.Attributes

import Json.Encode as Encode

import Array

import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World exposing (World)

import Scene3d
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh exposing (Mesh)
import Scene3d.Light exposing (fluorescent)

import Color
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

import Json.Decode as Decode exposing (Decoder, string)
import Html exposing (s)
import Length exposing (Length)
import Acceleration exposing (Acceleration)
import Block3d exposing (Block3d)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none)
        , subscriptions = subscriptions
        }


-- MODEL


type alias Data =
    { name : String
    }


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { world : World Data
    , exposureValue : Float
    , fps : List Float
    , speeding : Float
    , steeting : Float 
    , jumping : Float 
    , azimuth : Angle -- 方位
    , elevation : Angle -- 高さ（ぐぐったら気高さとか高尚とか出てきた。)
    }


type Unit
    = X Basics.Float 
    | Y Basics.Float
    | Z Basics.Float


type ViewPoint
    = ViewX Float
    | ViewY Float
    | ViewZ Float


type Command
    = Speed Float
    | Steer Float
    | Jump Float


-- Msgには何がいるのか
type Msg
    = Tick Float -- 恐らく時間
    | KeyDown Command
    | KeyUp Command
    | Restart




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



measureSize : (Float -> Float -> msg) -> Cmd msg
measureSize fn =
    Task.perform
        (\{ viewport } -> fn viewport.width viewport.height)
        Browser.Dom.getViewport



init : () -> ( Model, Cmd Msg )
init _ =
    (
    { world=initialWorld
    , fps = []
    , speeding=0
    , steeting=0
    , jumping=0
    , azimuth=Angle.degrees 45
    , elevation=Angle.degrees 45
    , exposureValue= 6
    }
    , Cmd.none
    )



-- UPDATE
update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            Debug.todo "時を動かす"

        KeyDown (Speed k) ->
            { model | speeding=k }

        KeyDown (Steer k) ->
            { model | steeting=k }

        KeyDown (Jump j) ->
            { model | jumping=j } 

        KeyUp (Speed _) ->
            { model | speeding=0}

        KeyUp (Steer _) ->
            { model | steeting=0}

        KeyUp (Jump j) ->
            { model | jumping=-j } 

        Restart ->
            { model | world=initialWorld } 



-- SUBSCRIPTISONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
        ]


-- VIEW

lightBulb : Light WorldCoordinates Bool
lightBulb =
    -- Define a light bulb similar to a 30 watt incandescent light bulb
    Scene3d.Light.point (Scene3d.Light.castsShadows True)
        { chromaticity = Scene3d.Light.incandescent
        , intensity = LuminousFlux.lumens 300
        , position = Point3d.centimeters 0 0 30
        }


overheadLighting : Light WorldCoordinates Never
overheadLighting =
    -- Add some soft overhead lighting
    Scene3d.Light.overhead
        { upDirection = Direction3d.positiveZ
        , chromaticity = Scene3d.Light.fluorescent
        , intensity = Illuminance.lux 100
        }



view : Model -> Html Msg
view model =
    Html.div []
        [ Scene3d.custom -- ここでやっと３Dを実装する。
            { lights = Scene3d.twoLights lightBulb overheadLighting
            , camera = camera model
            , clipDepth = Length.meters 0.1
            , dimensions = ( Pixels.int 640, Pixels.int 480 )
            , antialiasing = Scene3d.multisampling
            , exposure = Scene3d.exposureValue model.exposureValue
            , toneMapping = Scene3d.noToneMapping
            , whiteBalance = fluorescent
            , background = Scene3d.transparentBackground
            , entities =
                [ ultramarineBlueSphere -- このリストに入れたい物質を入れる。
                , nibuiOrangeFloor
                ]
            }
        ]


-- WORLD
initialWorld:World Data
initialWorld=
    World.empty
        |> World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
        |> World.add floorBody -- 床



-- MATERIAL

ultramarineBlue:Material.Uniform WorldCoordinates
ultramarineBlue=
    Material.metal
        { baseColor=Color.rgb255 71 83 162
        , roughness=0.5
        }


nibuiOrange:Material.Uniform WorldCoordinates
nibuiOrange=
    Material.metal
        { baseColor=Color.rgb255 163 105 72
        , roughness=0.5
        }


nibuiOrangeFloor:Scene3d.Entity WorldCoordinates
nibuiOrangeFloor=
    Scene3d.block nibuiOrange <|
        Block3d.from
            (Point3d.centimeters -55 -55 -22)
            (Point3d.centimeters 55 55 -20)


ultramarineBlueSphere:Scene3d.Entity WorldCoordinates
ultramarineBlueSphere=
    Scene3d.sphereWithShadow (Material.uniform ultramarineBlue) <|
        Sphere3d.withRadius (Length.centimeters 10) (Point3d.centimeters 20 20 20)
    


floorOffset:{x:Float, y:Float,z:Float}
floorOffset=
    { x=0, y=0, z=-1 }



floorBody:Body Data
floorBody =
    Body.plane { name="floorBody" }
        |> Body.moveTo (Point3d.fromMeters floorOffset)



-- RENDERING


camera : Model -> Camera3d Meters WorldCoordinates
camera model =
    -- 俯瞰する類のカメラかな？
    -- the model
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.centimeters 0 0 -30
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 3
                }
        , verticalFieldOfView = Angle.degrees 30
        }
