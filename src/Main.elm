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

import Physics.Body exposing (Body, frame, data, applyForce, translateBy, plane)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World exposing (World)
import Physics.Constraint exposing (Constraint, hinge)

import Force

import Scene3d
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh exposing (Mesh)
import Scene3d.Light exposing (fluorescent)

import Duration
import Duration exposing (seconds)

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
import Frame3d exposing (Frame3d)
import Vector3d exposing (Vector3d)

import Json.Decode as Decode exposing (Decoder, string)
import Html exposing (s)
import Length exposing (Length)
import Acceleration exposing (Acceleration)
import Block3d exposing (Block3d)
import Axis3d
import Html.Attributes exposing (width)
import Html.Attributes exposing (height)
import Point2d exposing (pixels)
import Point2d exposing (meters)
import Point2d exposing (millimeters)
import Html.Attributes exposing (shape)



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none)
        , subscriptions = subscriptions
        }


-- MODEL


-- IDで役割を分ける。
type Id
    = Key
    | Player
    | Floor


type alias Data =
    { entity : Scene3d.Entity BodyCoordinates
    , id : Id
    }



type alias Model =
    { world : World Data
    , exposureValue : Float
    , speeding : Float
    , steeting : Float 
    , jumping : Float 
    , azimuth : Angle -- 方位
    , elevation : Angle -- 高さ（ぐぐったら気高さとか高尚とか出てきた。)
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    }


type Command
    = Speed Float
    | Steer Float
    | Jump Float



-- Msgには何がいるのか
type Msg
    = AnimationFrame
    | Resize (Quantity Float Pixels) (Quantity Float Pixels)
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


init : () -> ( Model, Cmd Msg )
init _ =
    ({ world=initialWorld
    , speeding=0
    , steeting=0
    , jumping=0
    , azimuth=Angle.degrees 45
    , elevation=Angle.degrees 45
    , exposureValue= 6
    , width=pixels 0
    , height=pixels 0
    }
    , Cmd.none
    )


initialWorld: World Data
initialWorld=
    World.empty
        |> World.withGravity 
            (Acceleration.metersPerSecondSquared 9.80665)
            Direction3d.negativeZ
        |> World.add player
        |> World.add floor


-- Worldに召喚されるもの
player:Body Data
player=
    let
        heroModel =
            Block3d.centeredOn Frame3d.atOrigin
                (millimeters 35, millimeters 35, millimeters 15)
    in
    plane
        { id = Player
        , entity=
            shape
                |> Scene3d.block (Material.matte Color.blue)
                |> Scene3d.translateBy (Vector3d.millimeters 0 0 -5)
            }
    


-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case msg of
        AnimationFrame ->
            { model | world = World.simulate (seconds (1 / 60)) model.world }

        Resize width height ->
            { model | width = width, height = height }

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
    let
        entities =
            List.map
                (\body ->
                    Scene3d.placeIn
                        (frame body)
                        (data body).entity
                )
                (World.bodies model.world)
    in
    Html.div []
        [ Scene3d.custom -- ここでやっと３Dを実装する。
            { lights = Scene3d.twoLights lightBulb overheadLighting
            , camera = camera model
            , clipDepth = Length.meters 0.1
            , dimensions = ( Pixels.int 1280, Pixels.int 640 )
            , antialiasing = Scene3d.multisampling
            , exposure = Scene3d.exposureValue model.exposureValue
            , toneMapping = Scene3d.noToneMapping
            , whiteBalance = fluorescent
            , background = Scene3d.transparentBackground
            , entities = entities
            }
        ]


-- WORLD


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
            (Point3d.centimeters -155 -155 -2)
            (Point3d.centimeters 155 155 0)



ultramarineBlueSphere:Scene3d.Entity WorldCoordinates
ultramarineBlueSphere=
    Scene3d.sphereWithShadow (Material.uniform ultramarineBlue) <|
        Sphere3d.withRadius (Length.centimeters 10) (Point3d.centimeters 20 20 20)
    


floorOffset:{x:Float, y:Float,z:Float}
floorOffset=
    { x=0, y=0, z=-1 }




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
