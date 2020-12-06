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

import Physics.Body exposing (Body, compound, frame, data, applyForce, translateBy, plane)
import Physics.Body as Body
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World exposing (World, RaycastResult)
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
import Length exposing (Meters, meters, millimeters)
import Viewpoint3d exposing (Viewpoint3d)
import Angle exposing (Angle)
import Scene3d.Light exposing (Light)
import Quantity exposing (Quantity)
import Pixels exposing (Pixels, pixels)
import LuminousFlux
import Illuminance

import Point2d
import Plane3d

import Rectangle2d

import Direction3d
import Frame3d exposing (Frame3d)
import Vector3d exposing (Vector3d)

import Json.Decode as Decode exposing (Decoder, string)
import Html
import Length exposing (Length)
import Acceleration exposing (Acceleration)
import Block3d exposing (Block3d)
import Axis3d exposing (Axis3d)
import Html.Attributes exposing (width)
import Html.Attributes exposing (height)
import Html.Attributes exposing (shape)
import Task
import Browser.Dom exposing (Viewport)
import Html.Events
import Direction3d exposing (azimuthIn)
import Point2d exposing (Point2d)



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none)
        , view = view
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
    , vectol : Float
    , maybeRaycastResult : Maybe (RaycastResult Data)
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    }



-- Msgには何がいるのか
type Msg
    = AnimationFrame
    | Resize (Quantity Float Pixels) (Quantity Float Pixels)
    | Restart
    | KeyDown Direction
    | KeyUp Direction


-- KEYCONFIG
type Direction
    = LeftOrRight Float
    | UpOrDown Float
    | Space Float
    | Other


-- TODO どうやって Msg に組み込むか そしてその入力をどうやってプレイヤー・キャラクターに反映させるのか？
keyDecoder : (Direction -> Msg) -> Decode.Decoder Msg
keyDecoder toMsg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ArrowLeft" ->
                        Decode.succeed (toMsg (LeftOrRight -1))

                    "ArrowRight" ->
                        Decode.succeed (toMsg (LeftOrRight 1))

                    "ArrowUp" ->
                        Decode.succeed (toMsg (UpOrDown 1))

                    "ArrowDown" ->
                        Decode.succeed (toMsg (UpOrDown -1))

                    "Space" ->
                        Decode.succeed (toMsg (Space 1))

                    _ ->
                        Decode.fail ("どこのキー押しとんじゃい: " ++ string)
            )

{--
toDirection : String -> Direction
toDirection string =
  case string of
    "ArrowLeft" ->
        Left 1

    "ArrowRight" ->
        Right 1

    "ArrowUp" ->
        Up 1

    "ArrowDown" ->
        Down 1

    "Space" ->
        Space 1

    _ ->
        Other
--}


init : () -> ( Model, Cmd Msg )
init _ =
    ({ world=initialWorld
    , width=pixels 0
    , height=pixels 0
    , maybeRaycastResult = Nothing
    , exposureValue = 0
    , vectol = 0
    }
    , Task.perform
        (\{ viewport } ->
            Resize (pixels viewport.width) (pixels viewport.height)
        )
        Browser.Dom.getViewport
    )



-- WORLD
initialWorld: World Data
initialWorld=
    World.empty
        |> World.withGravity 
            (Acceleration.metersPerSecondSquared 9.80665)
            Direction3d.negativeZ
        |> World.add (player 0)
        |> World.add floorBody



-- Worldに召喚されるもの
-- MATERIAL
player: Float -> Body Data
player speed =
    let
        heroModel =
            Block3d.centeredOn Frame3d.atOrigin
                (millimeters 185, millimeters 185, millimeters 185)
    in
    plane
        { id = Player
        , entity=
            heroModel
                |> Scene3d.block (Material.metal { baseColor = Color.blue, roughness = 0.7 })
                |> Scene3d.translateBy (Vector3d.millimeters (10 + speed) (10 + speed) -10)
            }




floorBody : Body Data
floorBody =
    let
        shape =
            Block3d.centeredOn Frame3d.atOrigin
                ( meters 25, meters 25, millimeters 10 )
    in
    plane
        { id = Floor
        , entity =
            shape
                |> Scene3d.block (Material.matte Color.darkCharcoal)
                |> Scene3d.translateBy (Vector3d.millimeters 0 0 -85)
        }


-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        AnimationFrame ->
            { model
                | world =
                    let
                        baseFrame = 
                            model.world
                                |> World.bodies
                                |> List.filter (\b -> (data b).id == Player)
                                |> List.head
                                |> Maybe.map (\b -> Body.frame b)
                                |> Maybe.withDefault Frame3d.atOrigin
                    in
                    model.world
                        |> World.constrain (constainPlayer model.vectol)
                        |> World.update (applySpeed model.vectol baseFrame)
                        |> World.simulate (seconds (1 / 60))
            }

        Resize width height ->
            { model | width = width, height = height }

        -- 書き直し箇所その2 ここをelm-physicsのLack.elのupdate関数を参考に書き換える
        Restart ->
            { model | world=initialWorld } 

        KeyDown Other ->
            { model | vectol=0 }

        KeyDown (Space k) ->
            { model | vectol=k }

        KeyDown (LeftOrRight k) ->
            { model | vectol=k }

        KeyDown (UpOrDown k) ->
            { model | vectol=k }

        KeyUp (LeftOrRight _) ->
            { model | vectol=0 }

        KeyUp (UpOrDown _) ->
            { model | vectol=0 }

        KeyUp (Space _) ->
            { model | vectol=0 }

        KeyUp Other ->
            { model | vectol=0 }



-- SUBSCRIPTISONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize
            (\width height ->
                Resize (pixels (toFloat width)) (pixels (toFloat height))
            )
        , Browser.Events.onAnimationFrame (\_ -> AnimationFrame)
        , Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
        ]


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


-- VIEW


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
    Html.div [ ]
            [ Scene3d.sunny
                { upDirection = Direction3d.z
                , sunlightDirection = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
                , shadows = True
                , camera = camera
                , dimensions =
                    ( Pixels.int (round (Pixels.toFloat model.width))
                    , Pixels.int (round (Pixels.toFloat model.height))
                    )
                , background = Scene3d.transparentBackground
                , clipDepth = Length.meters 0.1
                , entities = entities
                }
            ]


-- RENDERING
constainPlayer : Float -> Body Data -> Body Data -> List Constraint 
constainPlayer vectol b1 b2 =
    let
        vectolAngle =
            vectol * pi / 8
        
        dx = 
            cos vectolAngle

        dy = 
            sin vectolAngle

        hingee =
            hinge
                (Axis3d.through
                    (Point3d.meters 3 3 0)
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )
                (Axis3d.through
                    (Point3d.meters 0 0 0)
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
    in
    case ( (Body.data b1).id, (Body.data b2).id ) of
        ( Player, Key ) ->
            [ hingee ]

        _ ->
            []





applySpeed : Float -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> Body Data
applySpeed speed baseFrame body =
    if speed /= 0 then
        let
            forward =
                Frame3d.yDirection baseFrame

            up =
                Frame3d.zDirection baseFrame

            wheelPoint =
                Frame3d.originPoint (Body.frame body)

            pointOnTheWheel =
                wheelPoint |> Point3d.translateBy (Vector3d.withLength (Length.meters 1.2) up)

            pointUnderTheWheel =
                wheelPoint |> Point3d.translateBy (Vector3d.withLength (Length.meters 1.2) (Direction3d.reverse up))
        in
        body
            |> Body.applyForce (Force.newtons (-speed * 100)) forward pointOnTheWheel
            |> Body.applyForce (Force.newtons (-speed * 100)) (Direction3d.reverse forward) pointUnderTheWheel

    else
        body



camera : Camera3d Meters WorldCoordinates
camera =
    -- 俯瞰する類のカメラかな？
    -- the model
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters 3 4 2
                , focalPoint = Point3d.meters -0.5 -0.5 0
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 30
        }
