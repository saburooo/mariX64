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
    , maybeRaycastResult : Maybe (RaycastResult Data)
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    }



-- Msgには何がいるのか
type Msg
    = AnimationFrame
    | Resize (Quantity Float Pixels) (Quantity Float Pixels)
    | KeyDown (Axis3d Meters WorldCoordinates)
    | KeyUp
    | Restart




init : () -> ( Model, Cmd Msg )
init _ =
    ({ world=initialWorld
    , width=pixels 0
    , height=pixels 0
    , maybeRaycastResult = Nothing
    , exposureValue = 0
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
        |> World.add player
        |> World.add floorBody



-- Worldに召喚されるもの
-- MATERIAL
player:Body Data
player=
    let
        heroModel =
            Block3d.centeredOn Frame3d.atOrigin
                (millimeters 85, millimeters 85, millimeters 85)
    in
    plane
        { id = Player
        , entity=
            heroModel
                |> Scene3d.block (Material.metal { baseColor = Color.blue, roughness = 0.7 })
                |> Scene3d.translateBy (Vector3d.millimeters 10 10 -10)
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


key:Body Data
key =
    compound []
        { id = Key
        , entity =
            Scene3d.sphere (Material.matte Color.white)
                (Sphere3d.atOrigin (millimeters 20))}


decodeKeyRay:
    Camera3d Meters WorldCoordinates
    -> Quantity Float Pixels
    -> Quantity Float Pixels
    -> (Axis3d Meters WorldCoordinates -> msg)
    -> Decode.Decoder msg
decodeKeyRay camera3d width height rayToMsg =
    Decode.map2
        (\x y ->
            rayToMsg
                (Camera3d.ray
                    camera3d
                    (Rectangle2d.with
                        { x1 = pixels 0
                        , y1 = height
                        , x2 = width
                        , y2 = pixels 0
                        }
                    )
                    (Point2d.pixels x y)
                )
        )
        -- おそらくここにイベントリスナを書く
        (Decode.field "leftKey" Decode.float)
        (Decode.field "rightKey" Decode.float)



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        AnimationFrame ->
            { model | world = World.simulate (seconds (1 / 60)) model.world }

        Resize width height ->
            { model | width = width, height = height }

        -- 書き直し箇所その2 ここをelm-physicsのLack.elのupdate関数を参考に書き換える
        KeyDown keyRay ->
            case model.maybeRaycastResult of
                Just raycastResult ->
                    let
                        worldPoint =
                            Point3d.placeIn
                                (Body.frame raycastResult.body)
                                raycastResult.point

                        plane =
                            Plane3d.through
                                worldPoint
                                (Viewpoint3d.viewDirection (Camera3d.viewpoint camera))
                    in
                    { model
                        | world =
                            World.update
                                (\body ->
                                    if (Body.data body).id == Key then
                                        case Axis3d.intersectionWithPlane plane keyRay of
                                            Just intersection ->
                                                Body.moveTo intersection body

                                            Nothing ->
                                                body

                                    else
                                        body
                                )
                                model.world
                    }

                Nothing ->
                    model

        KeyUp ->
            { model
                | maybeRaycastResult = Nothing
                , world =
                    World.keepIf
                        (\body -> (Body.data body).id /= Key)
                        model.world
            }

        Restart ->
            { model | world=initialWorld } 



-- SUBSCRIPTISONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize
            (\width height ->
                Resize (pixels (toFloat width)) (pixels (toFloat height))
            )
        , Browser.Events.onAnimationFrame (\_ -> AnimationFrame)
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
