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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type WorldCoordinates
    = WorldCoordinates

-- MATERIALS


-- ボルドー
bordeaux : Material.Uniform WorldCoordinates
bordeaux =
    Material.metal
        { baseColor=Color.rgb255 108 39 45
        , roughness=0.6
        }


-- ボルドーよりあかるーい
lightRed : Material.Uniform WorldCoordinates
lightRed =
    Material.metal
        { baseColor=Color.rgb255 255 92 105
        , roughness=0.6
        }




-- まずは簡単に球体の表示からかな・・

-- ENTITIES
bordeauxSphere:Scene3d.Entity WorldCoordinates
bordeauxSphere =
    Scene3d.sphereWithShadow (Material.uniform bordeaux) <|
        Sphere3d.withRadius (Length.centimeters 10) (Point3d.centimeters 20 20 0)




quad:Scene3d.Entity WorldCoordinates
quad =
    Scene3d.quadWithShadow (Material.uniform lightRed)
        (Point3d.centimeters 10 10 -5)
        (Point3d.centimeters -10 10 0)
        (Point3d.centimeters -10 -10 5)
        (Point3d.centimeters 10 -10 0)


-- RENDERING


camera:Model -> Camera3d Meters WorldCoordinates
camera model =
    -- MODELからカメラを生み出す。
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.centimeters 0 0 -20
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 2
                }
        , verticalFieldOfView = Angle.degrees 30
        }



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

-- TONEMAPPING


type ToneMapping
    = NoToneMapping
    | Reinhard
    | ReinhardPerChannel
    | HableFilmic



-- トーンマッピングの種類と露出値を記録するためのModel
-- MODEL

type alias Model =
    { exposureValue : Float
    , toneMapping : ToneMapping
    , azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    }


type Msg
    = SetExposureValue Float
    | SetToneMapping ToneMapping
    | KeyUp
    | KeyDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)



-- UPDATE
update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model =
    case msg of
        SetExposureValue value ->
             ( { model | exposureValue = value }, Cmd.none )

        SetToneMapping value ->
             ( { model | toneMapping = value }, Cmd.none)

        KeyDown ->
            ( { model | orbiting = True }, Cmd.none )

        KeyUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotationRate =
                        Angle.degrees 0.25 |> Quantity.per Pixels.pixel
                    
                    rotation numPixels = 
                        numPixels |> Quantity.at rotationRate

                    newAzimuth =
                        model.azimuth |> Quantity.minus (rotation dx)
                    
                    newElevation =
                        model.elevation
                            |> Quantity.plus (rotation dy)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ({model|azimuth=newAzimuth, elevation=newElevation}
                ,Cmd.none
                )

            else
                (model, Cmd.none)
                

toneMappingDescription:ToneMapping -> String
toneMappingDescription toneMapping =
    case toneMapping of
        NoToneMapping ->
            "No tone Mapping"
            
        Reinhard ->
            "Reinhard"

        ReinhardPerChannel ->
            "Reinhard per channel"

        HableFilmic ->
            "Hable filmic"



toneMappingOptions : List ToneMapping
toneMappingOptions =
    [ NoToneMapping
    , Reinhard
    , ReinhardPerChannel
    , HableFilmic
    ]


init : () -> ( Model, Cmd Msg )
init () =
    ( { exposureValue = 6
      , toneMapping = NoToneMapping
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 40
      , orbiting = False
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        -- 'Tone mapping' refers to various methods used to map high dynamic
        -- range colors (colors with a wider range of brightnesses than could
        -- be displayed on a standard monitor) into a narrower range of colors
        -- that *can* be displayed, without making most colors very dark;
        -- different kinds of tone mapping apply different kinds of color
        -- transformations
        toneMapping =
            case model.toneMapping of
                NoToneMapping ->
                    Scene3d.noToneMapping

                Reinhard ->
                    Scene3d.reinhardToneMapping 5

                ReinhardPerChannel ->
                    Scene3d.reinhardPerChannelToneMapping 5

                HableFilmic ->
                    Scene3d.hableFilmicToneMapping
    in
    Html.div []
        -- マウス以外にも方向キー等で動かせるようにしたい。
        [ Html.div [ Html.Events.onMouseDown KeyDown ]
            [ Scene3d.custom
                { lights = Scene3d.twoLights lightBulb overheadLighting
                , camera = camera model
                , clipDepth = Length.meters 0.1
                , dimensions = ( Pixels.int 640, Pixels.int 480 )
                , antialiasing = Scene3d.multisampling
                , exposure = Scene3d.exposureValue model.exposureValue
                , toneMapping = toneMapping
                , whiteBalance = Scene3d.Light.fluorescent
                , background = Scene3d.transparentBackground
                , entities =
                    [ bordeauxSphere
                    , quad
                    ]
                }
            ]

        -- Allow exposure value and tone mapping type to be adjusted at runtime; try
        -- playing around with different combinations of values! See below for
        -- 'slider' and 'comboBox' implementations if you're curious
        , Html.div []
            [ Html.text "Exposure value:"
            , slider [] { min = 3, max = 9 } model.exposureValue
                |> Html.map SetExposureValue
            ]
        , Html.div []
            [ Html.text "Tone mapping:"
            , comboBox [] toneMappingDescription toneMappingOptions model.toneMapping
                |> Html.map SetToneMapping
            ]
        ]


{-| Decode mouse movement just like in OrbitingCamera example
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


-- SUBSCRIPTION

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed KeyUp)
            ]

    else
        -- Unlike OrbitingCamera example, don't listen for mouse-down events
        -- anywhere since then things like dragging the slider will also cause
        -- orbiting (a mouse down handler is added to the scene itself instead)
        Sub.none

-- HTML UTILITIES


slider :
    List (Html.Attribute Float)
    -> { min : Float, max : Float }
    -> Float
    -> Html Float
slider attributes { min, max } =
    let
        targetValueDecoder currentValue =
            Decode.map (String.toFloat >> Maybe.withDefault currentValue)
                Html.Events.targetValue

        newValueDecoder currentValue =
            targetValueDecoder currentValue
                |> Decode.andThen
                    (\newValue ->
                        if newValue /= currentValue then
                            Decode.succeed newValue

                        else
                            Decode.fail "value did not change"
                    )

        commonAttributes =
            Html.Attributes.property "min" (Encode.float min)
                :: Html.Attributes.property "max" (Encode.float max)
                :: Html.Attributes.property "step" (Encode.string "any")
                :: attributes
    in
    \currentValue ->
        Html.input
            (Html.Attributes.type_ "range"
                :: Html.Attributes.property "value" (Encode.float currentValue)
                :: Html.Events.on "input" (newValueDecoder currentValue)
                :: Html.Events.on "change" (newValueDecoder currentValue)
                :: commonAttributes
            )
            []


comboBox : List (Html.Attribute a) -> (a -> String) -> List a -> a -> Html a
comboBox attributes toStr allItems =
    let
        itemsArray =
            Array.fromList allItems

        selectedIndexDecoder =
            Decode.at [ "target", "selectedIndex" ] Decode.int

        newSelectionDecoder currentItem =
            selectedIndexDecoder
                |> Decode.andThen
                    (\selectedIndex ->
                        case Array.get selectedIndex itemsArray of
                            Just newItem ->
                                if newItem /= currentItem then
                                    Decode.succeed newItem

                                else
                                    Decode.fail "selected item did not change"

                            Nothing ->
                                Decode.fail "selected index out of range"
                    )
    in
    \currentItem ->
        let
            decoder =
                newSelectionDecoder currentItem

            onChange =
                Html.Events.on "change" decoder

            onKeyUp =
                Html.Events.on "keyup" decoder

            toOption item =
                Html.option [ Html.Attributes.selected (item == currentItem) ]
                    [ Html.text (toStr item) ]
        in
        Html.select (onChange :: onKeyUp :: attributes)
            (List.map toOption allItems)
