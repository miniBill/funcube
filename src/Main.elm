module Main exposing (..)

import Browser
import Element exposing (Attribute, Element, alignTop, centerY, column, el, fill, height, image, inFront, moveDown, moveRight, moveUp, padding, paddingEach, paragraph, rgb, shrink, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Http exposing (Error(..))
import Iso8601
import Json.Decode as JD
import Model exposing (AntsDTO, AsibDTO, Data, EpsDTO, PaDTO, RfDTO, SwDTO, dataDecoder)
import Time


type Model
    = Loading
    | Error String
    | Loaded Data


type Msg
    = Get
    | GotError String
    | GotData Data


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading, getData )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Get ->
            ( model, getData )

        GotError err ->
            ( Error err, Cmd.none )

        GotData data ->
            ( Loaded data, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 30000 <| \_ -> Get



-- Styling


rythm : number
rythm =
    10


fontSizes : { huge : Attribute msg, normal : Attribute msg }
fontSizes =
    { huge = Font.size 48
    , normal = Font.size 16
    }


mainAttributes : List (Attribute msg)
mainAttributes =
    [ Background.color <| rgb 0 0 0
    , Font.color <| rgb 1 1 1
    , fontSizes.normal
    ]



-- View code


view : Model -> Html Msg
view model =
    Element.layout mainAttributes
        (innerView model)


innerView : Model -> Element Msg
innerView model =
    let
        header =
            wrappedRow [ spacing rythm ]
                [ image []
                    { src = "http://www.amsat-nl.org/status/images/AMSAT-NL-Logo320-transparent.png"
                    , description = "AMSAT-NL logo"
                    }
                , paragraph
                    [ Font.bold
                    , fontSizes.huge
                    , moveUp 20
                    ]
                    [ text "FUNcube-1 FM STATUS" ]
                ]

        inner =
            case model of
                Loading ->
                    text "Loading..."

                Error e ->
                    text <| "There was an error loading the data: " ++ e

                Loaded data ->
                    viewData data
    in
    column
        [ spacing rythm
        , padding rythm
        ]
        [ header
        , inner
        ]


viewData : Data -> Element msg
viewData data =
    wrappedRow [ spacing rythm ]
        [ viewMisc data
        , viewPa data.paDTO
        , viewRf data.rfDTO
        , viewAnts data.antsDTO
        , viewAsib data.asibDTO
        , viewSw data.swDTO
        , viewEps data.epsDTO
        ]


viewMisc : Data -> Element msg
viewMisc data =
    [ ( "Position", viewPosition data )
    , ( "Packet count", viewInt data.packetCount )
    , ( "Frame type", viewInt data.frameType )
    , ( "Satellite time", viewTimeUtc data.satelliteTime )
    , ( "Created date", viewTimeUtc data.createdDate )
    ]
        |> box "Others"


viewEps : EpsDTO -> Element msg
viewEps data =
    [ ( "panelVolts1", viewInt data.panelVolts1 )
    , ( "panelVolts2", viewInt data.panelVolts2 )
    , ( "panelVolts3", viewInt data.panelVolts3 )
    , ( "totPhotoCurr", viewInt data.totPhotoCurr )
    , ( "batteryVolts", viewInt data.batteryVolts )
    , ( "totSystemCurr", viewInt data.totSystemCurr )
    , ( "rebootCount", viewInt data.rebootCount )
    , ( "epsSwErrors", viewInt data.epsSwErrors )
    , ( "boostTemp1", viewInt data.boostTemp1 )
    , ( "boostTemp2", viewInt data.boostTemp2 )
    , ( "boostTemp3", viewInt data.boostTemp3 )
    , ( "batteryTemp", viewInt data.batteryTemp )
    , ( "latchUpCount5v", viewInt data.latchUpCount5v )
    , ( "latchUpCount3v3", viewInt data.latchUpCount3v3 )
    , ( "resetCause", viewInt data.resetCause )
    , ( "pptMode", viewInt data.pptMode )
    ]
        |> box "EPS"


viewAsib : AsibDTO -> Element msg
viewAsib data =
    [ ( "sunSensorX", viewFloat data.sunSensorX )
    , ( "sunSensorY", viewFloat data.sunSensorY )
    , ( "sunSensorZ", viewFloat data.sunSensorZ )
    , ( "solXPlus", viewFloat data.solXPlus )
    , ( "solXMinus", viewFloat data.solXMinus )
    , ( "solYPlus", viewFloat data.solYPlus )
    , ( "solYMinus", viewFloat data.solYMinus )
    , ( "busVolts3v3", viewFloat data.busVolts3v3 )
    , ( "busCurr3v3", viewFloat data.busCurr3v3 )
    , ( "busVolts5", viewFloat data.busVolts5 )
    ]
        |> box "Asib"


viewRf : RfDTO -> Element msg
viewRf data =
    [ ( "rxDoppler", viewInt data.rxDoppler )
    , ( "rxRSSI", viewInt data.rxRSSI )
    , ( "rxTemp", viewFloat data.rxTemp )
    , ( "rxCurr", viewInt data.rxCurr )
    , ( "txBusCurr3v3", viewInt data.txBusCurr3v3 )
    , ( "txBusCurr5v", viewInt data.txBusCurr5v )
    ]
        |> box "Rf"


viewPa : PaDTO -> Element msg
viewPa data =
    [ ( "txRevPwr", viewFloat data.txRevPwr )
    , ( "txFwdPwr", viewFloat data.txFwdPwr )
    , ( "txTemp", viewFloat data.txTemp )
    , ( "txCurr", viewFloat data.txCurr )
    ]
        |> box "Pa"


viewAnts : AntsDTO -> Element msg
viewAnts data =
    [ ( "antTemp0", viewFloat data.antTemp0 )
    , ( "antTemp1", viewFloat data.antTemp1 )
    , ( "antDepl0", viewString data.antDepl0 )
    , ( "antDepl1", viewString data.antDepl1 )
    , ( "antDepl2", viewString data.antDepl2 )
    , ( "antDepl3", viewString data.antDepl3 )
    ]
        |> box "Ants"


viewSw : SwDTO -> Element msg
viewSw data =
    [ ( "dtmfCmdCount", viewInt data.dtmfCmdCount )
    , ( "dtmfLastCmd", viewString data.dtmfLastCmd )
    , ( "dtmfCmdSuccess", viewValid data.dtmfCmdSuccess )
    , ( "dataValidASIB", viewValid data.dataValidASIB )
    , ( "dataValidEPS", viewValid data.dataValidEPS )
    , ( "dataValidPA", viewValid data.dataValidPA )
    , ( "dataValidRF", viewValid data.dataValidRF )
    , ( "dataValidiMTQ", viewValid data.dataValidiMTQ )
    , ( "dataValidAntsBusB", viewValid data.dataValidAntsBusB )
    , ( "dataValidAntsBusA", viewValid data.dataValidAntsBusA )
    , ( "inEclipseMode", viewValid data.inEclipseMode )
    , ( "inSafeMode", viewValid data.inSafeMode )
    , ( "hardwareABFOnOff", viewValid data.hardwareABFOnOff )
    , ( "softwareABFOnOff", viewValid data.softwareABFOnOff )
    , ( "deploymentWait", viewValid data.deploymentWait )
    ]
        |> box "Sw"



-- View utilities


standardCell : List (Attribute msg) -> String -> Element msg
standardCell attrs =
    el ([ centerY, padding <| rythm // 2 ] ++ attrs) << text


viewTimeUtc : Time.Posix -> Element msg
viewTimeUtc value =
    standardCell
        [ Font.family [ Font.monospace ]
        , moveDown 2
        ]
        (Iso8601.fromTime value)


viewInt : Int -> Element msg
viewInt value =
    standardCell [] <| String.fromInt value


viewFloat : Float -> Element msg
viewFloat value =
    standardCell [] <| formatFloat 1 value


viewString : String -> Element msg
viewString =
    standardCell []


formatFloat : Int -> Float -> String
formatFloat precision value =
    let
        stringInt =
            String.fromInt <| round (value * toFloat (10 ^ precision))

        beforeDotRaw =
            String.slice 0 -1 stringInt

        beforeDot =
            case beforeDotRaw of
                "" ->
                    "0"

                "-" ->
                    "-0"

                _ ->
                    beforeDotRaw
    in
    beforeDot ++ "." ++ String.right 1 stringInt


viewValid : Bool -> Element msg
viewValid valid =
    let
        ( bgColor, content ) =
            if valid then
                ( rgb 0.1 0.5 0.1, "Valid" )

            else
                ( rgb 0.5 0.1 0.1, "Invalid" )
    in
    el
        [ width fill
        , height fill
        , Background.color bgColor
        ]
        (standardCell [] content)


viewPosition : { a | latitude : Float, longitude : Float } -> Element msg
viewPosition { latitude, longitude } =
    let
        latitudeString =
            if latitude > 0 then
                formatFloat 1 latitude ++ " N"

            else
                formatFloat 1 -latitude ++ " S"

        longitudeString =
            if longitude > 180 then
                formatFloat 1 (360 - longitude) ++ " W"

            else
                formatFloat 1 longitude ++ " E"
    in
    standardCell [] <| latitudeString ++ ", " ++ longitudeString


box : String -> List ( String, Element msg ) -> Element msg
box label data =
    let
        inner =
            el
                [ Border.width 1
                , height fill
                , paddingEach
                    { left = rythm
                    , top = rythm * 5 // 2
                    , right = rythm
                    , bottom = rythm
                    }
                ]
                (table [ width fill ]
                    { data = data
                    , columns =
                        [ { view =
                                \( rowLabel, _ ) ->
                                    rowLabel
                                        |> text
                                        |> el [ centerY, padding <| rythm // 2 ]
                          , header = Element.none
                          , width = shrink
                          }
                        , { view = Tuple.second
                          , header = Element.none
                          , width = fill
                          }
                        ]
                    }
                )
    in
    el
        [ paddingEach
            { left = 0
            , top = rythm * 4 // 2
            , right = 0
            , bottom = 0
            }
        , inFront <|
            el
                [ moveRight rythm
                , Border.width 1
                , Background.color <| rgb 0 0 0
                , padding rythm
                ]
                (text label)
        , alignTop
        , height fill
        ]
        inner



-- HTTP communication


dataUrl : String
dataUrl =
    if False then
        "http://data.funcube.org.uk/funcube/data/realtime?callback="

    else
        "http://localhost:8000/funcube/data/realtime?callback="


getData : Cmd Msg
getData =
    Http.get
        { url = dataUrl
        , expect =
            Http.expectString
                (Result.mapError httpErrorToString
                    >> Result.andThen
                        (String.slice 1 -1
                            >> JD.decodeString dataDecoder
                            >> Result.mapError JD.errorToString
                        )
                    >> (\r ->
                            case r of
                                Err e ->
                                    GotError e

                                Ok o ->
                                    GotData o
                       )
                )
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    let
        _ =
            case error of
                Timeout ->
                    "Timeout"

                BadUrl _ ->
                    "Bad URL"

                NetworkError ->
                    "Network error"

                BadStatus _ ->
                    "Bad status"

                BadBody _ ->
                    "Bad body"
    in
    "Error in server answer"
