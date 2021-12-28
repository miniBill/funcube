module Main exposing (..)

import Browser
import Element exposing (Attribute, Element, alignTop, centerY, column, el, fill, image, inFront, moveDown, moveRight, moveUp, padding, paddingEach, rgb, row, shrink, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Http exposing (Error(..))
import Iso8601
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline as JD
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
            row [ spacing rythm ]
                [ image []
                    { src = "http://www.amsat-nl.org/status/images/AMSAT-NL-Logo320-transparent.png"
                    , description = "AMSAT-NL logo"
                    }
                , el
                    [ Font.bold
                    , fontSizes.huge
                    , moveUp 20
                    ]
                    (text "FUNcube-1 FM STATUS")
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
        , viewRf data.rfDTO
        , viewPa data.paDTO
        , viewAnts data.antsDTO
        , viewAsib data.asibDTO
        , viewSw data.swDTO
        , viewEps data.epsDTO
        ]


viewMisc : Data -> Element msg
viewMisc data =
    [ ( "Position"
      , (if data.latitude > 0 then
            formatFloat 1 data.latitude ++ " N, "

         else
            formatFloat 1 -data.latitude ++ " S, "
        )
            ++ (if data.longitude > 180 then
                    formatFloat 1 (360 - data.longitude) ++ " W"

                else
                    formatFloat 1 data.longitude ++ " E"
               )
      )
    , ( "Packet count", String.fromInt data.packetCount )
    , ( "Frame type", String.fromInt data.frameType )
    , ( "Satellite time", formatTimeUtc data.satelliteTime )
    , ( "Created date", formatTimeUtc data.createdDate )
    ]
        |> box "Others"


viewEps : EpsDTO -> Element msg
viewEps data =
    [ ( "panelVolts1", String.fromInt data.panelVolts1 )
    , ( "panelVolts2", String.fromInt data.panelVolts2 )
    , ( "panelVolts3", String.fromInt data.panelVolts3 )
    , ( "totPhotoCurr", String.fromInt data.totPhotoCurr )
    , ( "batteryVolts", String.fromInt data.batteryVolts )
    , ( "totSystemCurr", String.fromInt data.totSystemCurr )
    , ( "rebootCount", String.fromInt data.rebootCount )
    , ( "epsSwErrors", String.fromInt data.epsSwErrors )
    , ( "boostTemp1", String.fromInt data.boostTemp1 )
    , ( "boostTemp2", String.fromInt data.boostTemp2 )
    , ( "boostTemp3", String.fromInt data.boostTemp3 )
    , ( "batteryTemp", String.fromInt data.batteryTemp )
    , ( "latchUpCount5v", String.fromInt data.latchUpCount5v )
    , ( "latchUpCount3v3", String.fromInt data.latchUpCount3v3 )
    , ( "resetCause", String.fromInt data.resetCause )
    , ( "pptMode", String.fromInt data.pptMode )
    ]
        |> box "EPS"


viewAsib : AsibDTO -> Element msg
viewAsib data =
    [ ( "sunSensorX", formatFloat 1 data.sunSensorX )
    , ( "sunSensorY", formatFloat 1 data.sunSensorY )
    , ( "sunSensorZ", formatFloat 1 data.sunSensorZ )
    , ( "solXPlus", formatFloat 1 data.solXPlus )
    , ( "solXMinus", formatFloat 1 data.solXMinus )
    , ( "solYPlus", formatFloat 1 data.solYPlus )
    , ( "solYMinus", formatFloat 1 data.solYMinus )
    , ( "busVolts3v3", formatFloat 1 data.busVolts3v3 )
    , ( "busCurr3v3", formatFloat 1 data.busCurr3v3 )
    , ( "busVolts5", formatFloat 1 data.busVolts5 )
    ]
        |> box "Asib"


viewRf : RfDTO -> Element msg
viewRf data =
    [ ( "rxDoppler", String.fromInt data.rxDoppler )
    , ( "rxRSSI", String.fromInt data.rxRSSI )
    , ( "rxTemp", formatFloat 1 data.rxTemp )
    , ( "rxCurr", String.fromInt data.rxCurr )
    , ( "txBusCurr3v3", String.fromInt data.txBusCurr3v3 )
    , ( "txBusCurr5v", String.fromInt data.txBusCurr5v )
    ]
        |> box "Rf"


viewPa : PaDTO -> Element msg
viewPa data =
    [ ( "txRevPwr", formatFloat 1 data.txRevPwr )
    , ( "txFwdPwr", formatFloat 1 data.txFwdPwr )
    , ( "txTemp", formatFloat 1 data.txTemp )
    , ( "txCurr", formatFloat 1 data.txCurr )
    ]
        |> box "Pa"


viewAnts : AntsDTO -> Element msg
viewAnts data =
    [ ( "antTemp0", formatFloat 1 data.antTemp0 )
    , ( "antTemp1", formatFloat 1 data.antTemp1 )
    , ( "antDepl0", data.antDepl0 )
    , ( "antDepl1", data.antDepl1 )
    , ( "antDepl2", data.antDepl2 )
    , ( "antDepl3", data.antDepl3 )
    ]
        |> box "Ants"


viewSw : SwDTO -> Element msg
viewSw data =
    [ ( "dtmfCmdCount", String.fromInt data.dtmfCmdCount )
    , ( "dtmfLastCmd", data.dtmfLastCmd )
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


formatTimeUtc : Time.Posix -> String
formatTimeUtc =
    Iso8601.fromTime


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


viewValid : Bool -> String
viewValid valid =
    if valid then
        "Valid"

    else
        "Invalid"


box : String -> List ( String, String ) -> Element msg
box label data =
    let
        inner =
            el
                [ Border.width 1
                , paddingEach
                    { left = rythm
                    , top = rythm * 5 // 2
                    , right = rythm
                    , bottom = rythm
                    }
                ]
                (table
                    [ width fill
                    , spacing rythm
                    ]
                    { data = data
                    , columns =
                        [ { view =
                                Tuple.first
                                    >> text
                                    >> el [ centerY ]
                          , header = Element.none
                          , width = shrink
                          }
                        , { view =
                                Tuple.second
                                    >> text
                                    >> el
                                        [ Font.alignRight
                                        , width fill
                                        , Font.family [ Font.monospace ]
                                        , centerY
                                        , moveDown 2
                                        ]
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



-- Data structures and decoders


type alias Data =
    { latitude : Float
    , longitude : Float
    , packetCount : Int
    , epsDTO : EpsDTO
    , frameType : Int
    , satelliteTime : Time.Posix
    , createdDate : Time.Posix
    , asibDTO : AsibDTO
    , rfDTO : RfDTO
    , paDTO : PaDTO
    , antsDTO : AntsDTO
    , swDTO : SwDTO
    }


dataDecoder : Decoder Data
dataDecoder =
    JD.field "data"
        (JD.succeed Data
            |> JD.required "latitude" JD.float
            |> JD.required "longitude" JD.float
            |> JD.required "packetCount" JD.int
            |> JD.required "epsDTO" epsDTODecoder
            |> JD.required "frameType" JD.int
            |> JD.required "satelliteTime" timestampDecoder
            |> JD.required "createdDate" timestampDecoder
            |> JD.required "asibDTO" asibDTODecoder
            |> JD.required "rfDTO" rfDTODecoder
            |> JD.required "paDTO" paDTODecoder
            |> JD.required "antsDTO" antsDTODecoder
            |> JD.required "swDTO" swDTODecoder
        )


type alias EpsDTO =
    { panelVolts1 : Int
    , panelVolts2 : Int
    , panelVolts3 : Int
    , totPhotoCurr : Int
    , batteryVolts : Int
    , totSystemCurr : Int
    , rebootCount : Int
    , epsSwErrors : Int
    , boostTemp1 : Int
    , boostTemp2 : Int
    , boostTemp3 : Int
    , batteryTemp : Int
    , latchUpCount5v : Int
    , latchUpCount3v3 : Int
    , resetCause : Int
    , pptMode : Int
    }


epsDTODecoder : Decoder EpsDTO
epsDTODecoder =
    JD.succeed EpsDTO
        |> JD.required "panelVolts1" JD.int
        |> JD.required "panelVolts2" JD.int
        |> JD.required "panelVolts3" JD.int
        |> JD.required "totPhotoCurr" JD.int
        |> JD.required "batteryVolts" JD.int
        |> JD.required "totSystemCurr" JD.int
        |> JD.required "rebootCount" JD.int
        |> JD.required "epsSwErrors" JD.int
        |> JD.required "boostTemp1" JD.int
        |> JD.required "boostTemp2" JD.int
        |> JD.required "boostTemp3" JD.int
        |> JD.required "batteryTemp" JD.int
        |> JD.required "latchUpCount5v" JD.int
        |> JD.required "latchUpCount3v3" JD.int
        |> JD.required "resetCause" JD.int
        |> JD.required "pptMode" JD.int


type alias AsibDTO =
    { sunSensorX : Float
    , sunSensorY : Float
    , sunSensorZ : Float
    , solXPlus : Float
    , solXMinus : Float
    , solYPlus : Float
    , solYMinus : Float
    , busVolts3v3 : Float
    , busCurr3v3 : Float
    , busVolts5 : Float
    }


asibDTODecoder : Decoder AsibDTO
asibDTODecoder =
    JD.succeed AsibDTO
        |> JD.required "sunSensorX" Json.Decode.Extra.parseFloat
        |> JD.required "sunSensorY" Json.Decode.Extra.parseFloat
        |> JD.required "sunSensorZ" Json.Decode.Extra.parseFloat
        |> JD.required "solXPlus" Json.Decode.Extra.parseFloat
        |> JD.required "solXMinus" Json.Decode.Extra.parseFloat
        |> JD.required "solYPlus" Json.Decode.Extra.parseFloat
        |> JD.required "solYMinus" Json.Decode.Extra.parseFloat
        |> JD.required "busVolts3v3" Json.Decode.Extra.parseFloat
        |> JD.required "busCurr3v3" Json.Decode.Extra.parseFloat
        |> JD.required "busVolts5" Json.Decode.Extra.parseFloat


type alias RfDTO =
    { rxDoppler : Int
    , rxRSSI : Int
    , rxTemp : Float
    , rxCurr : Int
    , txBusCurr3v3 : Int
    , txBusCurr5v : Int
    }


rfDTODecoder : Decoder RfDTO
rfDTODecoder =
    JD.succeed RfDTO
        |> JD.required "rxDoppler" JD.int
        |> JD.required "rxRSSI" JD.int
        |> JD.required "rxTemp" Json.Decode.Extra.parseFloat
        |> JD.required "rxCurr" JD.int
        |> JD.required "txBusCurr3v3" JD.int
        |> JD.required "txBusCurr5v" JD.int


type alias PaDTO =
    { txRevPwr : Float
    , txFwdPwr : Float
    , txTemp : Float
    , txCurr : Float
    }


paDTODecoder : Decoder PaDTO
paDTODecoder =
    JD.succeed PaDTO
        |> JD.required "txRevPwr" Json.Decode.Extra.parseFloat
        |> JD.required "txFwdPwr" Json.Decode.Extra.parseFloat
        |> JD.required "txTemp" Json.Decode.Extra.parseFloat
        |> JD.required "txCurr" Json.Decode.Extra.parseFloat


type alias AntsDTO =
    { antTemp0 : Float
    , antTemp1 : Float
    , antDepl0 : String
    , antDepl1 : String
    , antDepl2 : String
    , antDepl3 : String
    }


antsDTODecoder : Decoder AntsDTO
antsDTODecoder =
    JD.succeed AntsDTO
        |> JD.required "antTemp0" Json.Decode.Extra.parseFloat
        |> JD.required "antTemp1" Json.Decode.Extra.parseFloat
        |> JD.required "antDepl0" JD.string
        |> JD.required "antDepl1" JD.string
        |> JD.required "antDepl2" JD.string
        |> JD.required "antDepl3" JD.string


type alias SwDTO =
    { dtmfCmdCount : Int
    , dtmfLastCmd : String
    , dtmfCmdSuccess : Bool
    , dataValidASIB : Bool
    , dataValidEPS : Bool
    , dataValidPA : Bool
    , dataValidRF : Bool
    , dataValidiMTQ : Bool
    , dataValidAntsBusB : Bool
    , dataValidAntsBusA : Bool
    , inEclipseMode : Bool
    , inSafeMode : Bool
    , hardwareABFOnOff : Bool
    , softwareABFOnOff : Bool
    , deploymentWait : Bool
    }


swDTODecoder : Decoder SwDTO
swDTODecoder =
    JD.succeed SwDTO
        |> JD.required "dtmfCmdCount" JD.int
        |> JD.required "dtmfLastCmd" JD.string
        |> JD.required "dtmfCmdSuccess" boolDecoder
        |> JD.required "dataValidASIB" boolDecoder
        |> JD.required "dataValidEPS" boolDecoder
        |> JD.required "dataValidPA" boolDecoder
        |> JD.required "dataValidRF" boolDecoder
        |> JD.required "dataValidiMTQ" boolDecoder
        |> JD.required "dataValidAntsBusB" boolDecoder
        |> JD.required "dataValidAntsBusA" boolDecoder
        |> JD.required "inEclipseMode" boolDecoder
        |> JD.required "inSafeMode" boolDecoder
        |> JD.required "hardwareABFOnOff" boolDecoder
        |> JD.required "softwareABFOnOff" boolDecoder
        |> JD.required "deploymentWait" boolDecoder


boolDecoder : Decoder Bool
boolDecoder =
    JD.string
        |> JD.andThen
            (\b ->
                case String.toUpper b of
                    "YES" ->
                        JD.succeed True

                    "ON" ->
                        JD.succeed True

                    "NO" ->
                        JD.succeed False

                    "OFF" ->
                        JD.succeed False

                    _ ->
                        JD.fail <| "Unexpected boolean: " ++ b
            )


timestampDecoder : Decoder Time.Posix
timestampDecoder =
    JD.string
        |> JD.andThen
            (\timestamp ->
                timestamp
                    |> String.replace "Data received: " ""
                    |> String.replace " " "T"
                    |> Iso8601.toTime
                    |> (\r ->
                            case r of
                                Ok o ->
                                    JD.succeed o

                                Err _ ->
                                    JD.fail <| "Failed to parse timestamp " ++ timestamp
                       )
            )
