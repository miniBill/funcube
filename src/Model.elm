module Model exposing (AntsDTO, AsibDTO, Data, EpsDTO, PaDTO, RfDTO, SwDTO, dataDecoder)

import Iso8601
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JD
import Json.Decode.Pipeline as JD
import Time


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
        |> JD.required "sunSensorX" JD.parseFloat
        |> JD.required "sunSensorY" JD.parseFloat
        |> JD.required "sunSensorZ" JD.parseFloat
        |> JD.required "solXPlus" JD.parseFloat
        |> JD.required "solXMinus" JD.parseFloat
        |> JD.required "solYPlus" JD.parseFloat
        |> JD.required "solYMinus" JD.parseFloat
        |> JD.required "busVolts3v3" JD.parseFloat
        |> JD.required "busCurr3v3" JD.parseFloat
        |> JD.required "busVolts5" JD.parseFloat


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
        |> JD.required "rxTemp" JD.parseFloat
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
        |> JD.required "txRevPwr" JD.parseFloat
        |> JD.required "txFwdPwr" JD.parseFloat
        |> JD.required "txTemp" JD.parseFloat
        |> JD.required "txCurr" JD.parseFloat


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
        |> JD.required "antTemp0" JD.parseFloat
        |> JD.required "antTemp1" JD.parseFloat
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
