port module Port exposing (Action(..), Event(..), Message, eventDecoder, messageDecoder, onResponse, requestEncoder, toSocket, usersDecoder)

import Json.Decode as Decode
import Json.Encode as Encode


type alias Message =
    { msgType : MessageType
    , from : Maybe String
    , message : String
    }


type MessageType
    = SystemMessage
    | UserMessage


{-| Outgoing Action's: From Elm to the Interloop
-}
type Action
    = Join
    | SendMessage


{-| Incoming event's: From Interloop to the Elm
-}
type Event
    = ChatState
    | SelfJoined
    | UserJoined
    | NewMessage



-- DECODERS


messageDecoder : Decode.Decoder Message
messageDecoder =
    Decode.at [ "payload", "message" ] <|
        Decode.map3 Message
            (Decode.field "msgType" messageTypeDecoder)
            (Decode.maybe (Decode.field "from" Decode.string))
            (Decode.field "message" Decode.string)


messageTypeDecoder : Decode.Decoder MessageType
messageTypeDecoder =
    Decode.string
        |> Decode.andThen messageTypeFromString


messageTypeFromString : String -> Decode.Decoder MessageType
messageTypeFromString str =
    case str of
        "SystemMessage" ->
            Decode.succeed SystemMessage

        "UserMessage" ->
            Decode.succeed UserMessage

        _ ->
            Decode.fail ("Invalid message type: " ++ str)


usersDecoder : Decode.Decoder (List String)
usersDecoder =
    Decode.at [ "payload", "users" ] (Decode.list Decode.string)


eventDecoder : Decode.Decoder Event
eventDecoder =
    Decode.field "event" Decode.string
        |> Decode.andThen eventFromString


eventFromString : String -> Decode.Decoder Event
eventFromString str =
    case str of
        "ChatState" ->
            Decode.succeed ChatState

        "SelfJoined" ->
            Decode.succeed SelfJoined

        "UserJoined" ->
            Decode.succeed UserJoined

        "NewMessage" ->
            Decode.succeed NewMessage

        _ ->
            Decode.fail ("Invalid event: " ++ str)



-- ENCODERS


requestEncoder : String -> Action -> Encode.Value
requestEncoder nickname action =
    Encode.object
        [ ( "nickname", Encode.string nickname )
        , ( "action", actionToValue action )
        ]


actionToValue : Action -> Encode.Value
actionToValue act =
    case act of
        Join ->
            Encode.string "join"

        SendMessage ->
            Encode.string "sendMessage"



-- PORTS


port toSocket : Encode.Value -> Cmd msg


port onResponse : (String -> msg) -> Sub msg
