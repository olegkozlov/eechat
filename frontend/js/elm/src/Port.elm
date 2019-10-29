port module Port exposing (Action(..), Event(..), Message, MessageType(..), eventDecoder, joinEncoder, messageDecoder, messageEncoder, onResponse, toSocket, usernameDecoder, usersDecoder)

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
    = SelfJoined
    | NewUserJoined
    | UserDisconnected
    | UsersListState
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


usernameDecoder : Decode.Decoder String
usernameDecoder =
    Decode.at [ "payload", "username" ] Decode.string


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
        "UsersListState" ->
            Decode.succeed UsersListState

        "Joined" ->
            Decode.succeed SelfJoined

        "NewUserJoined" ->
            Decode.succeed NewUserJoined

        "UserDisconnected" ->
            Decode.succeed UserDisconnected

        "NewMessage" ->
            Decode.succeed NewMessage

        _ ->
            Decode.fail ("Invalid event: " ++ str)



-- ENCODERS


messageEncoder : String -> Action -> Encode.Value
messageEncoder message action =
    Encode.object
        [ ( "message", Encode.string message )
        , ( "action", actionToValue action )
        ]


joinEncoder : String -> Action -> Encode.Value
joinEncoder nickname action =
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
            Encode.string "message"



-- PORTS


port toSocket : Encode.Value -> Cmd msg


port onResponse : (String -> msg) -> Sub msg
