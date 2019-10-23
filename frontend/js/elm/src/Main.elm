module Main exposing (main)

import Browser exposing (..)
import Html exposing (Html, button, div, h1, h3, img, input, li, p, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Port exposing (Action(..), Event(..), Message, eventDecoder, messageDecoder, onResponse, requestEncoder, toSocket, usersDecoder)


type alias Model =
    { status : Status
    , nickname : String
    , users : List String
    , messages : List Message
    }


type Msg
    = OnNicknameType String
    | JoinClick
    | OnResponse String


type Status
    = NotJoined
    | Joined


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = NotJoined
      , nickname = ""
      , users = []
      , messages = []
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnNicknameType nickname ->
            ( { model | nickname = String.trim nickname }, Cmd.none )

        JoinClick ->
            let
                isValid =
                    model.nickname /= ""
            in
            case isValid of
                True ->
                    ( model, toSocket (requestEncoder model.nickname Join) )

                False ->
                    ( model, Cmd.none )

        OnResponse json ->
            let
                decodeResult =
                    Decode.decodeString eventDecoder json
            in
            case decodeResult of
                Ok event ->
                    case event of
                        ChatState ->
                            let
                                usersDecodeResult =
                                    Decode.decodeString usersDecoder json

                                newUsers =
                                    case usersDecodeResult of
                                        Ok users ->
                                            users

                                        Err error ->
                                            model.users
                            in
                            ( { model | users = newUsers }, Cmd.none )

                        SelfJoined ->
                            ( { model | status = Joined }, Cmd.none )

                        UserJoined ->
                            ( model, Cmd.none )

                        NewMessage ->
                            let
                                messageDecodeResult =
                                    Decode.decodeString messageDecoder json

                                newMessages =
                                    case messageDecodeResult of
                                        Ok m ->
                                            m :: model.messages

                                        Err _ ->
                                            model.messages
                            in
                            ( { model | messages = newMessages }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "This is title"
    , body = [ renderLayout model ]
    }


renderLayout : Model -> Html Msg
renderLayout model =
    case model.status of
        NotJoined ->
            renderWelcomeScreen model.nickname

        Joined ->
            renderChatScreen model


renderChatScreen : Model -> Html Msg
renderChatScreen model =
    div [ class "chat" ]
        [ renderUsersList model.nickname model.users
        , renderChatWindow model.messages
        ]


renderChatWindow : List Message -> Html Msg
renderChatWindow messages =
    div [ class "content" ]
        [ div [ class "messages" ]
            [ ul [] (List.map renderSingleMessage messages)
            ]
        , div [ class "message-input" ]
            [ div [ class "wrap" ]
                [ input [ type_ "text", placeholder "Write your message..." ] []
                , button [ class "submit" ] [ text "Send" ]
                ]
            ]
        ]


renderSingleMessage : Message -> Html Msg
renderSingleMessage message =
    let
        from =
            case message.from of
                Just str ->
                    str

                Nothing ->
                    ""
    in
    li [ class "sent" ]
        [ img [ src ("https://api.adorable.io/avatars/285/" ++ from ++ ".png") ] []
        , p [] [ text message.message ]
        ]


renderUsersList : String -> List String -> Html Msg
renderUsersList nickname users =
    div [ class "sidepanel" ]
        [ div [ class "profile" ]
            [ div [ class "wrap" ]
                [ img
                    [ class "online"
                    , src ("https://api.adorable.io/avatars/285/" ++ nickname ++ ".png")
                    ]
                    []
                , p [] [ text nickname ]
                ]
            ]
        , div [ class "contacts" ]
            [ ul []
                (List.map
                    renderSingleUser
                    users
                )
            ]
        ]


renderSingleUser : String -> Html Msg
renderSingleUser nickname =
    li [ class "contact" ]
        [ div [ class "wrap" ]
            [ img
                [ src ("https://api.adorable.io/avatars/285/" ++ nickname ++ ".png")
                ]
                []
            , div [ class "meta" ]
                [ p [ class "name" ] [ text nickname ] ]
            ]
        ]


renderWelcomeScreen : String -> Html Msg
renderWelcomeScreen nickname =
    div [ class "lobby" ]
        [ h1 [] [ text "Welcome to the SuperCHAT!" ]
        , div []
            [ input
                [ type_ "text"
                , placeholder "Nickname"
                , onInput OnNicknameType
                , value nickname
                ]
                []
            , input
                [ type_ "button"
                , class "form-button"
                , onClick JoinClick
                , value "JOIN"
                ]
                []
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onResponse OnResponse


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
