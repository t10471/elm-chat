module Main exposing (..)

import Date exposing (Date)
import Html exposing (program, Html, Attribute, div, img, text, textarea, button)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Html.Attributes exposing (src, value)
import Task exposing (perform)
import Json.Decode as Json
import Styles as Styles
import Date.Extra.Config.Config_ja_jp exposing (config)
import Date.Extra.Format as Format exposing (format)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Chat =
    { id : Int
    , name : String
    , image : String
    , message : String
    , createdAt : String
    , isEdit : Bool
    , eMessage : String
    }


type alias Self =
    { name : String
    , image : String
    }


type alias Model =
    { chats : List Chat
    , self : Self
    , input : String
    , lastId : Int
    }


type Msg
    = Create
    | CreateChat Date
    | Delete Int
    | Edit Int
    | Update Int
    | Cancel Int
    | UpdateCreateInput String
    | UpdateEditInput Int String


tomImg : String
tomImg =
    "http://www.hochi.co.jp/photo/20170718/20170718-OHT1I50084-T.jpg"


dateImg : String
dateImg =
    "https://imgcp.aacdn.jp/img-c/680/auto/tipsplus/series/246/20160608_1465380998273.jpg"


init : ( Model, Cmd Msg )
init =
    ( { chats =
            [ Chat 0 "伊達" dateImg "最初" "2018/01/27 15:30" False "最初"
            , Chat 1 "富沢" tomImg "2ばんめ" "2018/01/27 15:32" False "2ばんめ"
            ]
      , self = { name = "富沢", image = tomImg }
      , input = ""
      , lastId = 1
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create ->
            ( model, Task.perform CreateChat Date.now )

        CreateChat d ->
            ( addChat model d, Cmd.none )

        Delete id ->
            ( deleteChat model id, Cmd.none )

        Edit id ->
            ( editChat model id, Cmd.none )

        Update id ->
            ( updateChat model id, Cmd.none )

        Cancel id ->
            ( cancelChat model id, Cmd.none )

        UpdateCreateInput s ->
            ( { model | input = s }, Cmd.none )

        UpdateEditInput id s ->
            ( updateEditInput model id s, Cmd.none )


addChat : Model -> Date -> Model
addChat model d =
    let
        self =
            model.self

        newId =
            model.lastId + 1

        fd =
            format config config.format.dateTime d

        new =
            Chat newId self.name self.image model.input fd False model.input
    in
        { model | chats = model.chats ++ [ new ], input = "", lastId = model.lastId + 1 }


deleteChat : Model -> Int -> Model
deleteChat model id =
    { model | chats = List.filter (\m -> m.id /= id) model.chats }


editChat : Model -> Int -> Model
editChat model id =
    let
        mapf m =
            if m.id == id then
                { m | isEdit = True }
            else
                m
    in
        { model | chats = List.map mapf model.chats }


updateChat : Model -> Int -> Model
updateChat model id =
    let
        mapf m =
            if m.id == id then
                { m | message = m.eMessage, isEdit = False }
            else
                m
    in
        { model | chats = List.map mapf model.chats }


cancelChat : Model -> Int -> Model
cancelChat model id =
    let
        mapf m =
            if m.id == id then
                { m | eMessage = m.message, isEdit = False }
            else
                m
    in
        { model | chats = List.map mapf model.chats }


updateEditInput : Model -> Int -> String -> Model
updateEditInput model id s =
    let
        mapf m =
            if m.id == id then
                { m | eMessage = s }
            else
                m
    in
        { model | chats = List.map mapf model.chats }


view : Model -> Html Msg
view model =
    div
        [ Styles.mainWrap ]
        [ div [ Styles.postForm ] [ renderRegister model ]
        , div [ Styles.talk ] (List.map (renderList model.self) model.chats)
        ]


renderRegister : Model -> Html Msg
renderRegister model =
    div [ Styles.formLeft ]
        [ img [ Styles.selfImg, src model.self.image ] []
        , div [ Styles.formRight ]
            [ textarea [ onInput UpdateCreateInput, value model.input, Styles.formArea ] []
            , button [ onClick Create, Styles.postButton ] [ text "投稿！" ]
            ]
        ]


renderList : Self -> Chat -> Html Msg
renderList self m =
    div [ Styles.talk ]
        [ div [ Styles.talkLeft ]
            [ img [ Styles.posterImg, src m.image ] [] ]
        , renderTalkRight self m
        ]


renderTalkRight : Self -> Chat -> Html Msg
renderTalkRight self m =
    div [ Styles.talkRight ]
        [ div [ Styles.posterName ] [ text m.name ]
        , renderMessage self m
        , div [ Styles.talkFooter ]
            [ text m.createdAt
            , (renderButton self m)
            ]
        ]


renderMessage : Self -> Chat -> Html Msg
renderMessage self m =
    if m.isEdit then
        textarea
            [ Styles.editingMessage
            , onEnter (Update m.id)
            , onInput (UpdateEditInput m.id)
            , value m.eMessage
            ]
            []
    else
        div [ Styles.message ] [ text m.message ]


renderButton : Self -> Chat -> Html Msg
renderButton self m =
    if m.isEdit then
        div [ Styles.buttons ]
            [ button [ onClick (Update m.id), Styles.editButton ] [ text "更新" ]
            , button [ onClick (Cancel m.id), Styles.deleteButton ] [ text "キャンセル" ]
            ]
    else if m.name == self.name then
        div [ Styles.buttons ]
            [ button [ onClick (Edit m.id), Styles.editButton ] [ text "編集" ]
            , button [ onClick (Delete m.id), Styles.deleteButton ] [ text "削除" ]
            ]
    else
        div [] []


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)
