module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markov exposing (..)
import Random


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { inputText : String
    , generator : Markov.Markov
    , out : String
    }


init : ( Model, Cmd Msg )
init =
    ( { inputText = "hello world"
      , generator = markovify (Random.initialSeed 0) ""
      , out = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MarkovifyIn String
    | MarkovifyOut String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MarkovifyIn str ->
            let
                markovGenerator =
                    Markov.newInput model.generator str
            in
                ( { model | generator = markovGenerator }, Cmd.none )

        MarkovifyOut _ ->
            let
                ( nextWord, generator ) =
                    Markov.next model.generator

                out =
                    model.out ++ " " ++ nextWord
            in
                ( { model | out = out, generator = generator }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        textStyle =
            style [ ( "width", "200px" ), ( "height", "200px" ) ]
    in
        div []
            [ textarea [ textStyle, placeholder "input", onInput MarkovifyIn ] []
            , textarea [ textStyle, placeholder "type here", onInput MarkovifyOut, value model.out ] []
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
