module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Loan =
    { name : String
    , apr : Float
    , minimum : Float
    , principal : Float
    }


defaultLoan : Loan
defaultLoan =
    { name = "New Loan", apr = 0.0, minimum = 0.0, principal = 0.0 }


type alias Model =
    { loans : List Loan
    , newLoan : Loan
    }


init : () -> ( Model, Cmd Msg )
init key =
    ( { loans = [], newLoan = defaultLoan }, Cmd.none )


type Msg
    = AddLoan


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddLoan ->
            ( { model | loans = model.newLoan :: model.loans, newLoan = defaultLoan }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [] [ h1 [] [ text "Hello World" ] ]
