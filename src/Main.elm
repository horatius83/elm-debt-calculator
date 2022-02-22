module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra exposing (getAt, removeAt)
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
    , errors : List String
    }


init : () -> ( Model, Cmd Msg )
init key =
    ( { loans = [], newLoan = defaultLoan, errors = [] }, Cmd.none )


type Msg
    = AddLoan
    | UpdateLoan Int
    | UpdateLoanName String
    | UpdateLoanApr String
    | UpdateLoanPrincipal String
    | UpdateLoanMinimum String
    | Error String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldNewLoan =
            model.newLoan
    in
    case msg of
        AddLoan ->
            ( { model | loans = model.newLoan :: model.loans, newLoan = defaultLoan }, Cmd.none )

        UpdateLoan index ->
            let
                loan =
                    getAt index model.loans

                loans =
                    removeAt index model.loans

                indexAsString =
                    String.fromInt index
            in
            case loan of
                Just ln ->
                    ( { model | loans = loans, newLoan = ln }, Cmd.none )

                _ ->
                    update (Error <| "Attempted to update loan " ++ indexAsString) model

        UpdateLoanName name ->
            let
                newLoan =
                    { oldNewLoan | name = name }
            in
            ( { model | newLoan = newLoan }, Cmd.none )

        UpdateLoanApr apr ->
            let
                newLoan =
                    Maybe.map (\x -> { oldNewLoan | apr = x }) <| String.toFloat apr

                errorMessage =
                    "Could not parse apr: " ++ apr
            in
            case newLoan of
                Just ln ->
                    ( { model | newLoan = ln }, Cmd.none )

                Nothing ->
                    update (Error errorMessage) model

        UpdateLoanPrincipal principal ->
            let
                newLoan =
                    Maybe.map (\x -> { oldNewLoan | principal = x }) <| String.toFloat principal

                errorMessage =
                    "Could not parse principal: " ++ principal
            in
            case newLoan of
                Just ln ->
                    ( { model | newLoan = ln }, Cmd.none )

                Nothing ->
                    update (Error errorMessage) model

        UpdateLoanMinimum minimum ->
            let
                newLoan =
                    Maybe.map (\x -> { oldNewLoan | minimum = x }) <| String.toFloat minimum

                errorMessage =
                    "Could not parse minimum: " ++ minimum
            in
            case newLoan of
                Just ln ->
                    ( { model | newLoan = ln }, Cmd.none )

                Nothing ->
                    update (Error errorMessage) model

        Error errorMessage ->
            ( { model | errors = errorMessage :: model.errors }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [] [ h1 [] [ text "Hello World" ] ]
