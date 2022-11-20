module NewLoan exposing (..)

import List exposing (minimum)
import Loan exposing (Loan)
import State exposing (Model, Msg(..), NewLoanForm)


emptyLoanForm : NewLoanForm
emptyLoanForm =
    { name = "", apr = "", minimum = "", principal = "" }


addLoan : Model -> ( Model, Cmd Msg )
addLoan model =
    let
        principal =
            String.toFloat model.newLoanForm.principal

        minimum =
            String.toFloat model.newLoanForm.minimum

        apr =
            String.toFloat model.newLoanForm.apr

        toLoan =
            Loan model.newLoanForm.name

        newLoan =
            Maybe.map3 toLoan apr minimum principal
    in
    case newLoan of
        Just loan ->
            ( { model | loans = model.loans ++ [ loan ], newLoanForm = emptyLoanForm }, Cmd.none )

        _ ->
            ( model, Cmd.none )


canAddLoan : Model -> Bool
canAddLoan model =
    let
        principal =
            String.toFloat model.newLoanForm.principal

        minimum =
            String.toFloat model.newLoanForm.minimum

        apr =
            String.toFloat model.newLoanForm.apr
    in
    case ( principal, minimum, apr ) of
        ( Just p, Just m, Just a ) ->
            p > 0 && m > 0 && a >= 0

        _ ->
            False


resetLoan : Model -> ( Model, Cmd Msg )
resetLoan model =
    ( { model | newLoanForm = emptyLoanForm }, Cmd.none )


updateNewLoanForm : (NewLoanForm -> NewLoanForm) -> Model -> ( Model, Cmd Msg )
updateNewLoanForm f model =
    let
        oldLoanForm =
            model.newLoanForm

        newLoanForm =
            f oldLoanForm
    in
    ( { model | newLoanForm = newLoanForm }, Cmd.none )


updateLoanName : String -> Model -> ( Model, Cmd Msg )
updateLoanName name model =
    updateNewLoanForm (\nlf -> { nlf | name = name }) model


updateLoanPrincipal : String -> Model -> ( Model, Cmd Msg )
updateLoanPrincipal principal model =
    updateNewLoanForm (\nlf -> { nlf | principal = principal }) model


updateLoanMinimum : String -> Model -> ( Model, Cmd Msg )
updateLoanMinimum minimum model =
    updateNewLoanForm (\nlf -> { nlf | minimum = minimum }) model


updateLoanApr : String -> Model -> ( Model, Cmd Msg )
updateLoanApr apr model =
    updateNewLoanForm (\nlf -> { nlf | apr = apr }) model
