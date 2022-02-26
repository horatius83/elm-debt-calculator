module NewLoan exposing (..)

import Loan exposing (Loan)
import State exposing (Model, Msg(..))


defaultLoan : Loan
defaultLoan =
    { name = "New Loan", apr = 0.0, minimum = 0.0, principal = 0.0 }


toUpdateTuple : Model -> ( Model, Cmd Msg )
toUpdateTuple model =
    ( model, Cmd.none )


addLoan : Model -> ( Model, Cmd Msg )
addLoan model =
    { model | loans = model.loans ++ [ model.newLoan ], newLoan = defaultLoan }
        |> toUpdateTuple


resetLoan : Model -> ( Model, Cmd Msg )
resetLoan model =
    { model | newLoan = defaultLoan }
        |> toUpdateTuple


updateLoanName : String -> Model -> ( Model, Cmd Msg )
updateLoanName name model =
    let
        oldNewLoan =
            model.newLoan

        newLoan =
            { oldNewLoan | name = name }
    in
    { model | newLoan = newLoan }
        |> toUpdateTuple


updateLoanPrincipal : String -> Model -> (Msg -> Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
updateLoanPrincipal principalAsString model update =
    let
        updatePrincipal loan principal =
            { loan | principal = principal }

        errorMessage =
            "Could not parse principal: " ++ principalAsString
    in
    updateFloat principalAsString model errorMessage updatePrincipal update


updateLoanMinimum : String -> Model -> (Msg -> Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
updateLoanMinimum minimumAsString model update =
    let
        updateMinimum loan minimum =
            { loan | minimum = minimum }

        errorMessage =
            "Could not parse minimum: " ++ minimumAsString
    in
    updateFloat minimumAsString model errorMessage updateMinimum update


updateLoanApr : String -> Model -> (Msg -> Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
updateLoanApr aprAsString model update =
    let
        updateApr loan apr =
            { loan | apr = apr }

        errorMessage =
            "Could not parse APR: " ++ aprAsString
    in
    updateFloat aprAsString model errorMessage updateApr update


updateFloat : String -> Model -> String -> (Loan -> Float -> Loan) -> (Msg -> Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
updateFloat valueAsString model errorMessage updateValue updateModel =
    let
        currentNewLoan =
            model.newLoan

        uv =
            updateValue currentNewLoan

        result =
            String.toFloat valueAsString
                |> Maybe.map uv
                |> Result.fromMaybe errorMessage
    in
    case result of
        Ok ln ->
            toUpdateTuple { model | newLoan = ln }

        Err msg ->
            updateModel (Error msg) model
