module NewLoan exposing (..)

import State exposing (Loan, Model, Msg(..))


defaultLoan : Loan
defaultLoan =
    { name = "New Loan", apr = 0.0, minimum = 0.0, principal = 0.0 }


toUpdateTuple : Model -> ( Model, Cmd Msg )
toUpdateTuple model =
    ( model, Cmd.none )


addLoan : Model -> ( Model, Cmd Msg )
addLoan model =
    { model | loans = model.newLoan :: model.loans, newLoan = defaultLoan }
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
        currentNewLoan =
            model.newLoan

        errorMessage =
            "Could not parse principal: " ++ principalAsString

        r =
            String.toFloat principalAsString
                |> Maybe.map (\p -> { currentNewLoan | principal = p })
                |> Result.fromMaybe errorMessage
    in
    case r of
        Ok ln ->
            toUpdateTuple { model | newLoan = ln }

        Err msg ->
            update (Error msg) model
