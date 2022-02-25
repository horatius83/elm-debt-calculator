module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Extra exposing (removeAt)


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
init _ =
    ( { loans = [], newLoan = defaultLoan, errors = [] }, Cmd.none )


type Msg
    = AddLoan
    | ResetNewLoan
    | DeleteLoan Int
    | UpdateLoanName String
    | UpdateLoanApr String
    | UpdateLoanPrincipal String
    | UpdateLoanMinimum String
    | Error String
    | DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldNewLoan =
            model.newLoan
    in
    case msg of
        AddLoan ->
            ( { model | loans = model.newLoan :: model.loans, newLoan = defaultLoan }, Cmd.none )

        DeleteLoan index ->
            let
                loans =
                    removeAt index model.loans
            in
            ( { model | loans = loans, newLoan = defaultLoan }, Cmd.none )

        ResetNewLoan ->
            ( { model | newLoan = defaultLoan }, Cmd.none )

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

        DoNothing ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        title =
            [ h1 [] [ text "Loans" ] ]

        headRow =
            thead []
                [ tr []
                    [ th [] []
                    , th [] [ text "Principal" ]
                    , th [] [ text "Minimum" ]
                    , th [] [ text "APR" ]
                    , th [] []
                    ]
                ]

        tableRows =
            List.indexedMap viewLoan model.loans

        loans =
            if List.isEmpty model.loans then
                []

            else
                [ table [] <| headRow :: tableRows
                ]

        newLoans =
            [ viewNewLoan model.newLoan ]
    in
    div [] (title ++ loans ++ newLoans)


viewNewLoan : Loan -> Html Msg
viewNewLoan loan =
    form [ onSubmit DoNothing ]
        [ fieldset []
            [ viewTextInput "Name" loan.name "new-loan-name" UpdateLoanName
            , viewNumericInput "Principal" loan.principal "new-loan-principal" UpdateLoanPrincipal
            , viewNumericInput "Minimum" loan.minimum "new-loan-minimum" UpdateLoanMinimum
            , viewNumericInput "APR" loan.apr "new-loan-apr" UpdateLoanApr
            , button [ onClick AddLoan ] [ text "Add Loan" ]
            , button [ onClick ResetNewLoan ] [ text "Reset" ]
            ]
        ]


viewTextInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewTextInput labelText value id callback =
    viewInput "text" labelText value id callback


viewNumericInput : String -> Float -> String -> (String -> Msg) -> Html Msg
viewNumericInput labelText value id callback =
    let
        valueAsString =
            String.fromFloat value
    in
    viewInput "numeric" labelText valueAsString id callback


viewInput : String -> String -> String -> String -> (String -> Msg) -> Html Msg
viewInput inputType labelText val id callback =
    div []
        [ label [ attribute "for" id ] [ text labelText ]
        , input
            [ attribute "type" inputType
            , value val
            , placeholder val
            , onInput callback
            ]
            []
        ]


viewLoan : Int -> Loan -> Html Msg
viewLoan index loan =
    let
        toCash value =
            String.fromFloat value
                |> (\v -> "$" ++ v)

        toPercent value =
            String.fromFloat value
                |> (\v -> v ++ "%")
    in
    tr []
        [ td [] [ text loan.name ]
        , td [ class "align-right" ] [ text <| toCash loan.principal ]
        , td [ class "align-right" ] [ text <| toCash loan.minimum ]
        , td [ class "align-right" ] [ text <| toPercent loan.apr ]
        , td [] [ button [ onClick (DeleteLoan index) ] [ text "Delete" ] ]
        ]
