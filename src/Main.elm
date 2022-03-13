module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Extra exposing (removeAt)
import Loan exposing (Loan, PaymentPlan, PaymentPlanResult(..), avalanche, getMinimumTotalAmount, snowball, toPaymentPlan)
import NewLoan exposing (defaultLoan)
import State exposing (Model, Msg(..), PaymentStrategy(..))
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { loans = []
      , newLoan = defaultLoan
      , errors = []
      , yearsToPayoff = 20
      , paymentStrategy = Avalanche
      , totalMonthlyPayment = 0
      , paymentPlan = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddLoan ->
            NewLoan.addLoan model

        DeleteLoan index ->
            let
                loans =
                    removeAt index model.loans
            in
            ( { model | loans = loans, newLoan = defaultLoan }, Cmd.none )

        ResetNewLoan ->
            NewLoan.resetLoan model

        UpdateLoanName name ->
            NewLoan.updateLoanName name model

        UpdateLoanPrincipal principal ->
            NewLoan.updateLoanPrincipal principal model update

        UpdateLoanMinimum minimum ->
            NewLoan.updateLoanMinimum minimum model update

        UpdateLoanApr apr ->
            NewLoan.updateLoanApr apr model update

        Error errorMessage ->
            ( { model | errors = errorMessage :: model.errors }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )

        UpdateYearsToPayoff yearsAsString ->
            let
                maybeYears =
                    String.toInt yearsAsString

                errorMessage =
                    "Could not parse " ++ yearsAsString ++ " as Years to Payoff"
            in
            case maybeYears of
                Just years ->
                    ( { model | yearsToPayoff = years }, Cmd.none )

                _ ->
                    update (Error errorMessage) model

        ChoosePaymentStrategy paymentStrategy ->
            ( { model | paymentStrategy = paymentStrategy }, Cmd.none )

        UpdateMaximumTotalPayment paymentAsString ->
            let
                maybePayment =
                    String.toFloat paymentAsString

                errorMessage =
                    "Could not parse " ++ paymentAsString ++ " as a Total Monthly Payment"
            in
            case maybePayment of
                Just payment ->
                    ( { model | totalMonthlyPayment = payment }, Cmd.none )

                _ ->
                    update (Error errorMessage) model

        GeneratePaymentPlan ->
            generatePaymentPlan model

        GotCurrentTime time ->
            ( model, Cmd.none )


generatePaymentPlan : Model -> ( Model, Cmd Msg )
generatePaymentPlan model =
    let
        newPaymentPlan =
            toPaymentPlan model.yearsToPayoff model.loans

        paymentPlanResult =
            case model.paymentStrategy of
                Avalanche ->
                    avalanche newPaymentPlan model.totalMonthlyPayment

                Snowball ->
                    snowball newPaymentPlan model.totalMonthlyPayment

        getErrorMessage minimumAmount =
            "Amount " ++ String.fromFloat minimumAmount ++ " is too low to calculate payment plan."
    in
    case paymentPlanResult of
        MaximumTotalAmountTooLow amount ->
            update (Error (getErrorMessage amount)) model

        NoFurtherPaymentsToBeMade paymentPlan ->
            ( { model | paymentPlan = Just paymentPlan }, Cmd.none )

        PaymentsRemaining paymentPlan ->
            generatePaymentPlan { model | paymentPlan = Just paymentPlan }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        title =
            [ h2 [] [ text "Loans" ] ]

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

        paymentStrategyTitle =
            h2 [] [ text "Payment Strategy" ]

        paymentStrategy =
            div []
                [ paymentStrategyTitle
                , viewPaymentStrategy model.yearsToPayoff model.totalMonthlyPayment model.loans
                ]

        paymentPlan =
            case model.paymentPlan of
                Just pp ->
                    [ viewPaymentPlan pp ]

                _ ->
                    []

        errorListItem txt =
            li [ class "red" ] [ text txt ]

        errors =
            ul [] <| List.map errorListItem model.errors
    in
    div [] (title ++ loans ++ newLoans ++ (paymentStrategy :: paymentPlan) ++ [ errors ])


viewNewLoan : Loan -> Html Msg
viewNewLoan loan =
    form [ onSubmit DoNothing ]
        [ fieldset []
            [ viewTextInput "Name" loan.name "new-loan-name" UpdateLoanName
            , viewFloatInput "Principal" loan.principal "new-loan-principal" Nothing UpdateLoanPrincipal
            , viewFloatInput "Minimum" loan.minimum "new-loan-minimum" Nothing UpdateLoanMinimum
            , viewFloatInput "APR" loan.apr "new-loan-apr" Nothing UpdateLoanApr
            , button [ onClick AddLoan ] [ text "Add Loan" ]
            , button [ onClick ResetNewLoan ] [ text "Reset" ]
            ]
        ]


viewPaymentStrategy : Int -> Float -> List Loan -> Html Msg
viewPaymentStrategy yearsToPayoff totalMaximumMonthlyPayment loans =
    let
        paymentPlan =
            toPaymentPlan yearsToPayoff loans

        totalMinimumAmount =
            getMinimumTotalAmount paymentPlan
                |> ceiling
                |> toFloat

        paymentStrategyOptions =
            [ "Highest Interest First"
            , "Lowest Principal First"
            ]

        optionToStrategy option =
            case option of
                "Lowest Principal First" ->
                    ChoosePaymentStrategy Snowball

                _ ->
                    ChoosePaymentStrategy Avalanche

        totalMinimumAmountAsString =
            "Total Minimum Amount: " ++ String.fromFloat totalMinimumAmount

        totalMaximumMonthlyPaymentAsString =
            "Total Maximum Payment: " ++ String.fromFloat totalMaximumMonthlyPayment
    in
    form [ onSubmit DoNothing ]
        [ fieldset []
            [ viewIntInput "Maximum number of years to payoff" yearsToPayoff "years-to-payoff" UpdateYearsToPayoff
            , viewFloatInput "Maximum total monthly payment" totalMaximumMonthlyPayment "total-minimum-amount" (Just totalMinimumAmount) UpdateMaximumTotalPayment
            , p [] [ text totalMinimumAmountAsString ]
            , p [] [ text totalMaximumMonthlyPaymentAsString ]
            , viewSelect "Payment Strategy" "payment-strategy" paymentStrategyOptions optionToStrategy
            , button
                [ disabled (isCalculatePaymentPlanButtonDisabled loans)
                , onClick GeneratePaymentPlan
                ]
                [ text "Calculate Payment Plan" ]
            ]
        ]


viewPaymentPlan : PaymentPlan -> Html Msg
viewPaymentPlan paymentPlan =
    let
        makePayment p =
            div []
                [ h3 [] [ text p.loan.name ]
                ]

        payments =
            List.map makePayment paymentPlan

        children =
            h2 [] [ text "Payment Plan" ] :: payments
    in
    div [] children


isCalculatePaymentPlanButtonDisabled : List Loan -> Bool
isCalculatePaymentPlanButtonDisabled loans =
    List.length loans == 0


viewTextInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewTextInput labelText val id callback =
    div []
        [ label [ attribute "for" id ] [ text labelText ]
        , br [] []
        , input
            [ value val
            , onInput callback
            , attribute "name" id
            , attribute "type" "text"
            ]
            []
        ]


viewFloatInput : String -> Float -> String -> Maybe Float -> (String -> Msg) -> Html Msg
viewFloatInput labelText value id minimum callback =
    let
        valueAsString =
            String.fromFloat value

        minimumAsString =
            Maybe.map String.fromFloat minimum

        attributes =
            case minimumAsString of
                Just m ->
                    [ Html.Attributes.min m ]

                _ ->
                    []
    in
    viewNumericInput labelText valueAsString id callback attributes


viewIntInput : String -> Int -> String -> (String -> Msg) -> Html Msg
viewIntInput labelText value id callback =
    let
        valueAsString =
            String.fromInt value
    in
    viewNumericInput labelText valueAsString id callback []


viewNumericInput : String -> String -> String -> (String -> Msg) -> List (Attribute Msg) -> Html Msg
viewNumericInput labelText val id callback otherAttributes =
    let
        attributeList =
            [ value val
            , onInput callback
            , attribute "step" "0.01"
            , attribute "name" id
            , attribute "type" "number"
            ]
                ++ otherAttributes
    in
    div []
        [ label [ attribute "for" id ] [ text labelText ]
        , br [] []
        , input
            attributeList
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


viewSelect : String -> String -> List String -> (String -> Msg) -> Html Msg
viewSelect labelText id options stringToMsg =
    let
        viewOption txt =
            option [ attribute "value" txt ] [ text txt ]

        optionsAsHtml =
            List.map viewOption options
    in
    div []
        [ label [ attribute "for" id ] [ text labelText ]
        , br [] []
        , select
            [ attribute "name" id
            , attribute "id" id
            , onInput stringToMsg
            ]
            optionsAsHtml
        ]


maybesToList : List (Maybe a) -> List a
maybesToList list =
    case list of
        (Just x) :: xs ->
            x :: maybesToList xs

        Nothing :: xs ->
            maybesToList xs

        [] ->
            []
