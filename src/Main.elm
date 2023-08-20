module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, disabled, href, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Extra exposing (removeAt)
import Loan exposing (PaymentPlanResult(..), avalanche, getMinimumTotalAmount, snowball, toPaymentPlan)
import NewLoan exposing (emptyLoanForm)
import PortConsole exposing (logError)
import PortPdfMake exposing (showAsPdf)
import State exposing (EmergencyFundPlan, FormState(..), Loan, Model, Msg(..), PaymentSequence, PaymentStrategy(..))
import Task
import Time exposing (Month(..))
import TimeUtil exposing (getNextMonth, getNextYear, monthToString)
import View exposing (viewLoan, viewNewLoan, viewPaymentPlan, viewPaymentStrategy)


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
      , errors = []
      , yearsToPayoff = 20
      , totalMonthlyPayment = 0
      , paymentPlan = Nothing
      , currentTime = Nothing
      , currentTimeZone = Nothing
      , formState = EnterLoans
      , newLoanForm = emptyLoanForm
      , strategyForm =
            { maxNumberOfYears = "20"
            , maxTotalPayment = ""
            , paymentStrategy = Avalanche
            , emergencyFund = Nothing
            }
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
            ( { model | loans = loans, newLoanForm = emptyLoanForm }, Cmd.none )

        ResetNewLoan ->
            NewLoan.resetLoan model

        UpdateLoanName name ->
            NewLoan.updateLoanName name model

        UpdateLoanPrincipal principal ->
            NewLoan.updateLoanPrincipal principal model

        UpdateLoanMinimum minimum ->
            NewLoan.updateLoanMinimum minimum model

        UpdateLoanApr apr ->
            NewLoan.updateLoanApr apr model

        Error errorMessage ->
            ( { model | errors = errorMessage :: model.errors }, logError errorMessage )

        DoNothing ->
            ( model, Cmd.none )

        UpdateYearsToPayoff yearsAsString ->
            let
                form =
                    model.strategyForm

                newForm =
                    { form | maxNumberOfYears = yearsAsString }
            in
            ( { model | strategyForm = newForm }, Cmd.none )

        GeneratePaymentPlan ->
            let
                f time timeZone =
                    generatePaymentPlan { model | currentTime = Just time, currentTimeZone = Just timeZone, formState = ViewPaymentPlan }
            in
            ( model, Task.perform (UpdateTimeAndThen f) Time.now )

        GeneratePaymentPlanAsPdf ->
            let
                f time timeZone =
                    let
                        ( paymentPlanModel, _ ) =
                            generatePaymentPlan { model | currentTime = Just time, currentTimeZone = Just timeZone, formState = ViewPaymentPlan }
                    in
                    ( paymentPlanModel, showAsPdf "placeholder" )
            in
            ( model, Task.perform (UpdateTimeAndThen f) Time.now )

        -- https://stackoverflow.com/questions/38021777/how-do-i-get-the-current-time-in-elm-0-17-0-18
        UpdateTimeAndThen f time ->
            let
                fPrime =
                    f time
            in
            ( model, Task.perform (UpdateTimeZoneAndThen fPrime) Time.here )

        UpdateTimeZoneAndThen f timeZone ->
            f timeZone

        ChoosePaymentStrategy paymentStrategy ->
            let
                form =
                    model.strategyForm

                newForm =
                    { form | paymentStrategy = paymentStrategy }
            in
            ( { model | strategyForm = newForm }, Cmd.none )

        UpdateMaximumTotalPayment paymentAsString ->
            let
                form =
                    model.strategyForm

                totalMonthlyPayment =
                    String.toFloat paymentAsString

                newForm =
                    { form | maxTotalPayment = paymentAsString }
            in
            case totalMonthlyPayment of
                Just tmp ->
                    ( { model | strategyForm = newForm, totalMonthlyPayment = tmp }, Cmd.none )

                Nothing ->
                    ( { model | strategyForm = newForm }, Cmd.none )

        ChangeFormState formState ->
            let
                paymentPlan =
                    toPaymentPlan model.yearsToPayoff model.loans

                totalMinimumAmount =
                    getMinimumTotalAmount paymentPlan
                        |> ceiling
                        |> toFloat

                sf =
                    model.strategyForm
            in
            case formState of
                EnterPaymentStrategy ->
                    ( { model
                        | formState = formState
                        , totalMonthlyPayment = totalMinimumAmount
                        , strategyForm = { sf | maxTotalPayment = String.fromFloat totalMinimumAmount }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | formState = formState }, Cmd.none )

        ToggleHasEmergencyFund ->
            let
                sf =
                    model.strategyForm

                newEmergencyFund =
                    case model.strategyForm.emergencyFund of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Just (EmergencyFundPlan 0 "0" 0.5 "50%")

                newStrategyForm =
                    { sf | emergencyFund = newEmergencyFund }
            in
            ( { model | strategyForm = newStrategyForm }, Cmd.none )

        UpdateEmergencyFundMaxAmount amount ->
            let
                sf =
                    model.strategyForm

                ef =
                    sf.emergencyFund

                amountN =
                    String.toFloat amount

                newEmergencyFundPlan ef_ amnt =
                    { ef_ | maxAmountAsString = amount, maxAmount = amnt }

                newEf =
                    Maybe.map2 newEmergencyFundPlan ef amountN

                newModel =
                    case newEf of
                        Just nefp ->
                            { model | strategyForm = { sf | emergencyFund = Just nefp } }

                        Nothing ->
                            { model | strategyForm = { sf | emergencyFund = Maybe.map (\x -> { x | maxAmountAsString = amount }) ef } }
            in
            ( newModel, Cmd.none )

        UpdateEmergencyFundPercentage amount ->
            let
                sf =
                    model.strategyForm

                ef =
                    sf.emergencyFund

                amountN =
                    String.toFloat amount

                newEmergencyFundPlan ef_ amnt =
                    { ef_ | percentageToApplyAsString = amount, percentageToApply = amnt }

                newEf =
                    Maybe.map2 newEmergencyFundPlan ef amountN

                newModel =
                    case newEf of
                        Just nefp ->
                            { model | strategyForm = { sf | emergencyFund = Just nefp } }

                        Nothing ->
                            { model | strategyForm = { sf | emergencyFund = Maybe.map (\x -> { x | percentageToApplyAsString = amount }) ef } }
            in
            ( newModel, Cmd.none )


generatePaymentPlan : Model -> ( Model, Cmd Msg )
generatePaymentPlan model =
    let
        newPaymentPlan =
            case model.paymentPlan of
                Nothing ->
                    toPaymentPlan model.yearsToPayoff model.loans

                Just x ->
                    x

        paymentPlanResult =
            case model.strategyForm.paymentStrategy of
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


viewMenu : FormState -> Html Msg
viewMenu formState =
    let
        loans =
            case formState of
                EnterLoans ->
                    text "Loans"

                _ ->
                    a [ href "#", onClick (ChangeFormState EnterLoans) ] [ text "Loans" ]

        paymentStrategy =
            case formState of
                EnterPaymentStrategy ->
                    text "Strategy"

                _ ->
                    a [ href "#", onClick (ChangeFormState EnterPaymentStrategy) ] [ text "Strategy" ]

        paymentPlan =
            text "Plan"

        divider =
            text " > "
    in
    case formState of
        EnterLoans ->
            h3 [] [ loans ]

        EnterPaymentStrategy ->
            h3 [] [ loans, divider, paymentStrategy ]

        ViewPaymentPlan ->
            h3 [] [ loans, divider, paymentStrategy, divider, paymentPlan ]


view : Model -> Html Msg
view model =
    let
        title =
            [ viewMenu model.formState ]

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
            [ viewNewLoan model ]

        paymentStrategy =
            div []
                [ viewPaymentStrategy model
                ]

        paymentPlan =
            case ( model.currentTime, model.currentTimeZone, model.paymentPlan ) of
                ( Just time, Just timeZone, Just pp ) ->
                    [ viewPaymentPlan (Time.toYear timeZone time) (Time.toMonth timeZone time) pp.payments ]

                _ ->
                    []

        errorListItem txt =
            li [ class "red" ] [ text txt ]

        errors =
            ul [] <| List.map errorListItem model.errors

        document =
            case model.formState of
                EnterLoans ->
                    div [] (title ++ loans ++ newLoans ++ [ errors ])

                EnterPaymentStrategy ->
                    div [] (title ++ [ paymentStrategy, errors ])

                ViewPaymentPlan ->
                    div [] (title ++ paymentPlan ++ [ errors ])
    in
    document
