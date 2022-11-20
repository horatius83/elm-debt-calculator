module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Extra exposing (removeAt)
import Loan exposing (Loan, PaymentPlanResult(..), PaymentSequence, avalanche, getMinimumTotalAmount, snowball, toPaymentPlan)
import NewLoan exposing (emptyLoanForm)
import PortConsole exposing (logError)
import PortPdfMake exposing (showAsPdf)
import State exposing (FormState(..), Model, Msg(..), PaymentStrategy(..))
import Task
import Time exposing (Month(..))
import TimeUtil exposing (getNextMonth, getNextYear, monthToString)


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
      , paymentStrategy = Avalanche
      , totalMonthlyPayment = 0
      , paymentPlan = Nothing
      , currentTime = Nothing
      , currentTimeZone = Nothing
      , formState = EnterLoans
      , newLoanForm = emptyLoanForm
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

        ChangeFormState formState ->
            let
                paymentPlan =
                    toPaymentPlan model.yearsToPayoff model.loans

                totalMinimumAmount =
                    getMinimumTotalAmount paymentPlan
                        |> ceiling
                        |> toFloat
            in
            case formState of
                EnterPaymentStrategy ->
                    ( { model | formState = formState, totalMonthlyPayment = totalMinimumAmount }, Cmd.none )

                _ ->
                    ( { model | formState = formState }, Cmd.none )


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
                [ viewPaymentStrategy model.yearsToPayoff model.totalMonthlyPayment model.loans
                ]

        paymentPlan =
            case ( model.currentTime, model.currentTimeZone, model.paymentPlan ) of
                ( Just time, Just timeZone, Just pp ) ->
                    [ viewPaymentPlan (Time.toYear timeZone time) (Time.toMonth timeZone time) pp ]

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


viewNewLoan : Model -> Html Msg
viewNewLoan model =
    let
        canPickPaymentStrategy =
            case model.loans of
                [] ->
                    True

                _ ->
                    False

        loan =
            model.newLoanForm

        isJust x =
            case x of
                Just _ ->
                    True

                Nothing ->
                    False

        cannotAddNewLoan =
            [ loan.principal
            , loan.minimum
            , loan.apr
            ]
                |> List.map String.toFloat
                |> List.all isJust
                |> not
    in
    form [ onSubmit DoNothing ]
        [ fieldset []
            [ viewTextInput "Name" loan.name "new-loan-name" UpdateLoanName
            , viewTextInput "Principal" loan.principal "new-loan-principal" UpdateLoanPrincipal
            , viewTextInput "Minimum" loan.minimum "new-loan-minimum" UpdateLoanMinimum
            , viewTextInput "APR" loan.apr "new-loan-apr" UpdateLoanApr
            , button
                [ onClick AddLoan
                , disabled cannotAddNewLoan
                ]
                [ text "Add Loan" ]
            , button [ onClick ResetNewLoan ] [ text "Reset" ]
            , button
                [ disabled canPickPaymentStrategy
                , onClick (ChangeFormState EnterPaymentStrategy)
                ]
                [ text "Next" ]
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
    in
    form [ onSubmit DoNothing ]
        [ fieldset []
            [ viewIntInput "Maximum number of years to payoff" yearsToPayoff "years-to-payoff" UpdateYearsToPayoff
            , viewFloatInput "Maximum total monthly payment" totalMaximumMonthlyPayment "total-minimum-amount" (Just totalMinimumAmount) UpdateMaximumTotalPayment
            , viewSelect "Payment Strategy" "payment-strategy" paymentStrategyOptions optionToStrategy
            , button
                [ disabled (isCalculatePaymentPlanButtonDisabled loans)
                , onClick GeneratePaymentPlan
                ]
                [ text "Calculate Payment Plan" ]
            ]
        ]


viewMoney : Float -> Html Msg
viewMoney f =
    (f * 100.0)
        |> round
        |> toFloat
        |> (\x -> x / 100.0)
        |> String.fromFloat
        |> (\x -> "$" ++ x)
        |> text


viewPaymentSequence : Int -> Month -> List PaymentSequence -> List (Html Msg)
viewPaymentSequence currentYear currentMonth paymentSequence =
    let
        viewDate year month =
            let
                yearAsText =
                    String.fromInt year

                monthAsText =
                    monthToString month
            in
            monthAsText ++ " " ++ yearAsText |> text

        getThisMonthsPaymentsAcc ps acc =
            case ps.payments of
                [] ->
                    acc

                x :: _ ->
                    List.append acc [ ( ps.loan.name, x ) ]

        thisMonthsPayments =
            List.foldl getThisMonthsPaymentsAcc [] paymentSequence

        nextMonthsPaymentSequenceAcc ps acc =
            case ps.payments of
                [] ->
                    acc

                _ :: xs ->
                    List.append acc [ PaymentSequence ps.loan ps.actualMinimum xs ps.isPaidOff ]

        nextMonthsPaymentSequence =
            List.foldl nextMonthsPaymentSequenceAcc [] paymentSequence

        viewMonthlyPayment loanName amount =
            tr []
                [ td [] [ text loanName ]
                , td [] [ viewMoney amount ]
                ]

        thisSection =
            section []
                [ div [] [ h2 [] [ viewDate currentYear currentMonth ] ]
                , table [] (List.map (\( name, amount ) -> viewMonthlyPayment name amount) thisMonthsPayments)
                ]
    in
    case thisMonthsPayments of
        [] ->
            []

        _ ->
            thisSection :: viewPaymentSequence (getNextYear currentYear currentMonth) (getNextMonth currentMonth) nextMonthsPaymentSequence


viewPaymentPlan : Int -> Month -> List PaymentSequence -> Html Msg
viewPaymentPlan currentYear currentMonth paymentPlan =
    let
        payments =
            viewPaymentSequence currentYear currentMonth paymentPlan
    in
    div [] payments


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
