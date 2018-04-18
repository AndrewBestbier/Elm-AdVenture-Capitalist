module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Progress as Progress
import Bootstrap.Text as Text
import Dict
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)
import Html exposing (Html, button, div, h1, h3, h4, text)
import Html.Attributes exposing (class, disabled, src, style)
import Round
import Time exposing (Time, millisecond)


---- MODEL ----


type alias Config =
    { quantity : Float, cost : Float, productivity : Float, coefficient : Float }


baseConfig : Dict.Dict String Config
baseConfig =
    Dict.fromList
        [ ( "Lemonade Stand", { quantity = 1, cost = 3.7, productivity = 1.67, coefficient = 1.07 } )
        , ( "Newspaper Delivery", { quantity = 0, cost = 60, productivity = 20, coefficient = 1.15 } )
        , ( "Car Wash", { quantity = 0, cost = 720, productivity = 90, coefficient = 1.14 } )
        , ( "Pizza Delivery", { quantity = 0, cost = 8640, productivity = 360, coefficient = 1.13 } )
        , ( "Donut Shop", { quantity = 0, cost = 103680, productivity = 2160, coefficient = 1.12 } )
        , ( "Shrimp Boat", { quantity = 0, cost = 1244160, productivity = 6480, coefficient = 1.11 } )
        , ( "Hockey Team", { quantity = 0, cost = 14929920, productivity = 19440, coefficient = 1.1 } )
        , ( "Movie Studio", { quantity = 0, cost = 179159040, productivity = 58320, coefficient = 1.09 } )
        , ( "Bank", { quantity = 0, cost = 2149908480, productivity = 174960, coefficient = 1.08 } )
        ]


type alias Model =
    { money : Float, config : Dict.Dict String Config }


init : ( Model, Cmd Msg )
init =
    ( { money = 1, config = baseConfig }, Cmd.none )



---- UPDATE ----


type Msg
    = Purchase String
    | Tick Time


getProfit model =
    Dict.foldl (\key x acc -> acc + x.quantity * x.productivity * 0.1) 0 model.config


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Purchase business ->
            ( { model
                | money = model.money - getCost model business
                , config =
                    Dict.update business
                        (\x ->
                            case x of
                                Just x ->
                                    Just
                                        { x
                                            | quantity = x.quantity + 1
                                            , cost = x.cost * x.coefficient
                                            , productivity =
                                                if round (x.quantity + 1) % 25 == 0 then
                                                    x.productivity * 2.0
                                                else
                                                    x.productivity
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.config
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model | money = model.money + getProfit model }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (100 * millisecond) Tick



---- VIEW ----


getCost model business =
    case Dict.get business model.config of
        Just x ->
            x.cost

        Nothing ->
            0


getProductivity model business =
    case Dict.get business model.config of
        Just x ->
            x.productivity

        Nothing ->
            0


getQuantity model business =
    case Dict.get business model.config of
        Just x ->
            x.quantity

        Nothing ->
            0


getProgress model business =
    case Dict.get business model.config of
        Just x ->
            (toFloat (round x.quantity % 25) / 25) * 100

        Nothing ->
            0


formatMoney value =
    "£" ++ format usLocale value


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col []
                [ h1 [ style [ ( "font-size", "5rem" ), ( "color", "white" ) ] ] [ text (formatMoney model.money) ]
                , Card.columns
                    [ businessDetails model "Lemonade Stand"
                    , businessDetails model "Newspaper Delivery"
                    , businessDetails model "Car Wash"
                    , businessDetails model "Pizza Delivery"
                    , businessDetails model "Donut Shop"
                    , businessDetails model "Shrimp Boat"
                    , businessDetails model "Hockey Team"
                    , businessDetails model "Movie Studio"
                    , businessDetails model "Bank"
                    ]
                ]
            ]
        ]


businessDetails model business =
    Card.config [ Card.attrs [ style [ ( "width", "20rem" ) ] ] ]
        |> Card.header [ class "text-center" ]
            [ h3 [] [ text business ]
            ]
        |> Card.block []
            [ Block.text [] [ text ("Cost: " ++ formatMoney (getCost model business)) ]
            , Block.text [] [ text ("Productivity: " ++ formatMoney (getProductivity model business) ++ "/s " ++ "each") ]
            , Block.text [] [ text ("Quantity: " ++ toString (getQuantity model business)) ]
            , Block.custom <|
                div [] [ text "Productivity upgrade progress:" ]
            , Block.custom <|
                div [ style [ ( "margin-top", "0.5rem" ), ( "margin-bottom", "2rem" ) ] ] [ Progress.progress [ Progress.value (getProgress model business) ] ]
            , Block.custom <|
                Button.button [ Button.success, Button.onClick (Purchase business), Button.disabled (model.money < getCost model business) ] [ text "Buy" ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
