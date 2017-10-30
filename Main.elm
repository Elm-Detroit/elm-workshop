module Main exposing (..)

import Html exposing (Html, br, button, div, h1, header, hr, img, nav, text)
import Html.Attributes exposing (class, classList, src, type_, width)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)


-- Model


type alias Model =
    { portfolio : Portfolio
    , selectedCategoryId : Maybe Int
    }


type alias Portfolio =
    { categories : List Category
    , items : List Item
    }


type alias Category =
    { id : Int, label : String }


type alias Item =
    { id : Int
    , title : String
    , categoryId : Int
    , imageUrl : String
    , linkUrl : String
    , description : String
    , overlayColor : String
    }


initialModel : Model
initialModel =
    { portfolio =
        { categories = []
        , items = []
        }
    , selectedCategoryId = Nothing
    }



-- View


view : Model -> Html Msg
view model =
    let
        portfolio =
            model.portfolio

        selectedCategoryId =
            case model.selectedCategoryId of
                Nothing ->
                    1

                Just selected ->
                    selected
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ div
                [ class "col"
                ]
                [ br [] [] ]
            ]
        , div [ class "row" ]
            [ div
                [ class "col"
                ]
                [ h1 [] [ text "Elmfolio" ] ]
            ]
        , viewCategoryNavbar portfolio selectedCategoryId
        , viewItems portfolio selectedCategoryId
        ]


viewCategoryNavbar { categories } selectedCategoryId =
    div [ class "row" ]
        [ div
            [ class "col" ]
            [ div [ class "nav-category" ]
                (List.map (viewCategoryButton selectedCategoryId) categories)
            ]
        ]


viewCategoryButton selectedCategoryId category =
    let
        categorySelected =
            selectedCategoryId == category.id

        classes =
            classList
                [ ( "btn btn-category", True )
                , ( "btn-primary", categorySelected )
                , ( "btn-secondary", not categorySelected )
                ]
    in
    button [ type_ "button", classes, onClick (CategoryClicked category.id) ] [ text category.label ]


viewItems { items } selectedCategoryId =
    let
        filteredItems =
            items |> List.filter (\i -> i.categoryId == selectedCategoryId) |> List.map viewItem
    in
    div [ class "row items-container" ]
        filteredItems


viewItem item =
    div
        [ class "col-4 item-panel" ]
        [ img [ src item.imageUrl ] [] ]



-- Update


type Msg
    = ApiResponse (Result Http.Error Portfolio)
    | CategoryClicked Int
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponse response ->
            case response of
                Ok response ->
                    let
                        updatedModel =
                            { model
                                | portfolio = response
                                , selectedCategoryId = Just 1
                            }
                    in
                    ( updatedModel, Cmd.none )

                Err error ->
                    let
                        x =
                            Debug.log "Err error" error
                    in
                    ( model, Cmd.none )

        CategoryClicked categoryId ->
            let
                updatedModel =
                    { model
                        | selectedCategoryId = Just categoryId
                    }
            in
            ( updatedModel, Cmd.none )

        None ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions =
    \_ -> Sub.none



-- Http


getPortfolio : Cmd Msg
getPortfolio =
    let
        url =
            "http://www.mocky.io/v2/59f748f62f000070135585d0"
    in
    Http.send ApiResponse (Http.get url portfolioDecoder)



-- JSON Decoding


portfolioDecoder : Decoder Portfolio
portfolioDecoder =
    decode Portfolio
        |> required "categories" (Decode.list categoryDecoder)
        |> required "items" (Decode.list itemDecoder)


categoryDecoder : Decoder Category
categoryDecoder =
    decode Category
        |> required "id" Decode.int
        |> required "label" Decode.string


itemDecoder : Decoder Item
itemDecoder =
    decode Item
        |> required "id" Decode.int
        |> required "title" Decode.string
        |> required "categoryId" Decode.int
        |> required "imageUrl" Decode.string
        |> required "linkUrl" Decode.string
        |> required "description" Decode.string
        |> required "overlayColor" Decode.string



-- Helpers


(=>) : a -> b -> ( a, b )
(=>) =
    (,)



-- Program Initialization


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


init =
    ( initialModel, getPortfolio )
