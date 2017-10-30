module Main exposing (..)

import Html exposing (Html, div, h1, header, img, text)
import Html.Attributes exposing (class, src, width)
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
    div [ class "container" ]
        [ div [ class "row" ]
            [ div
                [ class "col"
                ]
                [ text "Hello Workshop!" ]
            ]
        ]



-- Update


type Msg
    = ApiResponse (Result Http.Error Portfolio)
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApiResponse response ->
            case response of
                Ok response ->
                    let
                        updatedModel =
                            { model | portfolio = response }

                        x =
                            Debug.log "Ok portfolio" updatedModel
                    in
                    ( updatedModel, Cmd.none )

                Err error ->
                    let
                        x =
                            Debug.log "Err error" error
                    in
                    ( model, Cmd.none )

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
            "http://www.mocky.io/v2/59f508453100005c065b870f"
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
