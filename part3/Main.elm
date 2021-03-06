module Main exposing (Model, Msg(..), init, initialModel, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, h1, header, img, text)
import Html.Attributes exposing (class, src, width)


{--Model
The `initialModel` function initializes our Model. This function is called in `init` and outputs a Model
--}


initialModel : Model
initialModel =
    {}


type alias Model =
    {}



{--View
The function `view` renders an Html element using our application model.
Note that the type signature is Model -> Html Msg. This means that this function transforms an argument
of Model into an Html element would produce messages tagged with Msg.

We will see this when we introduce some interaction.
--}


view : Model -> Html Msg
view model =
    text "Hello, World!!!"



{--Update--
The `update` function will be called by Html.program each time a message (`Msg`) is received.
This update function responds to messages (`Msg`), updating the model and returning commands as needed.
--}


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



{--Subscriptions
In Elm, using subscriptions is how your application can listen for external input. Some examples are:
 - Keyboard events
 - Mouse movements
 - Browser locations changes
 - Websocket events

In this application, we don't have a need for any active subscriptions so we add in Sub.none
--}


subscriptions =
    \_ -> Sub.none



{--Program setup and initialization--}
{--
The `main` function is the entry point for our app which means it's the first thing that is run
--}


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



{--The `init` function is run by `main` upon application startup and allows us to set
our app's initial state as well as scheduling any commands we'd like to run after the app starts
up. For now, we don't need to run any commands so we'll use Cmd.none here.
--}


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
