
Back to [Part 1](../part1/README.md)

# Introduction to Elm (Part 2)

In this part, we'll start off with the basic frame of the app and look at each section.

#### Module and Imports

The file starts off with the `module` definition:

`module Main exposing (..)`

Here we're creating a module named `Main` that exposes all of it's functions.

Next, we'll look at imports, here we're importing a couple of common libraries from the Elm language, 
documentation for these libraries can be found here: [http://package.elm-lang.org/packages/elm-lang/html/latest]().

```
import Html exposing (Html, div, h1, header, img, text)
import Html.Attributes exposing (class, src, width)
```

In this code, we're only pulling in (`exposing`) a few functions for our use locally, we can however still call unexposed
functions by using their full namespace (ex: `Html.span`).

#### Model

The `initialModel` function initializes our Model. This function is called in `init` and outputs a `Model`.

```
initialModel : Model
initialModel =
{}
```

The type for our `Model` is pretty bare currently but that will change as we build our app. The important thing to know
is that this is the structure that will hold __all__ of our applications state.

```
type alias Model =
{}
```


#### View

The function `view` renders an `Html` element using our application `Model`.
Note that the type signature is `Model -> Html Msg`. This means that this function transforms an argument
of `Model` into an `Html` element would produce messages tagged with `Msg`.

We will see this when we introduce some interaction.

```
view : Model -> Html Msg
view model =
    text "Hello, World!!!"
```


#### Update
The `update` function will be called by `Html.program` each time a message is received.
This update function responds to messages (`Msg`), updating the model and returning commands (`Cmd`) as needed.

```
type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
```

##### More about commands:
A `Cmd` lets you do stuff: generate a random number, send an HTTP request, etc. Commands don't happen immediately, 
they are scheduled. 

When you schedule a `Cmd`, it runs within the next loop of the Elm runtime. In the code below
we don't need to run any commands to print "Hello World", but later we'll use them to schedule an HTTP request to get
some data from an API.


#### Subscriptions
In Elm, using subscriptions is how your application can listen for external input. Some examples are:
- Keyboard events
- Mouse movements
- Browser locations changes
- Websocket events

In this application, we don't have a need for any active subscriptions so we add in `Sub.none`

```
subscriptions =
\_ -> Sub.none
```


#### Program setup and initialization

The `main` function is the entry point for our app which means it's the first thing that is run

```
main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }
```

The `init` function is run by `main` upon application startup and allows us to set
our app's initial state as well as scheduling any commands we'd like to run after the app starts
up. For now, we don't need to run any commands so we'll use `Cmd.none` here.

```
init =
    ( initialModel, Cmd.none )
```

#### An illustration of how it works together

![The Elm runtime](https://guide.elm-lang.org/architecture/effects/program.svg)

#### Starting the app

Let's start out by moving into the directory for this part by running `cd part2` in a terminal

One of the easiest ways to run an Elm app on your computer _without_ having an HTML file to embed it into
is to run the Elm Reactor. We can do this now by simply running the following command in our terminal `elm reactor`.

Once you run the command in your terminal of choice, you should see output like the following:

```
>elm reactor
elm-reactor 0.18.0
Listening on http://localhost:8000
```

Now you can navigate your web browser to `http://localhost:8000` and you should see a UI that shows a list of the files for 
this project under __"File Navigation"__. Let's go ahead and click on the `Main.elm` file.

Once the app compiles (this takes a few seconds in reactor), you should see "Hello, World!!!" printed in the upper-left
corner of your browser window. The reactor is good for testing a basic idea/verifying logic when you don't need to
style the output with CSS.

There are also other tools that can accomplish similar things:

- The Elm REPL can be used to test small pieces of code and can be started by typing `elm repl` into a terminal.
- The Elm Live is a flexible dev server with features like live reload: [https://github.com/tomekwi/elm-live]()


>Elm Live is helpful when you need to run your code in an HTML file where you can include styles (CSS) and other resources.
>This is what we'll be using `elm-live` going forward with this workshop

Let's stop the `elm reactor` by clicking `Ctrl+C` in the terminal in which you started it

Now let's rerun the app using `elm live` by running the following commands in a terminal:

`elm-live Main.elm --output=static/js/elm.js --pushstate --open`

Once we run this command, we should see a browser window open with the app running and "Hello, World!!!" displayed in
the upper left corner just like when we used the reactor. 

Now, we'll show off one of the nicest features of `elm-live`, in the `Main.elm` file, change the string "Hello World!!!"
to "Hello `your name`!!!" and save the file!

One you do this, `elm-live` should automatically update the app in your browser window to say "Hello `your name`!!!"

#### Recap

In this section, we learned about the basic structure that most Elm applications will follow. We also learned about multiple
ways to run an Elm application.

Next, we'll start to extend the features of our Elm application by adding to the `Model` as well as the `update` and `view`
functions.

Go to [Part 3](../part3/README.md)