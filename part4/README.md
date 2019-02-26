
Back to [Part 3](../part3/README.md)

# Introduction to Elm (Part 4)

>Before getting started with this part, be sure to `cd` into the directory `part4` in your project run `npm install` and then open the `Main.elm` file.

In part 4, we'll introduce Elm's "custom type" and add to our `Msg` custom type as well as installing some new packages
and updating our imports section.

From the Elm guide:
>Many languages have trouble expressing data with weird shapes. They give you a small set of built-in types, and you have to represent everything with them. So you often find yourself using `null`, booleans or strings to encode details in a way that is quite error prone.
>
>Elm's custom types let you represent complex data much more naturally.

In this application we'll mostly use custom types to model the messages (`Msg`) that are passed to
our `update` function, but this is just one of many uses for custom types. Some examples of custom types in
action are Elm's `Maybe` type:

```elm
type Maybe a
    = Just a
    | Nothing
```

`Maybe` is generally used to handle situations in which `null` would be used in other languages, this allows
us to handle both cases so that our application never acts in an odd way when running into situations in
which no value exists (like the example in [part 1](https://github.com/Elm-Detroit/elm-workshop/blob/master/part1/README.md#array) in which `Array.get` could have no value for a non-existent index)

Elm's built in type `Bool` (used to store boolean `true | false` values) is also a custom type

```elm
type Bool = True | False
```

Let's move on to using custom types ourselves. Currently our `Msg` type is a custom type with one possible
state `None`

```elm
type Msg
    = None
```

We're at a point in building the app in which we can start modelling other states we'll need to accomplish
our requirements for the app. First and foremost, we need to fetch data from the API, so let's add some of the code for that now.
 
Let's update the `type Msg` to have another message called `ApiResponse`, this message will also need some data associated
with the API response so that we know if the request succeeded or failed. Update the `Msg` definition as follows:

```elm
type Msg
    = ApiResponse (Result Http.Error Portfolio)
    | None
```

To break this down a bit, we'll be using a new (to our app) library called `Http` to send the HTTP request as well as
route the response (regardless of whether or not it succeeds) to our new message `ApiResponse`. Since `Http.send` 
returns a `Result` with this data, we need to handle type. Parenthesis are needed here because `Result` is actually
a custom type itself:

```elm
type Result error value
    = Ok value
    | Err error
```

So, we either get `Ok` and a `value` (in this case, it's be the JSON response from the API) _or_ we get `Err error` in 
which `error` string like `NetworkError` if say, the API is down or we entered an incorrect URL. 

Again, we're forced to handle all states here because software does fail!

<!-- Next, we'll want to add a new library to our project. We'll need to do this in a terminal by running the following
command (be sure that you're in the `part4` directory when you run this)

`elm package install elm-lang/http --yes`

Well we're at it, let's set ourselves up for the next step by also running:

`elm package install NoRedInk/elm-decode-pipeline --yes`

>These are the only two additional libraries that we'll need for the remainder of the project. -->

Elm packages: Open your `part4/elm.json` file and in the `dependencies` section, it
should look something like the following: 

```json
"dependencies": {
        "direct": {
            "elm/browser": "1.0.0",
            "NoRedInk/elm-json-decode-pipeline": "1.0.0",
            "elm/core": "1.0.0",
            "elm/html": "1.0.0",
            "elm/http": "1.0.0"
        },
        "indirect": {
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.2",
            "elm/json": "1.0.0"
        }
    }
```

This isn't something we'll cover in depth because it's most likely not necessary but the `elm.json`
file holds the configuration for your Elm project. As you begin to run your application, you'll also see `elm-stuff` but
these aren't files you'll actively modify (as a matter of fact, modifying them directly would likely cause you issues!)

Next let's add the following installed packages to our imports section (near the top of our `Main.elm` file) and replace the
existing imports: 

Replace

```elm
import Browser
import Html exposing (Html, div, h1, header, img, text)
import Html.Attributes exposing (class, src, width)
```

with

```elm
import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, src, target, type_, width)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
```

#### Recap
In this part, we learned about custom types and some ways they are used to solve problems. We also learned how to install
new Elm packages. 

In the next part, we'll use the `Http` library to send a request to the API and create the code necessary to receive that
message by modifying the `update` function to use a `case` expression to route our updates based on the `Msg` that was
sent.

>Messages can be sent by user input (onClick etc.), as the result of a command (`Cmd`) or as the result of a subscription 
(`Sub`)  

Go to [Part 5](../part5/README.md)