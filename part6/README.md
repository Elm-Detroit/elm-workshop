
Back to [Part 5](../part5/README.md)

# Introduction to Elm (Part 6)

>Before getting started with this part, be sure to `cd` into the directory `part6` in your project run `npm install` and then open the `Main.elm` file

In this section, we'll explain JSON decoders and how they snap together like LEGOs to transform
a string of JSON into concrete Elm types.

If we look at the decoders we have for this app, we find that they match up semantically with our `type alias` for
`Portfolio`, `Category` and `Item`.

Types

```elm
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

```

JSON Decoders

```elm
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
```

For instance, our `portfolioDecoder` takes the full JSON string returned from the API and starts with defining how the
top level fields `categories` and `items` should be decoded, then we _snap_ in our `categoryDecoder` and `itemDecoder`
along with `Decode.list` to define how we'll want those decoded.

We continue to _snap_ in decoders all the way down to the point where we're using some of ELm built in primitive 
decoders to decode a single field `required "id" Decode.int`.

Here we're using `Decode.list`, `Decode.string` and `Decode.int`, some other commonly used built in decoders are:

- `Decode.float - Decodes decimal values like 3.14 but can also decode whole numbers like 42`
- `Decode.bool - Decodes true | false to True | False` 

You can read more about JSON decoders in package documentation: 

[http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode)

>In this app, we're using a library called `elm-json-decode-pipeline` created by the good folks at NoRedInk. This library
>allows us to simplify decoding of more complex data structures.

As a reminder, we can alias a `module` upon `import` by using the `as` keyword and we're currently giving the `Json.Decode.Pipeline`
library a namespace/alias of `Pipeline`
 
```elm
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
```

Additionally, we're directly exposing two functions from the library so that we can use them without the namespace
prefix `exposing (optional, required)`

We're not actually using `optional` but wanted to show it is it is helpful in instances where either a.) you may not
receive the field back from the API or the field _may_ be `null`. An optional field can be defined as follows and takes
an additional argument which is the default value that should be substituted should the field not exist or have a value of `null`. 

```elm
> optional "overlayColor" Decode.string "#f9f9f9"
```

>The default value for the `optional` field must share the same type as the decoder used to decode that field

Examples `...Decode.int 1`, `...Decode.string ""` or `...Decode.list []`. 

Something like `...Decode.string 0` would cause
a type mismatch and a compiler error.

>IMPORTANT: Decoders must be defined with the fields in the same order that they are defined in the `type alias`. If not, 
you may not get a compiler error but may just get unexpected results

#### Encoders

We won't cover it much here but our Elm types can also be encoded should we need to translate them to a valid JSON string
(ex: as part of the request payload for a JSON API).

Let's say our API required a `userName` field to be passed with a `String` value. We could encode such a value as follows:

```elm
import Json.Encode as Encode

apiEncoder data =
    Encode.object
          [ ("userName", Encode.string data.userName) ]
```

More on encoders and using them can be found at the link below:

- Package Documentation [https://package.elm-lang.org/packages/elm/json/latest/Json-Encode](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode)

#### Recap

In this part, we learned more about the JSON decoders that we added in the last section as well as a brief introduction to encoders. 

In the next section, we'll start updating our `view` function to display the data returned by the API in a meaningful way. 

Go to [Part 7](../part7/README.md)