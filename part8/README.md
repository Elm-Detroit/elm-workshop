
Back to [Part 7](../part7/README.md)

# Introduction to Elm (Part 8)

>Before getting started with this part, be sure to `cd` into the directory `part8` in your project run `npm install` and then open the `Main.elm` file

Currently, we have no real way to _interact_ with the app, in this section, we'll wire up some new 
messages that will be trigged by the `onClick` event for our category buttons as well as our `Item` images.
This is the last step in finishing off our first Elm web app!

Let's start with updating both our union type for `Msg` to account for handling an `Item` click as well as
a `Category` click. To accomplish this, we'll add two new options to our `Msg` union type:

```elm
type Msg
    = ApiResponse (Result Http.Error Portfolio)
    | CategoryClicked Int
    | ItemClicked Int
    | None
```

Here we added `CategoryClicked Int` and `ItemClicked Int`, this will allow us to setup our `view` to trigger these messages
upon specific events (in our case, we'll be using `onClick` for both)

Now that we've added these, we're free to handle our two `TODO` items in `Main.elm`

First, let's update the `buttonOnClick` in the `let` expression for `viewCategoryButton`. Make sure to preserve the necessary indentation!

```elm
        buttonOnClick =
            if categorySelected then
                []
            else
                [ onClick (CategoryClicked category.id) ]
```

Next, let's replace the current `viewItem` function with the following:

```elm
viewItem : Item -> Html Msg
viewItem item =
    div
        [ class "col-4 item-panel" ]
        [ img
            [ src item.imageUrl
            , class "img-fluid"
            , onClick (ItemClicked item.id)
            ]
            []
        ]
```

Here we've added an `onClick` handler as specified in our `TODO`, we've also updated the formatting of
the attributes `List` for the `img` tag so that it's more readable.

The final step is to add two new patterns to our `case` expression in the `update` function:

```elm
        CategoryClicked categoryId ->
            let
                updatedModel =
                    { model
                        | selectedCategoryId = Just categoryId
                        , selectedItemId = Nothing
                    }
            in
            ( updatedModel, Cmd.none )

        ItemClicked itemId ->
            let
                updatedModel =
                    { model
                        | selectedItemId = Just itemId
                    }
            in
            ( updatedModel, Cmd.none )
```

What we're doing in the code above is similar to what we did when we needed to handle our API response,
we're taking in a `Msg` and a `Model`, updating that `Model` accordingly by creating an 
`updatedModel` since values in Elm are immutable and then _scheduling_ any necessary commands (we don't
need to run any commands here so we use `Cmd.none`).

At this point, if we run: `npm start`, we should be able to click around on both categories as well as items and update the rendered HTML:

![App Preview](../static/images/app-item-selected.png)

#### Recap

In this part, we completed our Elm web app! 

In the final part, we'll discuss some ways to make this app better as well as touch on some subjects that
weren't covered. Finally, we'll link and re-link some resources for learning and using Elm.

Go to [Part 9](../part9/README.md)
