
Back to [Part 2](../part2/README.md)

# Introduction to Elm (Part 3)

>Before getting started with this part, be sure to `cd` into the directory `part3` in your project run `npm install` and then open the `Main.elm` file.

In part 3, we'll start to fill out our application. A good place to start seems like modelling the problem. 

We'll be building an app called "Elmfolio". This is a fictional portfolio app that displays project details includingan image, title, project description and a link to the project (should it have one).

An additional requirement is the ability to break projects out by category and only show projects of one category type at a time.

There is no requirement to update items so we can use a standard JSON API with the `GET` verb. 

An existing API has been created and is ready for us to use: [https://www.mocky.io/v2/59f8cfa92d0000891dad41ed](https://www.mocky.io/v2/59f8cfa92d0000891dad41ed)
, let's check this out now.

Okay, so we now know that the `categories` field contains a list `[...]` of objects `{...}` with an `id` and a `label`. 

```json
categories: [
    {
        "id": 1,
        "label": "Web Development"
    },
    {
        "id": 2,
        "label": "Graphic Design"
    },
    {
        "id": 3,
        "label": "Logo Design"
    }
]
```

We also know that we have a list of `items` with each item being an object like the one shown below:

```json
{
    "id": 1,
    "title": "Web Development Project 1",
    "categoryId": 1,
    "imageUrl": "static/images/webdesign1.jpg",
    "linkUrl": "https://web-project1.com",
    "description": "Lorem ipsum dolor sit amet...",
    "overlayColor": "#ef4581"
}
```

You may or may not have put together a connection between the `id` field in each of the `categories` and the `categoryId`
 of each of the `items`. We'll use this to link these two things and categorize our projects (items).
 

#### Modelling with Type Aliases

A `type alias` is a name that refers to a previously defined Type *or* `Record`. 

In our case, we'll likely want to model each of the types we found above with a `Record`.

In our `Main.elm` file, let's go ahead and add a new `type alias` for both `Category` and `Item` line 20
just below 
```elm
type alias Model = 
    {}
```

New code:

```elm
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

>Notice each field in the `type alias` is defined by a `fieldName : FieldType`

This gives us a named structure to store each of the `categories` and `items` that will be returned from the API. 

Well we're on that subject, note that the API returns a JavaScript `Array` for both `categories` and `items`, this will
translate one-to-one with Elm's `List` data structure, so we'll plan to use that type as a container for these two 
_lists_ of _records_

Next, we'll want to create a `type alias` that stores both `categories` and `items`, we need to do this so that later, 
we can translate the API's response directly to this container type that we're creating. We'll call this structure a `Portfolio`

Let's add this new `type alias` directly above our `Category` definition.

```elm
type alias Portfolio =
    { categories : List Category
    , items : List Item
    }
```

For our `Model`, let's make it a bit more interesting by adding a single field called `portfoio`

```elm
type alias Model =
    { portfolio : Portfolio}
```

Since we changed the definition of `Model`, we also need to update our `initialModel`

Let's update `initialModel` as follows:

```elm
initialModel : Model
initialModel =
    { portfolio = { categories = [], items = [] } }
```

#### Recap
In this part, we learned a little bit about types and type aliases and updated our Elm code based on
the app's requirements. 

Next we'll learn a bit about Elm's "custom types" and make some additional changes to both our `Msg` type and our `update` function 

Go to [Part 4](../part4/README.md)