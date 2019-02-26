Back to [Setup](../setup/README.md)

# Introduction to Elm (Part 1)
 
## What is Elm?

##### From the guide...
> __Elm is a functional language that compiles to JavaScript__. It competes with projects like React as a tool for creating websites and web apps. Elm has a very strong emphasis on simplicity, ease-of-use, and quality tooling.

- Functional language for the browser
- Compiles to JavaScript
- HTML, CSS, JavaScript interoperability
- Strongly and statically typed
- Declarative syntax (Declarative focuses on "What" you want to do and not "How" it should be done)

##### Some properties that have made Elm and solid choice for the front-end development  
- A beginner friendly language with a minimal but powerful set of features
- A quick (and kind) compiler that provides type inference
- Advertises “No” runtime exceptions
- Includes an architecture designed for the language
- Fully interoperable with JavaScript
- Package repository (https://package.elm-lang.org/) enforces semantic versioning
- A helpful and friendly community
- A formatter that eliminates the need for a linter

## Resources

#### Community
- Slack: https://elmlang.herokuapp.com/
- Discourse: https://discourse.elm-lang.org/
- Reddit: https://www.reddit.com/r/elm/
- Elm Detroit Meetup: https://www.meetup.com/elm-detroit/

#### Learning
<!---Examples from the official site: http://elm-lang.org/examples -->
- The Official Guide: https://guide.elm-lang.org/
- A "Beginning Elm" tutorial: http://elmprogramming.com/ (Currently not updated for Elm 0.19)
- Elm in Action (Book): https://www.manning.com/books/elm-in-action (Early release, chapters 1 - 3 have been updated to Elm 0.19)
- Building Web Apps with Elm (course): https://pragmaticstudio.com/courses/elm (Currently not updated for Elm 0.19)
- Short course on integrating Elm into an existing system: https://pragmaticstudio.com/courses/integrating-elm (Currently not updated for Elm 0.19)

#### In browser online editors
<!-- - Try Elm (for simple coding): http://elm-lang.org/try -->
- Ellie App: https://ellie-app.com/ (Allows for installing packages, JavaScript interop, sharing your code and more)
 
#### Podcasts
- Elm Town: https://elmtown.github.io/ 
- JS to Elm: https://itunes.apple.com/us/podcast/javascript-to-elm/id1272026213?mt=2

#### Misc
- Elm Cheatsheet: https://github.com/izdi/elm-cheat-sheet (Currently not updated for Elm 0.19)
- Elm News Aggregator: https://elm-news.com/
- Awesome Elm (collection of resources and libraries): https://github.com/isRuslan/awesome-elm

## Language Syntax

Here we'll look at Elm's syntax vs JavaScript together by looking at the link below

[http://elm-lang.org/docs/from-javascript](http://elm-lang.org/docs/from-javascript)

Next we'll look at some of Elm's code data structures:


#### Iterable structures
- [List](http://package.elm-lang.org/packages/elm-lang/core/latest/List)
- [Array](http://package.elm-lang.org/packages/elm-lang/core/latest/Array)
- [Set](http://package.elm-lang.org/packages/elm-lang/core/latest/Set)
- [Dict](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict)

##### List
A `List` is used to store an ordered iterable collection of values where every value in a list must have the same type. Lists are the most commonly used iterable data structure in Elm but they do have some limitations. One of the main limitations is that Lists are not indexed (unlike `Array` in Elm or `Array` in JavaScript) so you cannot get/update an element by index (`myArray[n]` in JavaScript). However, `List` implements many common functions like `filter`, `map` etc.

##### Array
Elm's `Array` structure is similar to a `List` but it also supports positional access via it's helper functions like `Array.get`, `Array.set` and more. An example of using `Array.get` is shown below:

```elm
a = Array.fromList [9,5,7,6]
b = Array.get 3 a
b == Just 6
```

One interesting note here is that `b == Just 6` instead of `6`, the reason for this is that `Array.get` returns a `Maybe` type. `Maybe` is used in places where the value might not exist. Similar to nullable values in other languages. In this case if we tried to call `Array.get 13 a`, we'd get back `Nothing` because index `13` does not exist. 

> We'll talk more about `Maybe` later, but for now, just know that `Maybe` is Elm's way of defining something that is _either_ `Nothing` or `Just something` where `something` is the value.

##### Set
Similar to a `List` but has some unique properties. A `Set` is a collection of unique values. The values can be any `comparable` type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists of comparable types. Sets are good for instances when you need unique values. A note about `Set` is that when accessing items, they are referenced by value and not index. For instance, the documentation for `Set.remove` states: "Remove a value from a `Set`. If the value is not found, no changes are made."

We brought up a new term here `comparable`, this is a special type classification that includes types of `Int`, `Float`, `Time`, `Char`, `String`, `Tuple` and `List`. This is not something that needs a deep dive but we wanted to bring it up.

##### Dict
A _dictionary_ mapping unique _keys_ to _values_. The keys can be any `comparable` type such as `Int`, `Float`, `Time`, `Char`,
`String`, and tuples or lists of `comparable` types. `Dict` is good for times when you need to store data that will need to be updated (or accessed by a unique key). In JavaScript a `Dict` would probably be used for times where you would use JSON similar to the following:
 
```json
{
  "123": {"username": "john_mccauley", "firstname": "John", "lastname": "McCauley"},
  "124": {"username": "james_smith", "firstname": "James", "lastname": "Smith"},
  ...
}
```

Where the key `123` or `124` would be the user's unique ID. 

>Note: Each of these structures implement common transformation functions like `map`, `filter` and folds (amongst others). 
>These functions are useful for many data structures (both iterable and non-iterable) for transforming/shaping data.

##### Tuple

A tuple is an ordered collection of values. Tuples can consist of two or three elements and each element does not have to be the same type. In Elm tuples are created using the literal syntax of two surrounding parenthesis. Tuples can be used for something like an `x` and `y` on a grid or something more complex. 

```elm
# Tuple example
position = (50, 123)
```

>Note: Tuples are limited to 3 values, for more complex data, you'll want to use a `Record` instead (see details below). 

##### Records

A record is a lightweight labeled data structure (similar to an `object` in JavaScript).
For instance, if we wanted to represent a point we could just create a record with an `x` and `y` field:

```elm
{ x = 3, y = 4 }
```

Despite the similarities to `object` in JavaScript, records have a few major differences:

- You cannot ask for a field that does not exist.
- No field will ever be `undefined` or `null`.
- You cannot create recursive records with a `this` or `self` keyword.

#### Recap

In this section, we learned what the Elm programming language is as well as some basic syntax as well
as some of the differences and similarities between Elm and JavaScript.

Next, we'll take a look at what an Elm program looks like in practice and take a look at how
the Elm Architecture is used to structure Elm applications.

Go to [Part 2](../part2/README.md)