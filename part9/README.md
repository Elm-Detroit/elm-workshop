
Back to [Part 8](../part8/README.md)

# Introduction to Elm (Part 9)

>Before getting started with this part, be sure to `cd` into the directory `part9` in your project run `npm install` and then open the `Main.elm` file

### Ways to improve our App

We've built a working Elm app, excellent! Due to the time limitation on this workshop, we had to skip a
couple of nice to have features that we could of added, should we have had the time. 

##### The RemoteData pattern

The first one is to use the `RemoteData` library and pattern to show the user friendly states for loading,
 success, error when we reach out to the API. 
 
The `RemoteData` package documentation is located here (this may not be updated for Elm 0.19): 
[http://package.elm-lang.org/packages/krisajenkins/remotedata/latest](http://package.elm-lang.org/packages/krisajenkins/remotedata/latest)

Kris Jenkins, the creator of this library also wrote a blog post on why he made the library
 and how's it's used: [How Elm Slays a UI Antipattern](http://blog.jenkster.com/2016/06/how-elm-slays-a-ui-antipattern.html)

Lastly, a basic example of using this pattern was created in Ellie: [https://ellie-app.com/3r2wFkFJBr6a1/0](https://ellie-app.com/3r2wFkFJBr6a1/0)

##### Adding animations to transition between states

Should we have had the time, we'd have likely added some animations either by using CSS animations directly
or using a library like [elm-style-animation](https://github.com/mdgriffith/elm-style-animation) with CSS animations

##### Using a view library for the Bootstrap 4 elements

Elm has a wonderful view library for Bootstrap called [elm-bootstrap](http://package.elm-lang.org/packages/rundis/elm-bootstrap/latest).
This makes the composability of Bootstrap elements specifically much nicer. 

In addition to the package documentation on the Elm package site, `elm-bootstrap` has a great documentation
site at [http://elm-bootstrap.info/](http://elm-bootstrap.info/)

#### Topics not discussed in this workshop

For the sake of time and sticking to our goal of "Building a working Elm app", we missed a few things of note. 

__JavaScript Interop__

Interop with JavaScript is done through a mechanism called Ports. Ports allow you to interact with JavaScript in a similar way
to how we interacted with our API. You send a message and you use `subscriptions` to receive messages from Ports. More
on Ports can be found here: 

__Compiler__

Throughout this workshop, we've been using `elm-live` to compile and run our projects in one step. This is convenient, especially
when you want to get up and running fast. However, we would have liked to go over the compiler a bit more as well
as the tool for compiling Elm apps directly `elm-make`, when you run `elm-live` this happens automatically in the 
background but it can be useful to run `elm-make` directly (as part of a build system where you don't need to see 
the output, just compile the app etc.)

Here's a post on some aspects of the Elm compiler that we missed: [Compilers as Assistants](http://elm-lang.org/blog/compilers-as-assistants)

>Note: This article is a bit older but really captures the essence of what the intention is with the Elm compiler. It's
meant to be a friendly assistant guiding you along rather that a task-master barking orders!

__Types__

We actually covered a bit about types in this workshop but we could have gone further. However, we opted not to 
specifically so that we could get you to the goal of building an Elm app without a lot of theory or hype about types. 

It will likely be beneficial for you to take another look and maybe a deeper look, here are a couple resource on Elm's
type system:

- From the guide: [Reading Types](https://guide.elm-lang.org/types/reading_types.html)
- Article: [Understanding the Elm type system](http://www.adamwaselnuk.com/elm/2016/05/27/understanding-the-elm-type-system.html)

#### Questions and Feedback

If we missed anything else, you have additional questions or you have any general feedback on the workshop, 
please open an issue on the REPO here:

[https://github.com/Elm-Detroit/elm-workshop/issues](https://github.com/Elm-Detroit/elm-workshop/issues)

# Thanks for Joining Us!