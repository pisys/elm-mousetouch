# elm-mousetouch
Unify touch/mouse events on HTML elements into a user defined signal.

## Problem

An `onClick` handler on an HTML element leads to different results on mouse pointing and touch devices. The latter induces a delay of approx. 300 ms prior to execution of the handler. This is due to default gestures supported in mobile Web engines. For instance double tapping usually zooms into the page. So after a tap the engine waits for another one in order to distinguish zooming from actual clicking. However, the delay of 300 ms affects the responsiveness of a Web app and makes it feel slow. [Read more.](http://www.html5rocks.com/en/mobile/touchandmouse/)

The [Touch Events](http://www.w3.org/TR/touch-events/) can work around this for instance by setting a handler on `touchend`, which will be executed the moment the user lifts his finger. These events don't fire on the corresponding mouse events, eg. `mouseup`, and there lies the problem: 

* Setting mouse event handlers only works on mouse pointing and touch devices but with the delay induced on the latter. 
* Setting touch event handlers only completely misses mouse pointing devices which can only trigger mouse events.
* Setting both mouse and touch event handlers leads to even worse behaviour on touch devices: First the touch event handlers are triggered and then, 300 ms later, the one for the mouse events. So twice. 

## Solution

MouseTouch is an [Elm](http://elm-lang.org) library which unifies touch and mouse event handling into one interface. See this modified example of a [Counter](https://github.com/evancz/elm-architecture-tutorial/#1):
    
    import Html exposing (div, button, text)
    import MouseTouch exposing (onClick)

    type Action = Increment | Decrement | NoOp

    view address model =
      div [] 
        [ button ( onClick address Decrement ) [ text "-" ]
        , div [] [ text (toString model) ]
        , button ( onClick address Increment ) [ text "+" ]
        ]

    update action model =
      case action of
        Increment -> model + 1
        Decrement -> model - 1

    actions =
        Signal.mailbox NoOp
        
    main = Signal.map (view actions.address) 
           <| Signal.foldp update 0 actions.signal

This example will result in instant sending of `Increment` or `Decrement` to the app's `actions.address` irrespectively of whether the user clicks or taps the button. 

Try out a demo [here](https://pisys.github.io/elm-mousetouch/examples/taptest.html) (It's [TapTest.elm](TapTest.elm) running there). View it in a browser with mobile development capabilities. E.g. in Firefox press `Ctrl+Shift+M` and enable simulation of touch events. Or just visit the page from a touch device!

The only difference between `Html.Events.onClick` and `MouseTouch.onClick` is that the latter returns a `List` of `Attribute`s and that's it! MouseTouch integrates perfectly with your architecture because its API is reduced to event handler functions only. 

The module exposes the following event handlers which are analogous to the respective handlers of [Html.Events](github.com/evancz/elm-html):
* `on` for explicit combination with an evaluation function
* `onWithOptions` for additional options concerning event bubbling.
* `onMultiple` for multiple `on`s on the same element, eg. for detecting either swipe left or swipe right.
* `messageOn` for creating short-hand handlers for specific evaluation functions.

Instead of the mere event name, they receive an evaluation function as the first parameter of type `(PastEvents -> Bool)`. On each low-level event this function evaluates the past events for what we want to be the high-level event - a click, for instance. If its result is `True`, the message is sent to the specified address.

Actually the evaluation function completely hides the input mode of the user's device. So it is possible to model user interaction on HTML elements transparently. For instance, double click could be modelled as two mouse clicks occurring within 300 ms on the one hand and as a long tap of at least 150ms on the other hand, both patterns being evaluated within the same evaluation function. 

The submodule `MouseTouch.Eval` comes with some built-in evaluation functions:
* `click` detects `mousestart` and `mouseend` as well as `touchstart` together with `touchend` eevents. For the mouse case it also checks whether respective touch events happened shortly before. If that's the case we assume that the mouse events were virtual ones and thus can be ignored.
* `start` just detects `mousestart` and `touchstart` events. Just like `click` it also checks whether the mouse event might be a virtual one.
* `swipeUp/Down/Left/Right` detect - guess! - a swipe gesture (for now only works with touch events).

It is possible to define own evaluation functions. Look at the source of [click'](src/MouseTouch/Eval.elm#L25) for a complete example.

Evaluation functions can be wrapped into short-hands like:
    
    onClick : String -> Signal.Address a -> a -> List Attribute
    onClick =
        messageOn click second

Didn't I say that? The base event handlers `on` and `onWithOptions` receive the following parameters:

* `EvalFunction` which evaluates past events for a matching pattern
* `Options`, just like in `Html.Events`
* `Time`, the time how long past events shall be kept for evaluation. For instance, the evaluation function for a complex gesture might need to look at the events which happened within the last 2-3 seconds whereas `click` only needs to look one second back (A full second for a click? Yes, because we need to check that a mouse event isn't a virtual companion of a succeeding touch event).
* `String`, a key under which past events resulting from this event handler are grouped, so that the past events of other handlers don't get mixed in.
* `Json.Decoder a` and `(a -> Signal.Message)`: business as usual as with `Html.Events`. The decoder decodes the most recent event object (the one which completed the pattern for the evuation function) and the value is passed to the message creating function.

## Status

This library is in alpha state, more tests are needed. Comments and pull requests welcome!

## License

BSD-3
