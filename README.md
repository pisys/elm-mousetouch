# elm-tapbox
Unify touch/mouse events on HTML elements into a user defined signal.

## Problem

An `onClick` handler on an HTML element leads to different results on mouse pointing and touch devices. The latter induces a delay of approx. 300 ms prior to execution of the handler. This is due to default gestures supported in mobile Web engines. For instance double tapping usually zooms into the page. So after a tap the engine waits for another one in order to distinguish zooming from actual clicking. However, the delay of 300 ms affects the responsiveness of a Web app and makes it feel slow. 

The [Touch Events](http://www.w3.org/TR/touch-events/) can work around this for instance by setting a handler on `touchend`, which will be executed the moment the user lifts his finger. These events don't fire on the corresponding mouse events, eg. `mouseup`, and there lies the problem: 

* Setting mouse event handlers only works on mouse pointing and touch devices but with the delay induced on the latter. 
* Setting touch event handlers only completely misses mouse pointing devices which can only trigger mouse events.
* Setting both mouse and touch event handlers leads to even worse behaviour on touch devices: First the touch event handlers are triggered and then, 300 ms later, the one for the mouse events. So twice. 

## Solution

TapBox is an [Elm](http://elm-lang.org) library which unifies touch and mouse event handling into one interface. See this modified example of a [Counter](https://github.com/evancz/elm-architecture-tutorial/#1):

    type Action = Increment | Decrement | NoOp

    myTapBox : TapBox Action
    myTapBox = tapbox NoOp second

    view address model =
      div [] 
        [ button ( on click address Decrement ) [ text "-" ]
        , div [] [ text (toString model) ]
        , button ( on click address Increment ) [ text "+" ]
        ]

    update action model =
      case action of
        Increment -> model + 1
        Decrement -> model - 1
        
    main = Signal.map (view myTapBox.address) 
           <| Signal.foldp update 0 myTapBox.signal

This example will result in instant sending of `Increment` or `Decrement` to `myTapBox.signal` irrespectively of whether the user clicks or taps the button. TapBox functions like Elm's `Mailbox`. 

The module exposes `on` and `onWithOptions` event handlers. Instead of the mere event name, they receive an evaluation function as the first parameter of type `(PastEvents -> Bool)`. On each low-level event this function evaluates the past events for what we want to be the high-level event - a click, for instance. If its result is `True` the signal of the TapBox changes to the defined value, eg. `Increment`. 

Actually the success function completely hides the input mode of the user's device. So it is possible to model user interaction on HTML elements transparently. For instance, double click could be modelled as two mouse clicks occurring within 300 ms on the one hand and as a long tap of at least 150ms on the other hand, both patterns being evaluated within the same evaluation function. 

It is possible to define own evaluation functions. 
Look at the source of [click'](src/TapBox.elm#L265) for a complete example.

## Status

This library is in alpha state, more tests are needed. Comments and pull requests welcome!

## License

BSD-3
