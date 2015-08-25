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

    view on model =
      div [] 
        [ button ( on click Decrement ) [ text "-" ]
        , div [] [ text (toString model) ]
        , button ( on click Increment ) [ text "+" ]
        ]

    update action model =
      case action of
        Increment -> model + 1
        Decrement -> model - 1
        
    main = Signal.map (view myTapBox.on) 
           <| Signal.foldp update 0 myTapBox.signal

This example will result in instant sending of `Increment` or `Decrement` to `myTapBox.signal` irrespectively of whether the user clicks or taps the button. TapBox functions like Elm's Mailbox with the difference that the event handler creating function `on` is exposed instead of an address. 

Additionally, instead of the mere event name, `on` receives a success function as the first parameter of type `(PastEvents -> Bool)` (PastEvents is a type alias for String). On each event this function evaluates the past string of events for a matching pattern that conforms to what we want to be a "click", for instance. If its result is `True` the signal of the TapBox changes to the defined value, eg. `Increment`. 

Actually the success function completely hides the input mode of the user's device. So it is possible to model user interaction on HTML elements transparently. For instance, double click could be modelled as two mouse clicks occurring within 250 ms on the one hand and as a long tap of at least 250ms on the other hand, both patterns being evaluated within the same success function. 

It is possible to define own success functions but you might need to look at the source of `click` to understand how. Here's a short explanation:

### Modelling device-agnostic events

First you need to know what `PastEvents` looks like. It is a string which encodes the past events in this form: 

    ([MT][selm]\d+)*

The symbols map to devices and actions, ie. 
    
    M => Mouse 
    T => Touch 
    s => start
    e => end
    l => leave
    m => move

followed by the time the event happened. An example of past events might look like this:

    Me500Ms450Te200Tm180Ts150

Timestamps are relative to a time base in order to save memory. In this case the last event was a `mouseend`, preceded by `mousestart`, `touchend`, `touchmove` and `touchstart`. All events resulted from the same tap. In this example you can also see the 300ms seconds delay between the touch and the mouse events. 

A success function can simply parse this string with regular expressions, submatch the timestamps, calculate differences if needed and decide whether it fulfills the requirements for the high level event (eg. the click). 

In the example above we would need a regex like the following in order to match the mouse click:

    ^Me(\d+)(?:Mm\d+){0,5}Ms(\d+)[^T]*(?:T.(\d+))?

Here we look for `mouseend` and `mousestart` and optionally up to 5 `mousemove`s in between to allow users to click a bit inaccurately. Apart from these events we look for a recent touch event. If there was one within a certain time frame (eg. 700ms) we assume that the mouse events were virtual ones and resulted from the same tap action. Hence we can ignore this match. It would have been processed already by the following pattern.

For detecting a tap we check for the corresponding touch events and additionally ignore any mouse events in between. That's because virtual mouse events can intermingle with touch events in case of a fast series of taps. 

    ^Te(\d+)(?:(?:Tm\d+)|(?:M.\d+)){0,5}Ts(\d+)

## Status

This library is in alpha state, more tests are needed. Comments and pull requests welcome!

## License

BSD-3
