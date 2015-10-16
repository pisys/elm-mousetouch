module MouseTouch
    ( PastEvents, Event, Device(..), Action(..), EvalFunction
    , on, onWithOptions, messageOn, onMultiple
    ) 
    where

{-| TapBox wraps "low-level" mouse/touch events on HTML Elements and transforms
them into "high-level" events (eg. a click), hiding the user input method of 
the device (mouse, touch or both) and circumventing quirks like the 300ms delay 
of 'click' events on touch devices.

Low-level events are mousestart, mouseend, mousemove, mouseout, touchstart,
touchend, touchmove and touchleave.

High-level events are defined as evaluation functions which evaluate past 
low-level events.

# Definition
@docs PastEvents, Event, Device, Action, EvalFunction

# Listeners 
@docs on, onWithOptions, onMultiple, messageOn

-}

import Html exposing (Attribute)
import Html.Events exposing (onWithOptions, Options, defaultOptions)
import Time exposing (Time)
import Signal exposing (Address)
import Debug
import Json.Decode as Json
import Native.MouseTouch

{-| Devices.
-}
type Device = Mouse 
            | Touch

{-| Actions.
-}
type Action = Start 
            | End 
            | Leave 
            | Move

{-| Event coordinates.
-}
type alias Coords = (Int,Int)

{-| Normalized low-level events. This type could be extended to keep coordinates as well.
-}
type alias Event = ((Device, Action), (Time, Coords))

{-| The past events are a list of `Event`s. These need to be evaluated by an `EvalFunction`.
-}
type alias PastEvents = List Event

{-| The evaluation function evaluates past events.
-}
type alias EvalFunction = (PastEvents -> Bool)

{-| Event handler function which takes an evaluation function, 
the time how long low-level events shall be kept for evaluation, 
a key by which low-level events will be grouped, a decoder,
and a message creating function for the decoded valued.
Returns a list of `Attribute`s.
Equivalent of `Html.Events.on`.
-}
on : EvalFunction 
     -> Time
     -> String
     -> Json.Decoder a
     -> (a -> Signal.Message)
     -> List Attribute
on eval pruneBelow key decoder toMessage =
    Native.MouseTouch.on eval defaultOptions pruneBelow key decoder toMessage

{-| Same as `on` but you can set a few options.
Equivalent of `Html.Events.onWithOptions`.
-}
onWithOptions : EvalFunction
                -> Options
                -> Time
                -> String
                -> Json.Decoder a
                -> (a -> Signal.Message)
                -> List Attribute
onWithOptions =
    Native.MouseTouch.on

{-| For listening to more than one events patterns. Same as `on` but various configurations on the same DOM element.
-}
onMultiple : List 
                { eval : EvalFunction 
                , options : Options
                , pruneBelow : Time
                , key : String
                , decoder : Json.Decoder a
                , toMessage : (a -> Signal.Message)
                }
                 -> List Attribute
onMultiple =
    Native.MouseTouch.onMultiple 
        
{-| Provides inputs for `Address a` and `a` and passes them on to `on` in a `a -> Message` function.
-}
messageOn : EvalFunction -> Time -> String -> Signal.Address a -> a -> List Attribute
messageOn eval pruneBelow key addr msg =
  on eval pruneBelow key Json.value (\_ -> Signal.message addr msg)

