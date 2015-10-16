module MouseTouch.Eval where

{-| Built-in evaluation functions.

# Evaluation Functions
@docs click', click, start, start', swipeDown, swipeUp, swipeLeft, swipeRight

# Pretty Listeners
@docs onClick
-}

import Html exposing (Attribute)
import Time exposing (Time, millisecond, second) 
import MouseTouch exposing 
    ( Device(..), Action(..), PastEvents, EvalFunction
    , messageOn)

{-| A click evaluation function which has to be parametrized with the maximum range
between the start and end event (ie. mousedown/mouseup or touchstart/touchend).

Click searches past events for a pattern of mouseend (mousemove{0,5}) mousestart
or touchend (touchmove|mouseevent){0|5} touchstart. The latter ignores virtual 
mouse events intermingling with a fast series of touches.
-}
click' : Time -> Time -> PastEvents -> Bool
click' maxRange minLast events =
    let
        diffTouch = 
            case events of
                ((Touch, End), (time1, _))
                :: rest -> 
                    case List.filter 
                            (\((d,a),_) -> d == Touch && a /= Move) 
                            rest of
                        ((Touch, Start), (time2, _))
                        :: rest ->
                            Just <| time1 - time2
                        _ -> Nothing
                _ -> Nothing

        (diffMouse,lastTouch) =
            case events of
                ((Mouse, End), (time1, _))
                :: rest ->
                    case List.filter 
                            (\((d,a),_) -> d == Mouse && a /= Move) 
                            rest of
                        ((Mouse, Start), (time2,_))
                        :: rest -> 
                            ( Just <| time1 - time2,
                                case List.filter
                                        (\((d,a),_) -> d == Touch)
                                        events of
                                    ((Touch, _), (last,_))
                                    :: rest ->
                                        Just <| time2 - last
                                    _ ->
                                        Nothing
                            )
                        _ -> 
                            (Nothing,Nothing)
                _ -> 
                    (Nothing,Nothing)

    in
       case diffTouch of
           Nothing -> 
               case (diffMouse,lastTouch) of
                   (Just d, Nothing) -> 
                       d < maxRange
                   (Just d, Just l) ->
                       d < maxRange && l > minLast
                   _ -> False
           Just d ->
               d < maxRange

{-| A convenient click evaluation function in the sense of start and end happening 
within 300 ms and not past virtual mouse event must exist within the last 700 ms
-}
click : EvalFunction
click = 
    click' (300*millisecond) (700*millisecond)

{-| A simple mousestart/touchstart evaluation function. Takes the minimum time distance 
to the last mouse event. 
-}
start' : Time -> PastEvents -> Bool
start' minLast events =
    let 
        (time, last) =
            case events of
                ((d, Start), (time, _)) 
                :: rest ->
                    (Just time, 
                        if d == Mouse 
                           then 
                               case List.filter 
                                        (\((d,a),_) -> d == Touch) 
                                        rest of
                                   ((Touch,_),(last,_)) 
                                   :: rest ->
                                       Just last
                                   _ -> Nothing
                           else 
                               Nothing
                   )
                _ -> (Nothing, Nothing)

    in
       case time of
           Nothing -> False
           Just t -> 
               case Maybe.map ((-) t) last of 
                   Nothing -> True
                   Just l -> l > minLast

{-| A wrapper around `start'` configured with 700 milliseconds distance to last 
mouse event.
-}
start : EvalFunction
start =
    start' (700*millisecond) 

{-| Evaluating swiping down gesture (touch only)
-}
swipeDown : EvalFunction
swipeDown events = 
    let 
        ySmaller ((_,action),(_,(_,ynew))) (yold, wasSmaller) =
            let 
                newSmaller =
                    if action /= Move
                       then wasSmaller
                       else wasSmaller && yold > ynew
            in
                (ynew, newSmaller)
    in
        case events of
            ((Touch,End),(_,(xend,yend))) 
            :: rest -> 
                case List.foldl ySmaller (9999999,True) 
                        <| List.filter (\((d,_),_) -> d == Touch) rest of 
                    (_, True) ->
                        case List.filter (\((_,a),_) -> a == Start) rest of
                            ((Touch,Start),(_,(xstart,ystart)))
                            :: rest -> 
                                abs (xstart - xend) < 50
                                && abs (ystart - yend) > 50
                            _ -> False
                    (_, False) -> False
            _ -> False

{-| Evaluating swiping down gesture (touch only)
-}
swipeUp : EvalFunction
swipeUp events = 
    let 
        yBigger ((_,action),(_,(_,ynew))) (yold, wasSmaller) =
            let 
                newSmaller =
                    if action /= Move
                       then wasSmaller
                       else wasSmaller && yold < ynew
            in
                (ynew, newSmaller)
    in
        case events of
            ((Touch,End),(_,(xend,yend))) 
            :: rest -> 
                case List.foldl yBigger (-1,True) 
                        <| List.filter (\((d,_),_) -> d == Touch) rest of 
                    (_, True) ->
                        case List.filter (\((_,a),_) -> a == Start) rest of
                            ((Touch,Start),(_,(xstart,ystart)))
                            :: rest -> 
                                abs (xstart - xend) < 50
                                && abs (ystart - yend) > 50
                            _ -> False
                    (_, False) -> False
            _ -> False

{-| Evaluating swiping down gesture (touch only)
-}
swipeRight : EvalFunction
swipeRight events = 
    let 
        xSmaller ((_,action),(_,(xnew,_))) (xold, wasSmaller) =
            let 
                newSmaller =
                    if action /= Move
                       then wasSmaller
                       else wasSmaller && xold > xnew
            in
                (xnew, newSmaller)
    in
        case List.filter (\((d,_),_) -> d == Touch) events of
            ((Touch,End),(_,(xend,yend))) 
            :: rest -> 
                case List.foldl xSmaller (9999999,True) rest of 
                    (_, True) ->
                        case List.filter (\((_,a),_) -> a == Start) rest of
                            ((Touch,Start),(_,(xstart,ystart)))
                            :: rest -> 
                                abs (xstart - xend) > 50
                                && abs (ystart - yend) < 50
                            _ -> False
                    (_, False) -> False
            _ -> False

{-| Evaluating swiping down gesture (touch only)
-}
swipeLeft : EvalFunction
swipeLeft events = 
    let 
        xBigger ((_,action),(_,(xnew,_))) (xold, wasSmaller) =
            let 
                newSmaller =
                    if action /= Move
                       then wasSmaller
                       else wasSmaller && xold < xnew
            in
                (xnew, newSmaller)
    in
        case List.filter (\((d,_),_) -> d == Touch) events of
            ((Touch,End),(_,(xend,yend))) 
            :: rest -> 
                case List.foldl xBigger (-1,True) rest of 
                    (_, True) ->
                        case List.filter (\((_,a),_) -> a == Start) rest of
                            ((Touch,Start),(_,(xstart,ystart)))
                            :: rest -> 
                                abs (xstart - xend) > 50
                                && abs (ystart - yend) < 50
                            _ -> False
                    (_, False) -> False
            _ -> False

{-| Equivalent for `Html.Events.onClick`
-}
onClick : String -> Signal.Address a -> a -> List Attribute
onClick =
  messageOn click second

