Elm.Native.MouseTouch = {};
Elm.Native.MouseTouch.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.MouseTouch = localRuntime.Native.MouseTouch || {};
    if (localRuntime.Native.MouseTouch.values)
    {
        return localRuntime.Native.MouseTouch.values;
    }
    if ('values' in Elm.Native.MouseTouch)
    {
        return localRuntime.Native.MouseTouch.values = Elm.Native.MouseTouch.values;
    }

    var VirtualDom = Elm.Native.VirtualDom.make(localRuntime);
    var Signal = Elm.Native.Signal.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);
    var List = Elm.Native.List.make(localRuntime);
    var Json = Elm.Native.Json.make(localRuntime);

    var that = this;
    that.pastEvents = {};

    function pruneEvents(pruneBelow, pastEvents) {
        var pruneBelowAbs = (new Date()).getTime() - pruneBelow;
        for(var i = 0; i < pastEvents.length; i++) {
            if(pastEvents[i]._1._0 < pruneBelowAbs) break;
        }
        return pastEvents.slice(0, i);
    }

    function getCoords(event) {
        var x,y;
        switch (event.type.substr(0,5)) {
            case "mouse":
                x = event.clientX;
                y = event.clientY;
                break;
            case "touch":
                switch (event.type.substr(5)) {
                    case "end" :
                        x = event.changedTouches[0].clientX;
                        y = event.changedTouches[0].clientY;
                        break;
                    default:
                        x = event.touches[0].clientX;
                        y = event.touches[0].clientY;
                }
                break;
            default :
                x = -1;
                y = -1;
        }

        return Utils.Tuple2(x,y);
    }

    function getEventHandler(evalFunction, options, pruneBelow, key, decoder, createMessage) {
        that.pastEvents[key] = that.pastEvents[key] || [];

        return function (lle, event) {
            if (options.stopPropagation)
            {
                event.stopPropagation();
            }
            if (options.preventDefault)
            {
                event.preventDefault();
            }

            var newEvent = Utils.Tuple2
                ( lle
                , Utils.Tuple2
                    ((new Date()).getTime()
                    , getCoords(event)
                    )
                );

            that.pastEvents[key].unshift(newEvent);

            that.pastEvents[key] = pruneEvents(pruneBelow, that.pastEvents[key]);

            if( evalFunction(List.fromArray(that.pastEvents[key])) ) {
                var value = A2(Json.runDecoderValue, decoder, event);
                if (value.ctor === 'Ok')
                {
                    Signal.sendMessage(createMessage(value._0));
                }
            }
        }
    }

    function toList(configs) {
        var list = [];
        var config = configs;
        while( config.ctor == "::" ) {
            var eventHandler = getEventHandler(
                                 config._0.$eval,
                                 config._0.options,
                                 config._0.pruneBelow,
                                 config._0.key,
                                 config._0.decoder,
                                 config._0.toMessage
                                );
            list.push(eventHandler);
            config = config._1;
        }
        return list;
    }

    function onList(list) {
        function executeEval (e, device, action) {
            for ( var i = 0; i < list.length; i++ ) {
                list[i]
                    ( Utils.Tuple2({ctor:device},{ctor:action})
                    , e
                    )
            }
        }

        return List.fromArray(
            [ VirtualDom.property.func("onmousedown", 
                    function(e){
                        executeEval(e, "Mouse", "Start");
                    })
            , VirtualDom.property.func("onmouseup", 
                    function(e){
                        executeEval(e, "Mouse", "End");
                    })
            , VirtualDom.property.func("onmouseout", 
                    function(e){
                        executeEval(e, "Mouse", "Leave");
                    })
            , VirtualDom.property.func("onmousemove", 
                    function(e){
                        executeEval(e, "Mouse", "Move");
                    })
            , VirtualDom.property.func("ontouchstart", 
                    function(e){
                        executeEval(e, "Touch", "Start");
                    })
            , VirtualDom.property.func("ontouchend", 
                    function(e){
                        executeEval(e, "Touch", "End");
                    })
            , VirtualDom.property.func("ontouchleave", 
                    function(e){
                        executeEval(e, "Touch", "Leave");
                    })
            , VirtualDom.property.func("ontouchmove", 
                    function(e){
                        executeEval(e, "Touch", "Move");
                    })
            ]
        );
    }

    function on(evalFunction, options, pruneBelow, key, decoder, createMessage) {
        var eventHandler = getEventHandler(evalFunction, options, pruneBelow, key, decoder, createMessage);
        return onList([eventHandler]);
    }

    function onMultiple(configs) {
        return onList(toList(configs));
    }

    return Elm.Native.MouseTouch.values = {
        on : F6(on),
        onMultiple : onMultiple
    };
}
