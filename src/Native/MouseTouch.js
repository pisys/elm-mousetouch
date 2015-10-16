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

    function on(evalFunction, options, pruneBelow, key, decoder, createMessage) {
        that.pastEvents[key] = that.pastEvents[key] || [];
        function eventHandler(lle, event) {
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
        return List.fromArray(
            [ VirtualDom.property.func("onmousedown", 
                    function(e){
                        eventHandler(
                            Utils.Tuple2({ctor:"Mouse"},{ctor:"Start"}), 
                            e
                        )
                    })
            , VirtualDom.property.func("onmouseup", 
                    function(e){
                        eventHandler(
                            Utils.Tuple2({ctor:"Mouse"},{ctor:"End"}), 
                            e
                        )
                    })
            , VirtualDom.property.func("onmouseout", 
                    function(e){
                        eventHandler(
                            Utils.Tuple2({ctor:"Mouse"},{ctor:"Leave"}), 
                            e
                        )
                    })
            , VirtualDom.property.func("onmousemove", 
                    function(e){
                        eventHandler(
                            Utils.Tuple2({ctor:"Mouse"},{ctor:"Move"}), 
                            e
                        )
                    })
            , VirtualDom.property.func("ontouchstart", 
                    function(e){
                        eventHandler(
                            Utils.Tuple2({ctor:"Touch"},{ctor:"Start"}), 
                            e
                        )
                    })
            , VirtualDom.property.func("ontouchend", 
                    function(e){
                        eventHandler(
                            Utils.Tuple2({ctor:"Touch"},{ctor:"End"}), 
                            e
                        )
                    })
            , VirtualDom.property.func("ontouchleave", 
                    function(e){
                        eventHandler(
                            Utils.Tuple2({ctor:"Touch"},{ctor:"Leave"}), 
                            e
                        )
                    })
            , VirtualDom.property.func("ontouchmove", 
                    function(e){
                        eventHandler(
                            Utils.Tuple2({ctor:"Touch"},{ctor:"Move"}), 
                            e
                        )
                    })
            ]
        );
    }

    return Elm.Native.MouseTouch.values = {
        on : F6(on)
    };
}
