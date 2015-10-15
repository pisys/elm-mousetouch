Elm.Native.TapBox = {};
Elm.Native.TapBox.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.TapBox = localRuntime.Native.TapBox || {};
    if (localRuntime.Native.TapBox.values)
    {
        return localRuntime.Native.TapBox.values;
    }
    if ('values' in Elm.Native.TapBox)
    {
        return localRuntime.Native.TapBox.values = Elm.Native.TapBox.values;
    }

    var VirtualDom = Elm.Native.VirtualDom.make(localRuntime);
    var Signal = Elm.Native.Signal.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);

    var that = this;
    that.pastEvents = {};

    function pruneEvents(pastEvents) {
        var pruneBelow = (new Date()).getTime() - 1000;
        for(var i = 0; i < pastEvents.length; i++) {
            if(pastEvents[i]._1 < pruneBelow) break;
        }
        return pastEvents.slice(0, i);
    }

    function onWithOptions(key, evalFunction, options, message) {
        that.pastEvents[key] = that.pastEvents[key] || [];
        console.log("enter", that.pastEvents);
        function eventHandler(lle, event) {
            console.log('event', event);
            if (options.stopPropagation)
            {
                event.stopPropagation();
            }
            if (options.preventDefault)
            {
                event.preventDefault();
            }
            that.pastEvents[key].unshift(Utils.Tuple2(lle, (new Date()).getTime()));

            that.pastEvents[key] = pruneEvents(that.pastEvents[key]);

            if( evalFunction(List.fromArray(that.pastEvents[key])) ) {
                Signal.sendMessage(message);
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

    return Elm.Native.TapBox.values = {
        onWithOptions : F4(onWithOptions)
    };
}
