import { Marks, Box } from './core.js';
import { hamt } from './core/lib.js';

/* --------------------------------------------------------------------------*/
// Parameterization data structure is a HAMT Map keyed by parameter
// and the value is stored in a box. The parameter is function object
// which also acts as a key to look into parameterization map. When
// using `parameterize` form, extendParameterization creates a new box
// foreground the parameter to be used in context. Even though, its a
// new map, since each value is a box, any updates to other parameters
// is retained throughout its context, regardless whatever other
// parameter context we have created.
//
// When a parameter is created, it keeps an stores the initial value
// inside. An entry is not added to parameterization map. An entry is
// created in this map only when a context is created using
// `with-continuation-mark` or `parameterize`. `__top` keeps the top
// level parameter value local to current async callback. When a
// callback is created current parameterization should be copied using
// ffi's "=>$" form.

export const ParameterizationKey = {}; /* a unique reference that can act as key */
export const BreakEnabledKey = {}; /* a unique reference that can act as key */
export const ExceptionHandlerKey = {}; /* a unique reference that can act as key */
let __top;

export function getCurrentParameterization() {
    return Marks.getFirstMark(Marks.getFrames(), ParameterizationKey, false);
}

export function makeParameter(initValue) {
    const param = function (maybeSetVal) {
        // Get current value box from parameterization. The function
        // `param` that we result is the key.
        const current = getCurrentParameterization();
        let pv = (current && current.get(param, false)) ||
            __top.get(param, false);
        // Create entry in __top if its a mutation.
        if (!pv && maybeSetVal !== undefined) {
            pv = Box.make(initValue);
            __top.set(param, pv);
        }
        // Get/Set
        if (maybeSetVal === undefined) {
            return pv ? pv.get() : initValue;
        }
        pv.set(maybeSetVal);
    };
    return param;
}

export function extendParameterization(parameterization, ...args) {
    let result = parameterization;
    for (let i = 0; i < args.length; i += 2) {
        result = result.set(args[i], Box.make(args[i + 1]));
    }
    return result;
}

export function copyParameterization(parameterization) {
    let result = hamt.make();
    for (const [key, val] of parameterization) {
        result = result.set(key, Box.make(val.get()));
    }
    return result;
}

/* --------------------------------------------------------------------------*/
// Init parameterization

(function () {
    const p = getCurrentParameterization();
    if (p !== false) {
        return;
    }
    Marks.setMark(ParameterizationKey, hamt.make());

    __top = new Map();

    Marks.registerAsynCallbackWrapper({
        onCreate(state) {
            const paramz = {};
            paramz.top = new Map();
            for (const [key, val] of __top) {
                paramz.top.set(key, Box.make(val.get()));
            }

            paramz.bottom = copyParameterization(Marks.getFirstMark(Marks.getFrames(), ParameterizationKey, false));
            state.paramz = paramz;
        },
        onInvoke(state) {
            __top = state.paramz.top;
            Marks.setMark(ParameterizationKey, state.paramz.bottom);
        }
    });
}());
