import * as Core from "./core.js";
import {hamt} from "./core/lib.js";

/* --------------------------------------------------------------------------*/
// All exports go in exports

export const exports = {};
const Marks = Core.Marks;
const Box = Core.Box;

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
// `with-continuation-mark` or `parameterize`. `__locals` keeps the
// parameter value local to current async callback. When a callback is
// created current parameterization should be copied using ffi's "=>$"
// form.

const ParameterizationKey = {}; /* a unique reference that can act as key */
let __locals = undefined;

export function getCurrentParameterization() {
    return Marks.getFirstMark(Marks.getFrames(), ParameterizationKey, false);
}

export function makeParameter(initValue) {
    let param = function (maybeSetVal) {
	// Get current value box from parameterization. The function
	// `param` that we result is the key.
	let pv = getCurrentParameterization().get(param, false) ||
	    __locals.get(param, false);
	if (!pv) {
	    pv = Box.make(initValue);
	    __locals.set(param, pv);
	}
	if (maybeSetVal === undefined) {
	    return pv.get();
	} else {
	    pv.set(maybeSetVal);
	}
    }
    return param;
}

export function extendParameterization(parameterization, param, val) {
    return parameterization.set(param, Box.make(val));
}

export function copyParameterization(parameterization) {
    let result = hamt.make();
    for (let [key, val] of parameterization) {
	result = result.set(key, Box.make(val.get()));
    }
    return result;
}

/* --------------------------------------------------------------------------*/
// Init parameterization

(function () {
    let p = getCurrentParameterization();
    if (p !== false) {
	return;
    } else {
	Marks.setMark(ParameterizationKey, hamt.make());
    }
    __locals = new Map();

    Marks.registerAsynCallbackWrapper(function (oldFrames) {
	let oldP = Marks.getFirstMark(oldFrames, ParameterizationKey, false);
	let oldL = __locals;
	__locals = new Map();
	for (let [key, val] in oldL) {
	    __locals.set(key, Box.make(val.get()));
	}
	Marks.setMark(ParameterizationKey, copyParameterization(oldP));
    });
})();

/* --------------------------------------------------------------------------*/
// #%paramz exports

exports["parameterization-key"]    = ParameterizationKey;
exports["extend-parameterization"] = extendParameterization;
