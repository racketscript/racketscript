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
// When a parameter is created, it keeps an internal box with default
// value. An entry is not added to parameterization map. An entry is
// created in this map only when a context is created using
// `with-continuation-mark` or `parameterize`. This intenal box is
// used otherwise for both setting and getting value otherwise. This
// helps us to pass parameter around more conveniently, including the
// callbacks where we get completely different context.

const ParameterizationKey = {}; /* a unique reference that can act as key */

export function getCurrentParameterization() {
    return Marks.getFirstMark(Marks.getFrames(), ParameterizationKey, false);
}

export function makeParameter(initValue) {
    let valBox = Box.make(initValue);
    let param = function (maybeSetVal) {
	// Get current value box from parameterization. The function
	// `param` that we result is the key.
	let pv = getCurrentParameterization().get(param, false) || valBox;
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

/* --------------------------------------------------------------------------*/
// Init parameterization

(function () {
    let p = getCurrentParameterization();
    if (p !== false) {
	return;
    } else {
	Marks.setMark(ParameterizationKey, hamt.make());
    }
})();

/* --------------------------------------------------------------------------*/
// #%paramz exports

exports["parameterization-key"]    = ParameterizationKey;
exports["extend-parameterization"] = extendParameterization;
