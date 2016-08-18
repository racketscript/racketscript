import * as Primitive from "./primitive.js";
import {hash} from "../third-party/hash.js";

/* Equality checks */
export function isEqual(v1, v2) {
    if (Primitive.check(v1)) {
	return v1.equals(v2);
    } else {
	return v1 === v2;
    }
}

export function isEq(v1, v2) {
    // FIXME: We are not handling special case for Symbol
    return v1 === v2;
}

export let isEqv = isEq;

/* Hash functions */

export function hashEq(o) {
    return hash(o, false, false);
}

export function hashEqv(o) {
    return hash(o, true, false);
}

export function hashEqual(o) {
    return hash(o, true, true);
}

/* Other helpers */

export function toString(v) {
    if (Primitive.check(v)) {
	return v.toString();
    } else {
	return "" + v;
    }
}

export function attachReadOnlyProperty(o, k, v) {
    return Object.defineProperty(o, k, {
	value: v,
	writable: false,
	configurable: false
    });
}

export function internedMake(f) {
    let cache = {};
    return (v) => {
	if (v in cache) {
	    return cache[v];
	}
	let result = f(v);
	cache[v] = result;
	return result;
    }
}
