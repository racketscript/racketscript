import {hash} from "../third-party/hash.js";

export {hashString} from "../third-party/hash.js";
export {hamt} from "../third-party/hamt.js";

/* --------------------------------------------------------------------------*/
// Equality Checks */

export function isEqual(v1, v2) {
    if (isEqv(v1, v2)) {
	return true;
    } else if (typeof v1 === 'object' && typeof v2 === 'object' &&
	       v1.constructor !== v2.constructor) {
	return false;
    } else if (v1 instanceof Uint8Array && v2 instanceof Uint8Array // bytes
	       && v1.length === v2.length) {
	// TODO: bytes currently not interned, so wont be eq or eqv
	for (var i = 0; i < v1.length; i++) {
	    if (v1[i] !== v2[i]) return false;
	}
	return true;
    } else if (typeof v1.equals === 'function') {
	// Its a Primitive instance
	return v1.equals(v2) || false;
    } else {
	return false;
    }
}

export function isEqv(v1, v2) {
    // NOTE: We are not handling special case for Symbol.
    // Symbols and keywords are interned, so that's ok.
    return v1 === v2 || (typeof v1.valueOf === 'function' &&
			 typeof v2.valueOf === 'function' &&
			 v1.valueOf() === v2.valueOf());
}

export function isEq(v1, v2) {
    return v1 === v2;
}

/* --------------------------------------------------------------------------*/
// Hash Functions

export function hashEq(o) {
    return hash(o, false, false);
}

export function hashEqv(o) {
    return hash(o, true, false);
}

export function hashEqual(o) {
    return hash(o, true, true);
}

/* --------------------------------------------------------------------------*/
/* Strings */

export function toString(v) {
    //TODO: move displayln logic here and displayln should call toString
    return (v === undefined) ? "#<void>" : v.toString();
}

export function format1(pattern, args) {
    return pattern.replace(/{(\d+)}/g, function(match, number) { 
	return typeof args[number] != 'undefined'
	    ? args[number] 
	    : match;
    });
}

export function format(pattern, ...args) {
    return format1(pattern, args);
}

/* --------------------------------------------------------------------------*/
/* Arity */

export function attachProcedureArity(fn, arity) {
    if (arity === undefined || typeof arity === 'number' && arity >= 0) {
	fn.__rjs_lambdaType = 'variadic';
	fn.__rjs_arityValue = arity || fn.length;
    } else if (Array.isArray(arity)) {
	fn.__rjs_lambdaType = 'case-lambda';
	fn.__rjs_arityValue = arity;
    } else {
	throw racketCoreError("invalid arity provided");
    }
    return fn;
}

/* --------------------------------------------------------------------------*/
/* Errors */

function makeError(name) {
    let e = function(pattern, ...args) {
	this.name = name;
	this.message = format1(pattern, args);
	this.stack = (new Error()).stack;
	if (Error.captureStackTrace) {
            Error.captureStackTrace(this, this.constructor);
	} else {
            this.stack = (new Error()).stack;
	}
    }
    e.prototype = Object.create(Error.prototype);
    e.prototype.constructor = e;

    return (...args) =>
	new (Function.prototype.bind.apply(e, [this].concat(args)))
}

export let racketCoreError      = makeError("RacketCoreError");
export let racketContractError  = makeError("RacketContractError");

/* --------------------------------------------------------------------------*/
/* Other Helpers */

export function argumentsToArray(args) {
    return Array.prototype.slice.call(args, 0);
}

// Takes an array/arguemnt object and returns new //  array with first
// i items dropped.
//
// Eg. sliceArguments([1,2,3,4,5], 0)   => [ 1, 2, 3, 4, 5 ]
//     sliceArguments([1,2,3,4,5], 3)   => [ 4, 5 ]
//     sliceArguments([1,2,3,4,5], 10)  => []
export function argumentsSlice(a, i) {
    return [].slice.call(a, i);
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
