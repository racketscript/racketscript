export { hamt } from "../third-party/hamt.js";

// Because we don't have a wrapper type for bytes,
// we depend on it here for type-checking and toString conversion.
// TODO: extract toString to a separate file.
import * as Bytes from "./bytes.js";

/* --------------------------------------------------------------------------*/
/* Strings */

/**
 * @param {*} v
 * @return {!String}
 */
export function toString(v) {
    if (v === true) return "#t";
    if (v === false) return "#f";
    if (v === undefined || v === null) return "#<void>";
    if (Bytes.check(v)) return Bytes.toString(v);
    return v.toString();
}

export function format1(pattern, args) {
    return pattern.toString().replace(/{(\d+)}/g, function (match, number) {
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
    fn.__rjs_arityValue = arity || fn.length;
    return fn;
}

/* --------------------------------------------------------------------------*/
/* Errors */

function makeError(name) {
    let e = function (pattern, ...args) {
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

export let racketCoreError = makeError("RacketCoreError");
export let racketContractError = makeError("RacketContractError");

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
