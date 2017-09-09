export { hamt } from '../third-party/hamt.js';

// Because we don't have a wrapper type for bytes,
// we depend on it here for type-checking and toString conversion.
// TODO: extract toString to a separate file.
import * as Bytes from './bytes.js';

/* --------------------------------------------------------------------------*/
/* Strings */

/**
 * @param {*} v
 * @return {!String}
 */
export function toString(v) {
    if (v === true) return '#t';
    if (v === false) return '#f';
    if (v === undefined || v === null) return '#<void>';
    if (Bytes.check(v)) return Bytes.toString(v);
    return v.toString();
}

export function format1(pattern, args) {
    return pattern.toString().replace(/{(\d+)}/g, (match, number) => (typeof args[number] !== 'undefined'
        ? args[number]
        : match));
}

export function format(pattern, ...args) {
    return format1(pattern, args);
}

/* --------------------------------------------------------------------------*/
/* Errors */

function makeError(name) {
    const e = function (pattern, ...args) {
        this.name = name;
        this.message = format1(pattern, args);
        this.stack = (new Error()).stack;
        if (Error.captureStackTrace) {
            Error.captureStackTrace(this, this.constructor);
        } else {
            this.stack = (new Error()).stack;
        }
    };
    e.prototype = Object.create(Error.prototype);
    e.prototype.constructor = e;

    return (...args) =>
        new (Function.prototype.bind.apply(e, [this].concat(args)))();
}

export const racketCoreError = makeError('RacketCoreError');
export const racketContractError = makeError('RacketContractError');

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

/**
 * @param {function(String|UString.UString)} f
 * @return {function(String)}
 */
export function internedMake(f) {
    const cache = new Map();
    return (v) => {
        v = v.toString();
        let result = cache.get(v);
        if (result === undefined) {
            result = f(v);
            cache.set(v, result);
        }
        return result;
    };
}
