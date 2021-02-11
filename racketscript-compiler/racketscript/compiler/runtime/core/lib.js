export { hamt } from './hamt.js';

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
