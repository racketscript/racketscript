// Exports classes for creating basic data types and
// operation on them
import {default as Number} from "./core/numbers.js";
import {RacketCoreError} from "./core/error.js";
import * as Pair from "./core/pair.js";
import * as Primitive from "./core/primitive.js";
import * as Struct from "./core/struct.js";
import * as Symbol from "./core/symbol.js";
import * as Keyword from "./core/keyword.js";
import * as Values from "./core/values.js";
import * as Vector from "./core/vector.js";
import * as Hash from "./core/hash.js";
import * as rutils from "./core/utils.js";

/**
   Convert JS `arguments` object to plain JS array
*/
function argumentsToArray(a) {
    // arguments are not exactly array
    return (a.length === 1 ? [a[0]] : Array.apply(null, a));
}

/**
   Takes an array/arguemnt object and returns new 
   array with first i items dropped
   
   eg.
   > sliceArguments([1,2,3,4,5], 0)
   [ 1, 2, 3, 4, 5 ]
   > sliceArguments([1,2,3,4,5], 3)
   [ 4, 5 ]
   > sliceArguments([1,2,3,4,5], 10)
   []
*/
function sliceArguments(a, i) {
    return [].slice.call(a, i);
}

export {
    Number,
    Pair,
    Primitive,
    Struct,
    Symbol,
    Keyword,
    Values,
    Vector,
    Hash,
    argumentsToArray,
    sliceArguments,
    RacketCoreError
}

export {
    toString,
    isEq,
    isEqv,
    isEqual,
    hashEqual,
    hashEq,
    hashEqv
} from "./core/utils.js";
