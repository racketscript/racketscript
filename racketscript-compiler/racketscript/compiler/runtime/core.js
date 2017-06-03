// Exports classes for creating basic data types and operation on them

import * as Box from "./core/box.js";
import * as Hash from "./core/hash.js";
import * as Keyword from "./core/keyword.js";
import * as Number from "./core/numbers.js";
import * as Pair from "./core/pair.js";
import * as Ports from "./core/ports.js";
import * as Primitive from "./core/primitive.js";
import * as Struct from "./core/struct.js";
import * as Symbol from "./core/symbol.js";
import * as Values from "./core/values.js";
import * as Vector from "./core/vector.js";
import * as Marks from "./core/marks.js";
import * as MPair from "./core/mpair.js";


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
    Box,
    Marks,
    Ports,
    MPair
}

export {
    toString,
    format,

    isEq,
    isEqv,
    isEqual,

    hashEqual,
    hashEq,
    hashEqv,

    argumentsToArray,
    argumentsSlice,

    attachProcedureArity,

    racketCoreError,
    racketContractError
} from "./core/lib.js";

//;-----------------------------------------------------------------------------

export function bitwiseNot(a) {
    return ~a;
}
