// Exports classes for creating basic data types and operation on them

import './core/polyfills/string.js';

import * as Primitive from './core/primitive.js';
import './core/primitive_print.js';

import * as Box from "./core/box.js";
import * as Bytes from "./core/bytes.js";
import * as Char from "./core/char.js";
import * as UString from "./core/unicode_string.js";
import * as Regexp from "./core/regexp.js";
import * as Hash from "./core/hash.js";
import * as Keyword from "./core/keyword.js";
import * as Number from "./core/numbers.js";
import * as Pair from "./core/pair.js";
import * as Ports from "./core/ports.js";
import * as Struct from "./core/struct.js";
import * as Symbol from "./core/symbol.js";
import * as Values from "./core/values.js";
import * as Vector from "./core/vector.js";
import * as Marks from "./core/marks.js";
import * as MPair from "./core/mpair.js";

export {
    Bytes,
    Number,
    Char,
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
    UString,
    Regexp,
    MPair
}

export {
    openOutputString,
    getOutputString,
} from './core/ports_ustring.js';

export {
    argumentsToArray,
    argumentsSlice,
} from "./core/lib.js";

export {
    racketCoreError,
    racketContractError,
} from "./core/errors.js";

export {
    attachProcedureArity,
} from "./core/procedure.js";

export {
    isEq,
    isEqv,
    isEqual,
} from "./core/equality.js";

export {
    hashForEq,
    hashForEqv,
    hashForEqual,
} from "./core/hashing.js";

export {
    display,
    write,
    print,
    isPrintAsExpression,
    setPrintAsExpression,
} from "./core/print.js";

//;-----------------------------------------------------------------------------

export function bitwiseNot(a) {
    return ~a;
}
