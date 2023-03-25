// Exports classes for creating basic data types and operation on them

import { PrintablePrimitive } from './core/printable_primitive.js';
import * as Box from './core/box.js';
import * as Bytes from './core/bytes.js';
import * as Char from './core/char.js';
import * as UString from './core/unicode_string.js';
import * as Regexp from './core/regexp.js';
import * as Hash from './core/hash.js';
import * as Keyword from './core/keyword.js';
import * as Number from './core/numbers/numbers.js';
import * as Pair from './core/pair.js';
import * as Ports from './core/ports.js';
import * as Primitive from './core/primitive.js';
import * as PrimitiveSymbol from './core/primitive_symbol.js';
import * as Struct from './core/struct.js';
import * as Values from './core/values.js';
import * as Vector from './core/vector.js';
import * as Marks from './core/marks.js';
import * as MPair from './core/mpair.js';
import * as Correlated from './core/correlated.js';
import * as Linklet from './core/linklet.js';
import * as Path from './core/path.js';

export {
    Bytes,
    Number,
    Char,
    Pair,
    Primitive,
    Struct,
    PrimitiveSymbol,
    Keyword,
    Values,
    Vector,
    Hash,
    Box,
    Marks,
    Ports,
    UString,
    Regexp,
    MPair,
    Correlated,
    Linklet,
    Path
};

export { argumentsToArray, argumentsSlice } from './core/lib.js';

export {
    racketCoreError,
    racketContractError,
    makeArgumentError,
    makeResultError,
    makeArgumentsError,
    makeMismatchError,
    makeOutOfRangeError,
    isContractErr,
    isErr,
    errMsg
} from './core/errors.js';

export { attachProcedureArity, attachProcedureName } from './core/procedure.js';

export { isEq, isEqv, isEqual } from './core/equality.js';

export { hashForEq, hashForEqv, hashForEqual } from './core/hashing.js';

export { display, write, print } from './core/printing.js';

// ;-----------------------------------------------------------------------------

export function bitwiseNot(a) {
    return ~a;
}

class UnsafeUndefined extends PrintablePrimitive {
    equals(v) {
        return v === this;
    }

    /**
   * @return {!number} a 32-bit integer
   */
    hashForEqual() {
        return 0;
    }

    /**
   * @param {!Ports.NativeStringOutputPort} out
   */
    displayNativeString(out) {
        out.consume('#<unsafe-undefined>');
    }

    /**
   * @param {!Ports.UStringOutputPort} out
   */
    displayUString(out) {
        out.consume('#<unsafe-undefined>');
    }

    /**
   * @param {!Ports.NativeStringOutputPort} out
   */
    writeNativeString(out) {
        out.consume('#<unsafe-undefined>');
    }

    /**
   * @param {!Ports.UStringOutputPort} out
   */
    writeUString(out) {
        out.consume('#<unsafe-undefined>');
    }
}

// eslint-disable-next-line no-unused-vars
export const theUnsafeUndefined = new UnsafeUndefined();
