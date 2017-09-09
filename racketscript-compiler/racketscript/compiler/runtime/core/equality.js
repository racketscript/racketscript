import * as Primitive from './primitive.js';
import * as Char from './char.js';
import * as Bytes from './bytes.js';

/**
 * @param {*} v1
 * @param {*} v2
 * @return {!boolean}
 */
export function isEq(v1, v2) {
    return v1 === v2;
}

/**
 * @param {*} v1
 * @param {*} v2
 * @return {!boolean}
 */
export function isEqv(v1, v2) {
    // NOTE: We are not handling special case for Symbol.
    // Symbols and keywords are interned, so that's ok.

    // TODO: Handle numbers correctly.
    return v1 === v2 ||
        Char.check(v1) && Char.check(v2) && Char.eq(v1, v2);
}

/**
 * @param {*} v1
 * @param {*} v2
 * @return {!boolean}
 */
export function isEqual(v1, v2) {
    if (v1 === v2) return true;
    if (Primitive.check(v1)) return v1.equals(v2);

    // Bytes are not a Primitive.
    if (Bytes.check(v1) && Bytes.check(v2)) return Bytes.eq(v1, v2);

    return false;
}
