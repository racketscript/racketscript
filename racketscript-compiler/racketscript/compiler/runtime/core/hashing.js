import { hash } from './raw_hashing.js';
import * as Primitive from './primitive.js';
import * as Char from './char.js';
import * as Bytes from './bytes.js';

/**
 * @param {*} o
 * @return {!number} a 32-bit integer
 */
export function hashForEq(o) {
    return hash(o);
}

/**
 * @param {*} o
 * @return {!number} a 32-bit integer
 */
export function hashForEqv(o) {
    if (Char.check(o)) return o.codepoint;

    // TODO: Handle numbers.
    return hash(o);
}

/**
 * @param {*} o
 * @return {!number} a 32-bit integer
 */
export function hashForEqual(o) {
    if (Primitive.check(o)) return o.hashForEqual();
    if (Bytes.check(o)) return Bytes.hashForEqual(o);

    return hash(o);
}
