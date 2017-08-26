import {hash} from "../third-party/hash.js";

import * as Primitive from "./primitive.js";
import * as Char from "./char.js";
import * as Bytes from "./bytes.js";

/**
 * @return {!number}
 * @param {*} o
 */
export function hashForEq(o) {
    return hash(o, false, false);
}

/**
 * @return {!number}
 * @param {*} o
 */
export function hashForEqv(o) {
    if (Char.check(o)) return o.codepoint;

    // TODO: Handle numbers.
    return hash(o, true, false);
}

/**
 * @return {!number}
 * @param {*} o
 */
export function hashForEqual(o) {
    if (Primitive.check(o)) return o.hashForEqual();

    return hash(o, true, true);
}
