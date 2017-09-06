import {Decoder as TextDecoder} from "./text_transcoder.js";
import {hashIntArray} from "./raw_hashing.js";

/**
 * @param {*} bs
 * @return {!boolean}
 */
export function check(bs) {
    return typeof bs === 'object' && bs !== null &&
        bs.constructor === Uint8Array;
}

/**
 *
 * @param {!Uint8Array} a
 * @param {!Uint8Array} b
 * @return {!boolean}
 */
export function eq(a, b) {
    if (a.length !== b.length) return false;
    const n = a.length;
    for (let i = 0; i < n; i++) {
        if (a[i] !== b[i]) return false;
    }
    return true;
}

/**
 * @param {!number[]} ints non-negative integers less than 256.
 * @return {!Uint8Array}
 */
export function fromIntArray(ints) {
    return new Uint8Array(ints);
}

const utf8Decoder = new TextDecoder('utf-8');

/**
 * @param {!Uint8Array} bytes
 * @return {!String}
 */
export function toString(bytes) {
    return utf8Decoder.decode(bytes);
}

/**
 * Writes a string representation similar to Racket's `display` to the given port.
 *
 * @param {!Ports.NativeStringOutputPort} out
 * @param {!Uint8Array} bytes
 */
export function displayNativeString(out, bytes) {
    out.consume(toString(bytes));
}

/**
 * @param {!Uint8Array} bytes
 * @return {!number} a 32-bit integer
 */
export function hashForEqual(bytes) {
    return hashIntArray(bytes);
}
