import * as Bytes from './bytes.js';
import * as UString from './unicode_string.js';
import * as Pair from './pair.js';
import { racketContractError } from './errors.js';

/**
 * @param {*} v
 * @return {!boolean}
 */
export function check(v) {
    return v instanceof RegExp;
}

/**
 * @param {!UString.UString} str
 * @return {!RegExp}
 */
export function fromString(str) {
    // Pass 'u' once we drop support for Node 5.
    return new RegExp(str.toString());
}

/**
 * @param {!(RegExp|Uint8Array|UString.UString)} pattern
 * @param {!(Uint8Array|UString.UString)} input
 * @param {!Int} start-pos
 * @param {!Int|#f} end-pos
 * @return {!Pair.Pair|false} A list of bytes or strings, depending on the input.
 */
export function match(pattern, input, start, _end) {
    // TODO: Contract-checking should happen in kernel.rkt.
    const isRegexpPattern = check(pattern);
    const isBytesPattern = !isRegexpPattern && Bytes.check(pattern);
    const isStringPattern = !isRegexpPattern && !isBytesPattern && UString.check(pattern);
    const isBytesInput = Bytes.check(input);
    const isStringInput = !isBytesInput && UString.check(input);

    if (!(isRegexpPattern || isBytesPattern || isStringPattern)
        || !(isBytesInput || isStringInput)) {
        throw racketContractError('expected regexp, string or byte pat, ' +
            'and string or byte input, got pattern:', pattern, ', input:', input);
    }

    /** @type {!UString.UString} */
    const stringInput = isBytesInput
        ? UString.fromBytesUtf8(/** @type {!Uint8Array} */(input))
        : input;

    /** @type {!(RegExp|UString.UString)} */
    const stringOrRegExpPattern = isBytesPattern
        ? UString.fromBytesUtf8(/** @type {!Uint8Array} */(pattern))
        : pattern;

    const end = ((typeof _end) === 'number') ? _end : stringInput.length;

    const result = stringInput.toString().slice(start, end).match(stringOrRegExpPattern);

    if (result === null) {
        return false;
    }
    if ((isStringPattern || isRegexpPattern) && isStringInput) {
        return Pair.listFromArray(result.map(x => (x !== undefined
            ? UString.makeMutable(x)
            : false)));
    }
    return Pair.listFromArray(result.map(x => (x !== undefined
        ? UString.toBytesUtf8(x)
        : false)));
}
