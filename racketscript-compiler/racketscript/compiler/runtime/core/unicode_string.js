import { Primitive } from './primitive.js';
import * as Bytes from './bytes.js';
import * as Char from './char.js';
import * as Pair from './pair.js';
import * as $ from './lib.js';
import { internedMake } from './lib.js';
import { hashIntArray } from './raw_hashing.js';

// In node.js, TextEncoder is not global and needs to be imported.
if (typeof TextEncoder === 'undefined') {
    var TextEncoder = require('util').TextEncoder;
}

/**
 * A sequence of {Char.Char}s.
 *
 * See {Char.Char} for implementation notes.
 *
 * @abstract
 */
export class UString extends Primitive {
    /**
     * @param {!Char.Char[]} chars
     * @param {(string|null)} nativeString
     * @private
     */
    constructor(chars, nativeString) {
        super();
        this.chars = chars;
        this._nativeString = nativeString;
        this._cachedHashCode = null;
    }

    /**
     * @return {!number}
     */
    get length() {
        return this.chars.length;
    }

    /**
     * @return {!string}
     */
    toString() {
        if (this._nativeString === null) {
            this._nativeString = this.chars.join('');
        }
        return this._nativeString;
    }

    /**
     * @param {!number} i
     * @return {!Char.Char}
     */
    charAt(i) {
        this.checkIndexLtLength(i);
        return this.chars[i];
    }

    /**
     * @return {!MutableUString}
     */
    toLowerCase() {
        // TODO: Improve Racket compatibility.
        return makeMutable(this.toString().toLowerCase());
    }

    /**
     * @return {!MutableUString}
     */
    toUpperCase() {
        // TODO: Improve Racket compatibility.
        return makeMutable(this.toString().toUpperCase());
    }

    /**
     * @param {!number} start
     * @param {!number} end
     * @return {!MutableUString}
     */
    substring(start, end) {
        // RacketScript guarantees:
        //   start >= 0, end >= 0, end <= length, end >= start.
        // This might be faster if we keep the indices with the chars.
        return new MutableUString(this.chars.slice(start, end), null);
    }

    /**
     * @param {!(string|RegExp)} sep
     */
    split(sep) {
        // Known issue: if sep is a regex, it will not support Unicode fully.
        return this.toString().split(sep).map(s => makeMutable(s));
    }

    /**
     *
     * @param {!number} i
     */
    checkIndexLtLength(i) {
        if (i >= this.length) {
            throw $.racketContractError(this.length > 0
                ? `string-ref: index is out of range
  index: ${i}
  valid range: [0, ${this.length - 1}]
  string: ${this.toRawString()}`
                : `string-ref: index is out of range for empty string
  index: ${i}`);
        }
    }

    /**
     * Warning: JavaScript string comparison does not work correctly
     *   for Unicode strings with non-BMP characters.
     *
     * @return {!string}
     */
    valueOf() {
        return this.toString();
    }

    /**
     * @param {*} v
     * @return {!boolean}
     */
    equals(v) {
        return check(v) && this.toString() === v.toString();
    }

    /**
     * @return {!number} a 32-bit integer
     */
    hashForEqual() {
        if (this._cachedHashCode === null) {
            this._cachedHashCode = hashIntArray(this.chars);
        }
        return this._cachedHashCode;
    }

    /**
     * As source-code literal representation of this string.
     */
    toRawString() {
        // TODO: Improve Racket compatibility (e.g. unicode escapes).
        return JSON.stringify(this.toString());
    }
}

/**
 * An immutable sequence of {Char.Char}s.
 */
class ImmutableUString extends UString {
    /**
     * @return {true}
     */
    isImmutable() {
        return true;
    }
}

/**
 * An immutable sequence of {Char.Char}s.
 * All {Char.Char}s must be in the BMP Unicode plane.
 *
 * See {Char.Char} for implementation notes.
 */
class ImmutableBMPString extends ImmutableUString {
    /**
     * @param {!number} start
     * @param {!number} end
     * @return {!MutableUString}
     * @override
     */
    substring(start, end) {
        return new MutableUString(
            this.chars.slice(start, end),
            this.toString().substring(start, end)
        );
    }
}

/**
 * An mutable sequence of {Char.Char}s.
 */
class MutableUString extends UString {
    /**
     * @return {false}
     */
    isImmutable() {
        return false;
    }

    setCharAt(i, char) {
        this.checkIndexLtLength(i);
        if (!Char.eq(char, this.chars[i])) {
            this.chars[i] = char;
            this._nativeString = null;
            this._cachedHashCode = null;
        }
    }
}

/**
 * @param {!string} nativeString
 * @return {!Char.Char[]}
 */
function nativeStringToChars(nativeString) {
    // Array.from splits on codepoints (as per the String iterator spec).
    return Array.from(nativeString, Char.charFromNativeString);
}

/**
 * @param {!string} nativeString
 * @return {!ImmutableUString}
 */
export const makeInternedImmutable = internedMake(nativeString => makeImmutable(nativeString));

/**
 * @param {!string} nativeString
 * @return {!ImmutableUString}
 */
export function makeImmutable(nativeString) {
    return makeImmutableFromCharsAndNativeString(
        nativeStringToChars(nativeString),
        nativeString
    );
}

/**
 * @param {!Char.Char[]} chars
 * @param {!string} nativeString
 * @return {!ImmutableUString}
 */
function makeImmutableFromCharsAndNativeString(chars, nativeString) {
    return chars.length === nativeString.length
        ? new ImmutableBMPString(chars, nativeString)
        : new ImmutableUString(chars, nativeString);
}

/**
 * @param {!string} nativeString
 * @return {!MutableUString}
 */
export function makeMutable(nativeString) {
    return new MutableUString(
        nativeStringToChars(nativeString),
        nativeString
    );
}

/**
 * @param {!Char.Char[]} chars
 * @return {!MutableUString}
 */
export function makeMutableFromChars(chars) {
    return new MutableUString(chars, null);
}

/**
 * @param {!UString} str
 * @return {!MutableUString}
 */
export function copyAsMutable(str) {
    return new MutableUString(str.chars, str._nativeString);
}

export function makeMutableFromCharsVarArgs(...chars) {
    return makeMutableFromChars(chars);
}

/**
 * @param {!UString} v
 * @return {!ImmutableUString}
 */
export function stringToImmutableString(v) {
    return v instanceof ImmutableUString
        ? v
        : makeImmutableFromCharsAndNativeString(v.chars, v.toString());
}

/**
 * @param {!Pair.Pair} charsList a list of chars
 * @return {!MutableUString}
 */
export function listToString(charsList) {
    const chars = Pair.listToArray(charsList);
    return new MutableUString(chars, chars.join(''));
}

/**
 * @param {*} v
 * @return {!boolean}
 */
export function check(v) {
    return v instanceof UString;
}

/**
 * @param {!UString} a
 * @param {!UString} b
 * @return {!boolean}
 */
export function eq(a, b) {
    return a.toString() === b.toString();
}

/**
 * @param {!UString} a
 * @param {!UString} b
 * @return {!boolean}
 */
export function lt(a, b) {
    if (a.length < b.length) {
        return true;
    }
    const n = a.length;
    for (let i = 0; i < n; i++) {
        if (!Char.eq(a.chars[i], b.chars[i])) {
            return a.chars[i] < b.chars[i];
        }
    }
    return false;
}

/**
 * @param {!UString} a
 * @param {!UString} b
 * @return {!boolean}
 */
export function gt(a, b) {
    if (a.length > b.length) {
        return true;
    }
    const n = a.length;
    for (let i = 0; i < n; i++) {
        if (!Char.eq(a.chars[i], b.chars[i])) {
            return a.chars[i] > b.chars[i];
        }
    }
    return false;
}

/**
 * @param {!UString} a
 * @param {!UString} b
 * @return {!boolean}
 */
export function lte(a, b) {
    return !gt(a, b);
}

/**
 * @param {!UString} a
 * @param {!UString} b
 * @return {!boolean}
 */
export function gte(a, b) {
    return !lt(a, b);
}

/**
 * @param {!number} k
 * @param {!Char.Char} c
 */
export function repeatChar(k, c) {
    const chars = new Array(k);
    chars.fill(c.toString());
    return new MutableUString(chars, null);
}

const utf8Encoder = new TextEncoder();

/**
 * @param {!(UString|String)} str
 * @return {!Uint8Array}
 */
export function toBytesUtf8(str) {
    return utf8Encoder.encode(str.toString());
}

/**
 * @param {!Uint8Array} bytes
 * @return {!MutableUString}
 */
export function fromBytesUtf8(bytes) {
    return makeMutable(Bytes.toString(bytes));
}

/**
 * @param {!UString[]} strs
 * @return {!MutableUString}
 */
export function stringAppend(...strs) {
    return makeMutableFromChars([].concat(...strs.map(s => s.chars)));
}

const TRUE_STRING = makeInternedImmutable($.toString(true));
const FALSE_STRING = makeInternedImmutable($.toString(false));
const VOID_STRING = makeInternedImmutable($.toString(null));

/**
 * @param {any} v
 * @return {!UString}
 */
export function toUString(v) {
    if (check(v)) return v;
    if (v === true) return TRUE_STRING;
    if (v === false) return FALSE_STRING;
    if (v === undefined || v === null) return VOID_STRING;
    if (Bytes.check(v)) return makeImmutable(Bytes.toString(v));
    if ('toUString' in v) return v.toUString();
    return makeImmutable(v.toString());
}
