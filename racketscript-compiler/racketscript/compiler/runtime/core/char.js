import {Primitive} from "./primitive.js";

/**
 * A single Unicode character.
 * Value type semantics, immutable and final.
 *
 * The parameter type signatures in this class and file are enforced
 * by RacketScript when called from RacketScript.
 *
 * No checks are performed here, allowing us to use it internally
 * without paying the cost of runtime type checking.
 *
 * @property {!number} codepoint
 * @final
 */
export class Char extends Primitive {
    /**
     * @param {!number} codepoint a non-negative integer.
     * @param {(!string|null)} nativeString
     * @private
     */
    constructor(codepoint, nativeString) {
        super();
        this.codepoint = codepoint;
        this._nativeString = nativeString;
    }

    /**
     * @param {*} v
     * @return {!boolean}
     */
    equals(v) {
        return check(v) && eq(this, v);
    }

    /**
     * @return {true}
     */
    isImmutable() {
        return true;
    }

    /**
     * @return {!number} this.codepoint.
     * @override
     */
    valueOf() {
        return this.codepoint;
    }

    /**
     * @return {!String}
     * @override
     */
    toString() {
        if (this._nativeString === null) {
            this._nativeString = String.fromCodePoint(this.codepoint);
        }
        return this._nativeString;
    }

    /**
     * @return {!String}
     * @override
     */
    toRawString() {
        return `#\\u${this.codepoint}`;
    }

    /**
     * @return {!number} a non-negative integer.
     * @override
     */
    hashForEqual() {
        return this.codepoint;
    }
}

const INTERN_CACHE_SIZE = 256;

/**
 * @type {!Array<(!Char|undefined)>} A cache for chars with small codepoints.
 */
const internedCache = new Array(INTERN_CACHE_SIZE);

/**
 * @param {!number} codepoint a non-negative integer.
 * @return {!Char}
 */
export function charFromCodepoint(codepoint) {
    if (codepoint < INTERN_CACHE_SIZE) {
        if (internedCache[codepoint] === undefined) {
            internedCache[codepoint] = new Char(codepoint, null);
        }
        return internedCache[codepoint];
    }
    return new Char(codepoint, null);
}

/**
 * @param {!string} s A native string exactly one Unicode codepoint long.
 */
export function charFromNativeString(s) {
    const codepoint = s.codePointAt(0);
    if (codepoint < INTERN_CACHE_SIZE) {
        if (internedCache[codepoint] === undefined) {
            internedCache[codepoint] = new Char(codepoint, s);
        }
        return internedCache[codepoint];
    }
    return new Char(codepoint, s);
}

/**
 * @param {*} char
 * @return {!boolean}
 */
export function check(char) {
    // Because Char is final, we can compare the constructor directly
    // instead of using the much slower `instanceof` operator.
    return char.constructor === Char;
}

/**
 * @param {!Char} a
 * @param {!Char} b
 * @return {!boolean}
 */
export function eq(a, b) {
    return a.codepoint === b.codepoint;
}

/**
 * @param {!Char} c
 * @return {!number} the given character's Unicode codepoint.
 */
export function charToInteger(c) {
    return c.codepoint;
}

/**
 * @param {!number} k a non-negative integer representing a Unicode codepoint.
 * @return {!Char}
 */
export function integerToChar(k) {
    return charFromCodepoint(k);
}

/**
 * @param {!Char} c
 * @return {!number} an integer between 1 and 6 (inclusive).
 */
export function charUtf8Length(c) {
    const cp = c.codepoint;
    if (cp < 0x80) {
        return 1;
    } else if (cp < 0x800) {
        return 2;
    } else if (cp < 0x10000) {
        return 3;
    } else if (cp < 0x200000) {
        return 4;
    } else if (cp < 0x4000000) {
        return 5;
    } else {
        return 6;
    }
}
