import {Primitive} from "./primitive.js";
import * as Ports from './ports.js';

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
     * @return {!number} a non-negative integer.
     * @override
     */
    hashForEqual() {
        return this.codepoint;
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    displayNativeString(out) {
        out.consume(this.toString());
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    writeNativeString(out) {
        const c = this.codepoint;
        switch (c) {
            // Reference implementation:
            // https://github.com/racket/racket/blob/cbfcc904ab621a338627e77d8f5a34f930ead0ab/racket/src/racket/src/print.c#L4089
            case 0:
                out.consume('#\\nul');
                break;
            case 8:
                out.consume('#\\backspace');
                break;
            case 9:
                out.consume('#\\tab');
                break;
            case 10:
                out.consume('#\\newline');
                break;
            case 11:
                out.consume('#\\vtab');
                break;
            case 12:
                out.consume('#\\page');
                break;
            case 13:
                out.consume('#\\return');
                break;
            case 32:
                out.consume('#\\space');
                break;
            case 127:
                out.consume('#\\rubout');
                break;
            default:
                if (isGraphicCodepoint(c)) {
                    out.consume(`#\\${this.toString()}`);
                } else {
                    out.consume(c > 0xFFFF
                        ? `#\\U${c.toString(16).toUpperCase().padStart(8, '0')}`
                        : `#\\u${c.toString(16).toUpperCase().padStart(4, '0')}`);
                }
        }
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    printNativeString(out) {
        this.writeNativeString(out);
    }

    // displayUstring is defined in unicode_string.js to avoid circular dependency.

    /**
     * @param {!Ports.UStringOutputPort} out
     */
    printUString(out) {
        this.writeUString(out);
    }
}

const INTERN_CACHE_SIZE = 256;

/**
 * @type {!Array<Char|undefined>} A cache for chars with small codepoints.
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
    return typeof char === 'object' && char !== null &&
        char.constructor === Char;
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

// The Unicode property testing methods below were generated with:
// https://gist.github.com/glebm/2749c75b4fc4fed4dc5911925bb8f8b9

/**
 * WARNING: This currently always returns `false` for codepoints >= 2048,
 * even if they are graphic.
 *
 * A graphic codepoint in Racket is in one the following General Categories:
 *
 *   Letter, Mark, Number, Punctuation, Symbol.
 *
 * @param {!number} c a Unicode codepoint
 * @return {!boolean} Whether the codepoint is graphic
 * @api private
 */
export function isGraphicCodepoint(c) {
    // This is just a quick hack to have sensible output in European locales.
    // TODO: If we implement Unicode property testing, use it here instead.
    return (
        c > 32 && c < 127 ||
        (c > 160 && c < 896 && !(c === 173 || c > 887 && c < 890)) ||
        (c > 899 && c < 1480 && !(c === 1328 || c > 1366 && c < 1369 ||
            c === 1376 || c === 1416 || c > 1418 && c < 1421 || c === 1424 ||
            (c > 906 && c < 910 && c !== 908) || c === 930)) ||
        c > 1487 && c < 1515 || c > 1519 && c < 1525 ||
        (c > 1541 && c < 1970 && !(c > 1563 && c < 1566 || c === 1757 ||
            c > 1805 && c < 1808 || c > 1866 && c < 1869)) ||
        c > 1983 && c < 2043);
}

/**
 * @param {!number} c a Unicode codepoint
 * @return {!boolean} Whether the codepoint's Unicode general category
 * is "Zs" ("Space_Separator") or if char is #\tab.
 * @api private
 */
export function isBlankCodepoint(c) {
    return (
        c === 9 || c === 32 || c === 160 || c === 5760 ||
        c > 8191 && c < 8203 || c === 8239 || c === 8287 || c === 12288);
}

/**
 * @param {!number} c a Unicode codepoint
 * @return {!boolean} Whether the codepoint has the Unicode "White_Space" property.
 * @api private
 */
export function isWhitespaceCodepoint(c) {
    return (
        c > 8 && c < 14 || c === 32 || c === 133 || c === 160 || c === 5760 ||
        c > 8191 && c < 8203 || c > 8231 && c < 8234 || c === 8239 ||
        c === 8287 || c === 12288);

}
