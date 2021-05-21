import { Primitive } from './primitive.js';

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
export class Char extends Primitive /* implements Printable */ {
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
            if (isGraphic(this)) {
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

    // displayUstring is defined in unicode_string.js to avoid a circular dependency.

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

// NOTE:
//   "The Racket documentation only promises `eq?` for characters with
//    scalar values in the range 0 to 255, but Chez Scheme characters
//    are always `eq?` when they are `eqv?`."
// see: https://groups.google.com/g/racket-users/c/LFFV-xNq1SU/m/s6eoC35qAgAJ
//      https://docs.racket-lang.org/reference/characters.html
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
    }
    return 6;
}

/**
  * @param {!string} str
  * @return {!boolean}
 */
function isSingleCodePoint(str) {
    return str.length === 1 || (str.length === 2 && str.codePointAt(0) > 0xFFFF);
}

/**
* @param {!Char} c
* @return {!Char}
*/
export function upcase(c) {
    const upper = c.toString().toUpperCase();
    if (!isSingleCodePoint(upper)) return c;
    return charFromNativeString(upper);
}

/**
* @param {!Char} c
* @return {!Char}
*/
export function downcase(c) {
    const lower = c.toString().toLowerCase();
    if (!isSingleCodePoint(lower)) return c;
    return charFromNativeString(lower);
}

// Unicode property testing regexps.
// TODO (Deprecated): We use `new RegExp` because traceur crashes on `/u` RegExp literals.
const IS_ALPHABETIC = new RegExp('\\p{Alphabetic}', 'u');
const IS_LOWER_CASE = new RegExp('\\p{Lowercase}', 'u');
const IS_UPPER_CASE = new RegExp('\\p{Uppercase}', 'u');
const IS_TITLE_CASE = new RegExp('\\p{Lt}', 'u');
// TODO: IS_NUMERIC should use \\p{Numeric_Value},
// but js regexp errs with "Invalid property name".
// Fix this when js regexps become more compliant with unicode standard
// see also:
// - https://stackoverflow.com/questions/5562835/split-and-replace-unicode-words-in-javascript-with-regex
// - https://github.com/vishesh/racketscript/issues/176
const IS_NUMERIC = new RegExp('[\\p{Nd}\\p{Nl}\\p{No}]', 'u');
const IS_SYMBOLIC = new RegExp('\\p{S}', 'u');
const IS_PUNCTUATION = new RegExp('\\p{P}', 'u');
const IS_GRAPHIC = new RegExp('[\\p{L}\\p{N}\\p{M}\\p{S}\\p{P}\\p{Alphabetic}]', 'u');
const IS_WHITESPACE = new RegExp('\\p{White_Space}', 'u');
const IS_BLANK = new RegExp('[\\p{Zs}\\t]', 'u');

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isAlphabetic(c) {
    return IS_ALPHABETIC.test(c.toString());
}

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isLowerCase(c) {
    return IS_LOWER_CASE.test(c.toString());
}

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isUpperCase(c) {
    return IS_UPPER_CASE.test(c.toString());
}

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isTitleCase(c) {
    return IS_TITLE_CASE.test(c.toString());
}

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isNumeric(c) {
    return IS_NUMERIC.test(c.toString());
}

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isSymbolic(c) {
    return IS_SYMBOLIC.test(c.toString());
}

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isPunctuation(c) {
    return IS_PUNCTUATION.test(c.toString());
}

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isGraphic(c) {
    return IS_GRAPHIC.test(c.toString());
}

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isWhitespace(c) {
    return IS_WHITESPACE.test(c.toString());
}

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isBlank(c) {
    return IS_BLANK.test(c.toString());
}

/**
* @param {!Char} c
* @return {!boolean}
*/
export function isIsoControl(c) {
    const cp = c.codepoint;
    return (cp >= 0 && cp <= 0x1F) || (cp >= 0x7F && cp <= 0x9F);
}
