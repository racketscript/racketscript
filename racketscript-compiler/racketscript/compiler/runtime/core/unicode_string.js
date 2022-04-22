import { Primitive } from './primitive.js';
import * as Bytes from './bytes.js';
import * as Char from './char.js';
import { MiniNativeOutputStringPort } from './mini_native_output_string_port.js';
import { internedMake } from './lib.js';
import { hashIntArray } from './raw_hashing.js';

/**
 * A sequence of {Char.Char}s.
 *
 * See {Char.Char} for implementation notes.
 *
 * @abstract
 */
export class UString extends Primitive /* implements Printable */ {
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
     * @param {!number} i
     */
    isValidIndex(i) { return i < this.length; }

    /**
     * @param {!number} i
     */
    isEmpty() { return this.length === 0; }

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
     * @param {!Ports.NativeStringOutputPort} out
     */
    displayNativeString(out) {
        out.consume(this.toString());
    }

    /**
     * @param {!Ports.UStringOutputPort} out
     */
    displayUString(out) {
        out.consume(this);
    }

    /**
     * Writes a string representation that can be read by Racket's `read` to the given port.
     *
     * @param {!Ports.NativeStringOutputPort} out
     */
    writeNativeString(out) {
        // The JSON representation happens to be readable by Racket's `read`.
        out.consume('"');
        for (const char of this.chars) {
            const c = char.codepoint;
            switch (c) {
            // Reference implementation:
            // https://github.com/racket/racket/blob/cbfcc904ab621a338627e77d8f5a34f930ead0ab/racket/src/racket/src/print.c#L3690
            case 7:
                out.consume('\\a');
                break;
            case 8:
                out.consume('\\b');
                break;
            case 9:
                out.consume('\\t');
                break;
            case 11:
                out.consume('\\v');
                break;
            case 12:
                out.consume('\\f');
                break;
            case 10:
                out.consume('\\n');
                break;
            case 13:
                out.consume('\\r');
                break;
            case 27:
                out.consume('\\e');
                break;
            case 34:
                out.consume('\\"');
                break;
            case 92:
                out.consume('\\\\');
                break;
            default:
                if (Char.isGraphic(char) || Char.isBlank(char)) {
                    out.consume(char.toString());
                } else {
                    out.consume(c > 0xFFFF
                        ? `\\U${c.toString(16).toUpperCase().padStart(8, '0')}`
                        : `\\u${c.toString(16).toUpperCase().padStart(4, '0')}`);
                }
            }
        }
        out.consume('"');
    }

    /**
     * Writes a UString representation that can be read by Racket's `read` to the given port.
     *
     * @param {!Ports.UStringOutputPort} out
     */
    writeUString(out) {
        const stringOut = new MiniNativeOutputStringPort();
        this.writeNativeString(stringOut);
        out.consume(makeMutable(stringOut.getOutputString()));
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    printNativeString(out) {
        this.writeNativeString(out);
    }

    /**
     * @param {!Ports.UStringOutputPort} out
     */
    printUString(out) {
        this.writeUString(out);
    }

    /**
     * Whether the string can be parsed as an integer as defined by
     * Racket's string->number.
     *
     * @param {!number} radix an integer in [2, 16] range.
     * @return {!boolean}
     */
    isValidInteger(radix) {
        const startFrom = this.chars[0].codepoint === /* '-' */ 45 ? 1 : 0;
        if (radix > 10) {
            const maxLowercase = /* 'a' - 11 */ 86 + radix;
            const maxUppercase = maxLowercase - 32;
            for (let i = startFrom; i < this.chars.length; ++i) {
                const cp = this.chars[i].codepoint;
                if (cp < /* '0' */ 48 || cp > maxLowercase ||
                    cp > maxUppercase && cp < /* 'a' */ 97 ||
                    cp > /* '9' */ 57 && cp < /* 'A' */ 65) return false;
            }
        } else {
            const max = /* '0' - 1 */ 47 + radix;
            for (let i = startFrom; i < this.chars.length; ++i) {
                const cp = this.chars[i].codepoint;
                if (cp < /* '0' */ 48 || cp > max) return false;
            }
        }
        return true;
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

    /**
     * Writes a string representation that can be read by Racket's `read` to the given port.
     *
     * @param {!Ports.NativeStringOutputPort} out
     */
    writeNativeString(out) {
        // The JSON representation for BMP strings happens to be readable by Racket's `read`.
        out.consume(JSON.stringify(this.toString()));
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
export const make = makeInternedImmutable;

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
    const n = Math.min(a.length, b.length);
    for (let i = 0; i < n; i++) {
        if (!Char.eq(a.chars[i], b.chars[i])) {
            return a.chars[i] < b.chars[i];
        }
    }
    return (a.length < b.length);
}

/**
 * @param {!UString} a
 * @param {!UString} b
 * @return {!boolean}
 */
export function gt(a, b) {
    const n = Math.min(a.length, b.length);
    for (let i = 0; i < n; i++) {
        if (!Char.eq(a.chars[i], b.chars[i])) {
            return a.chars[i] > b.chars[i];
        }
    }
    return (a.length > b.length);
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
 * @param {!(UString|String)} str
 * @return {!Char.Char[]} chars
 */
export function toArray(str) {
    return str.chars;
}

/**
 * @param {!Uint8Array} bytes
 * @return {!MutableUString}
 */
export function fromBytesUtf8(bytes) {
    return makeMutable(Bytes.toString(bytes));
}

/**
 * @param {!Uint8Array} bytes
 * @return {!MutableUString}
 */
export function fromBytesLatin1(bytes) {
    return makeMutable(Bytes.toString(bytes));
}

/**
 * @param {!UString[]} strs
 * @return {!MutableUString}
 */
export function stringAppend(...strs) {
    return makeMutableFromChars([].concat(...strs.map(s => s.chars)));
}

// The Char Printable *UString methods are defined here,
// because Char cannot depend on UString.

/**
 * @param {!Ports.UStringOutputPort} out
 */
Char.Char.prototype.displayUString = function (out) {
    out.consume(new MutableUString([this], this._nativeString));
};

/**
 * @param {!Ports.UStringOutputPort} out
 */
Char.Char.prototype.writeUString = function (out) {
    const stringOut = new MiniNativeOutputStringPort();
    this.writeNativeString(stringOut);
    out.consume(makeMutable(stringOut.getOutputString()));
};
