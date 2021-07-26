import { hashString } from './raw_hashing.js';
import * as Sym from './symbol.js';

/**
 * Base class for various compound data types
 * such as structs, pairs, values, ...
 *
 * Subclasses must override the following methods:
 * * displayNativeString
 * * isImmutable
 * * equals
 *
 * @abstract
 */
export class Primitive {
    /** @abstract isImmutable(): boolean; */
    /** @abstract equals(*): boolean; */

    /**
   * @return {!number} a 32-bit integer
   */
    hashForEqual() {
        return hashString(this.toString());
    }
}

export function check(v) {
    return v instanceof Primitive;
}

// Safely get the name of a native JS Symbol
export function safeToString(v) {
    if (Sym.check(v)) {
        return Sym.getValue(v);
    }
    return v.toString();
}
