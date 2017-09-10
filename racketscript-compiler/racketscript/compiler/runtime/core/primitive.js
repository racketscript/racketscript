import { hashString } from './raw_hashing.js';

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
    return (v instanceof Primitive);
}
