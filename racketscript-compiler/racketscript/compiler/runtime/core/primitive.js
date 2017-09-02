import {hashString} from "./raw_hashing.js";

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
    constructor() {
        // Abstract base class
        // if (new.target === Primitive) {
        //     throw new TypeError("Cannot construct Abstract instances directly");
        // }
    }

    isImmutable() {
        throw new Error('Not Implemented');
    }

    /**
     * @param {*} v
     * @return {!boolean}
     */
    equals(v) {
        throw new Error('Not Implemented: equals');
    }

    /**
     * @return {!number} a 32-bit integer
     */
    hashForEqual() {
        return hashString(this.toString());
    }

    /**
     * This should either return {this} or a value which has
     * a partial order in JavaScript.
     *
     * @return {!(Primitive|number|string|boolean)}
     */
    valueOf() {
        return this;
    }
}

export function check(v) {
    return (v instanceof Primitive);
}
