import * as $ from "./lib.js";
import { hashString } from "./raw_hashing.js";

/**
 * Base class for various compound data types
 * such as structs, pairs, values, ...
 *
 * Subclasses must override the following methods:
 * * toString
 * * toRawString
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

    /**
     * @return {!String} The string representation matching Racket's `display`.
     */
    toString() {
        throw $.racketCoreError("Not Implemented");
    }

    /**
     * @return {!String} The deserializble string representation matching Racket's `write`.
     */
    toRawString() {
        return this.toString();
    }

    isImmutable() {
        throw $.racketCoreError("Not Implemented");;
    }

    /**
     * @param {*} v
     * @return {!boolean}
     */
    equals(v) {
        throw $.racketCoreError("Not Implemented {0}", v);
    }

    /**
     * @return {!number} a 32-bit integer
     */
    hashForEqual() {
        return hashString(this.toRawString());
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
