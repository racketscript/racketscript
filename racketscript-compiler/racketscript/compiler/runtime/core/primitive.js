import * as $ from "./lib.js";

/**
   Base class for various compound data types
   such as structs, pairs, values, ...
*/
export class Primitive {
    constructor() {
	// Abstract base class
	// if (new.target === Primitive) {
	//     throw new TypeError("Cannot construct Abstract instances directly");
	// }
	this.__cachedHashCode = undefined;
    }

    toString() {
	throw $.racketCoreError("Not Implemented");
    }

    toRawString() {
	return this.toString();
    }

    mutable() {
	throw $.racketCoreError("Not Implemented");
    }

    equals(v) {
	throw $.racketCoreError("Not Implemented {0}", v);
    }

    eqv(v) {
	return this === v;
    }

    valueOf() {
	return this;
    }

    hashEqual() {
	return $.hashString(this.toRawString());
    }

    hashCode() {
	if (this.__cachedHashCode === undefined) {
	    this.__cachedHashCode = this.hashEqual();
	}
	return this.__cachedHashCode;
    }
}

export function check(v) {
    return (v instanceof Primitive);
}
