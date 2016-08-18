import {RacketCoreError} from "./error.js";
import {hashString} from "../third-party/hash.js";

/**
   Base class for various compound data types
   such as structs, pairs, values, ...
*/
export default class Primitive {
    constructor() {
	// Abstract base class
	// if (new.target === Primitive) {
	//     throw new TypeError("Cannot construct Abstract instances directly");
	// }
    }

    toString() {
	throw new RacketCoreError("Not Implemented");
    }

    toRawString() {
	return this.toString();
    }

    mutable() {
	throw new RacketCoreError("Not Implemented");
    }

    equals(v) {
	throw new RacketCoreError("Not Implemented");
    }

    eqv(v) {
	return this === v;
    }

    valueOf() {
	return this;
    }

    hashEqual() {
	return hashString(this.toRawString());
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
