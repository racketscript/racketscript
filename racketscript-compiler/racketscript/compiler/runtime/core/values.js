import {Primitive} from "./primitive.js";
import {racketCoreError} from "./errors.js";

class Values extends Primitive {
    constructor(vals) {
	super();
	this.v = vals;
    }

    toString() {
	throw racketCoreError("Not Implemented");
    }

    getAt(i) {
	return this.v[i];
    }

    getAll() {
	return this.v;
    }
}


export function make(vals) {
    return new Values(vals);
}

export function check(v) {
    return (v instanceof Values);
}
