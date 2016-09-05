import {Primitive} from "./primitive.js";
import * as $ from "./lib.js";

class Box extends Primitive {
    constructor(v) {
	super();
	this.value = v;
    }

    toString() {
	return this.value;
    }

    toRawString() {
	return this.toString();
    }

    equals(v) {
	return $.isEqual(v.value, this.value);
    }

    set(v) {
	this.value = v;
    }

    get() {
	return this.value;
    }
}


export function make(v) {
    return new Box(v);
}

export function check(v) {
    return (v instanceof Box)
}
