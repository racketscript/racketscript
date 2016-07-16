import {default as Primitive} from "./primitive.js";

class Symbol extends Primitive {
    constructor(v) {
	super();
	this.v = v;
    }

    toString() {
	return this.v;
    }

    toRawString() {
	return "'" + this.v;
    }

    equals(v) {
	return this.v === v.v;
    }
}


export
function make(v) {
    return new Symbol(v);
}

export
function check(v) {
    return (v instanceof Symbol)
}
