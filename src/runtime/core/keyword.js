import {default as Primitive} from "./primitive.js";

class Keyword extends Primitive {
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
    return new Keyword(v);
}

export
function check(v) {
    return (v instanceof Keyword)
}
