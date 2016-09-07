import {Primitive} from "./primitive.js";
import {internedMake} from "./lib.js";

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
	return check(v) && this.v === v.v;
    }
}


export let make = internedMake(v => {
    return new Symbol(v);
});

export function check(v) {
    return (v instanceof Symbol)
}
