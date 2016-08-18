import {default as Primitive} from "./primitive.js";
import {hashString} from "../third-party/hash.js";
import {internedMake} from "./utils.js";

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
	return check(v) && this.v === v.v;
    }
}


export let make = internedMake(v => {
    return new Keyword(v);
});

export function check(v) {
    return (v instanceof Keyword)
}
