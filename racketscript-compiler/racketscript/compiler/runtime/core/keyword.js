import {Primitive} from "./primitive.js";
import {internedMake} from "./lib.js";
import * as Ports from './ports.js';

class Keyword extends Primitive {
    constructor(v) {
	super();
	this.v = v;
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    displayNativeString(out) {
        out.consume('#:');
        out.consume(this.v);
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
