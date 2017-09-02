import {Primitive} from "./primitive.js";
import {internedMake} from "./lib.js";
import * as Ports from './ports.js';

class Symbol extends Primitive {
    constructor(v) {
        super();
        this.v = v;
        this._cachedHashCode = null;
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    displayNativeString(out) {
        out.consume(this.v);
    }

    equals(v) {
	// Symbols are interned by default, and two symbols
	// with same name can't be unequal.
	// Eg. (define x (gensym)) ;;=> 'g60
	//     (equal? x 'g60)     ;;=> #f
	return v === this;
    }

    /**
     * @return {!number}
     */
    hashForEqual() {
        if (this._cachedHashCode === null) {
            this._cachedHashCode = super.hashForEqual();
        }
        return this._cachedHashCode;
    }
}


export let make = internedMake(v => {
    return new Symbol(v);
});

export let makeUninterned = v => new Symbol(v);

export function check(v) {
    return (v instanceof Symbol)
}
