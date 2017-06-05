import {Primitive} from "./primitive.js";
import * as $ from "./lib.js";

class Vector extends Primitive {
    constructor(items, mutable) {
	super();
	this.mutable = mutable;
	this.items = items;
    }

    toString() {
	let items = "";
	for (let i = 0; i < this.items.length; i++) {
	    items += this.items[i].toString();
	    if (i != this.items.length - 1) {
		items += " ";
	    }
	}
	return "#(" + items + ")";
    }

    toRawString() {
	return "'" + this.toString();
    }

    mutable() {
	return this.mutable;
    }

    ref(n) {
	if (n < 0 || n > this.items.length) {
	    throw $.racketCoreError("vector-ref", "index out of range");
	}

	return this.items[n];
    }

    set(n, v) {
	if (n < 0 || n > this.items.length) {
	    throw  $.racketCoreError("vector-set", "index out of range");
	} else if (!this.mutable) {
	    throw $.racketCoreError("vector-set", "immutable vector");
	}
	this.items[n] = v;
    }

    length() {
	return this.items.length;
    }

    equals(v) {
	if (!check(v)) {
	    return false;
	}

	let items1 = this.items;
	let items2 = v.items;

	if (items1.length != items2.length) {
	    return false;
	}

	for (let i = 0; i < items1.length; i++) {
	    if (!$.isEqual(items1[i], items2[i])) {
		return false;
	    }
	}
	
	return true;
    }
}

export function make(items, mutable) {
    return new Vector(items, mutable);
}

export function copy(vec, mutable) {
    return new Vector(vec.items, mutable);
}


export function makeInit(size, init) {
    let r = new Array(size);
    r.fill(init);
    return new Vector(r, true);
}

export function check(v1) {
    return (v1 instanceof Vector);
}
