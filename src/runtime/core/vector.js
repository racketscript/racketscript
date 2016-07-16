import {default as Primitive} from "./primitive.js";
import {RacketCoreError} from "./error.js";
import * as rutils from "./utils.js";

class Vector extends Primitive {
    constructor(items, mutable) {
	super();
	this.mutable = mutable;
	this.items = items;
    }

    toString() {
	var items = "";
	for (var i = 0; i < this.items.length; i++) {
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

    isMutable() {
	return this.mutable;
    }

    ref(n) {
	if (n < 0 || n > this.items.length) {
	    throw new RacketCoreError("vector-ref", "index out of range");
	}

	return this.items[n];
    }

    set(n, v) {
	if (n < 0 || n > this.items.length) {
	    throw new ("vector-set", "index out of range");
	} else if (!this.mutable) {
	    throw new RacketCoreError("vector-set", "immutable vector");
	}
	this.items[n] = v;
    }

    length() {
	return this.size;
    }

    equals(v) {
	var items1 = this.items;
	var items2 = v.items;

	if (items1.length != items2.length) {
	    return false;
	}

	for (var i = 0; i < items1.length; i++) {
	    if (!rutils.isEqual(items1[i], items2[i])) {
		return false;
	    }
	}
	
	return true;
    }
}

export
function make(items, mutable) {
    return new Vector(items, mutable);
}


export
function makeInit(size, init) {
    var r = new Array(size);
    return r.fill(init);
}

export
function check(v1) {
    return (v1 instanceof Vector);
}
