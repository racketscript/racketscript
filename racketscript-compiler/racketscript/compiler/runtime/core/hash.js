import * as $ from "./lib.js";
import * as Pair from "./pair.js";
import {Primitive} from "./primitive.js";

const hashConfigs = {
    eq: {
	hash: $.hashEq,
	keyEq: $.isEq
    },
    eqv: {
	hash: $.hashEqv,
	keyEq: $.isEqv
    },
    equal: {
	hash: $.hashEqual,
	keyEq: $.isEqual
    }
}

class Hash extends Primitive {
    constructor(hash, type, mutable) {
	super();
	this._h = hash;
	this._mutable = mutable;
	this._type = type;
    }

    toString() {
	let items = "";
	let i = 0;
	for (let [k, v] of this._h) {
	    items += "(";
	    items += $.toString(k);
	    items += " . ";
	    items += $.toString(v);
	    items += ")";
	    if (++i != this._h.size) {
		items += " ";
	    }
	}

	let typeSuffix = "";
	if (this._type === "eq" || this._type === "eqv") {
	    typeSuffix = this._type;
	}
	return "#hash" + typeSuffix + "(" + items + ")";
    }

    toRawString() {
	return "'" + this.toString();
    }

    mutable() {
	return this._mutable;
    }

    ref(k, fail) {
	let result = this._h.get(k);
	if (result !== undefined) {
	    return result;
	} else if (fail !== undefined) {
	    return fail;
	} else {
	    throw $.racketCoreError("hash-ref", "key not found");
	}
    }

    set(k, v) {
	let newH = this._h.set(k, v);

	if (this._mutable) {
	    this._h = newH;
	} else {
	    return new Hash(newH, this._type, false);
	}
    }

    size() {
	return this._h.size;
    }

    equals(v) {
	if (!check(v)) {
	    return false;
	}

	if (this._h.size !== v._h.size || this._type !== v._type || 
	    this._mutable !== v._mutable) {
	    return false;
	}

	for (let [key, val] of this._h) {
	    let vv = v._h.get(key);
	    if (vv === undefined || !$.isEqual(val, vv)) {
		return false;
	    }
	}

	return true;
    }

    type() {
	return this._type;
    }
}

export function make(items, type, mutable) {
    let h = items.reduce((acc, item) => {
	let [k, v] = item;
	return acc.set(k, v);
    }, $.hamt.make(hashConfigs[type]));
    return new Hash(h, type, mutable);
}

export function makeEq(items, mutable) {
    return make(items, "eq", mutable);
}

export function makeEqv(items, mutable) {
    return make(items, "eqv", mutable);
}

export function makeEqual(items, mutable) {
    return make(items, "equal", mutable);
}

export function makeFromAssocs(assocs, type, mutable) {
    let items = []
    Pair.listForEach(assocs, (item) => {
	items.push([item.hd, item.tl]);
    });
    return make(items, type, mutable);
}

export function map(hash, proc) {
    let result = Pair.Empty;
    hash._h.forEach((value, key) => {
	result = Pair.make(proc(key, value), result)
    });
    return result;
}

export function check(v1) {
    return (v1 instanceof Hash);
}
