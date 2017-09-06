import {PrintablePrimitive} from './printable_primitive.js';
import {isEqual, isEqv, isEq} from "./equality.js";
import {hashForEqual, hashForEqv, hashForEq} from "./hashing.js";
import {displayNativeString, writeNativeString} from './print_native_string.js';
import {racketCoreError} from './errors.js';
import {hamt} from './lib.js';
import * as Pair from "./pair.js";
import * as Ports from './ports.js';

const hashConfigs = {
    eq: {
        hash: hashForEq,
        keyEq: isEq
    },
    eqv: {
        hash: hashForEqv,
        keyEq: isEqv
    },
    equal: {
        hash: hashForEqual,
        keyEq: isEqual
    }
}

class Hash extends PrintablePrimitive {
    constructor(hash, type, mutable) {
	super();
	this._h = hash;
	this._mutable = mutable;
	this._type = type;
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     * @param {function(Ports.NativeStringOutputPort, *)} itemFn
     */
    writeToPort(out, itemFn) {
        out.consume('#hash');
        if (this._type === 'eq' || this._type === 'eqv') {
            out.consume(this._type);
        }
        out.consume('(');
        const n = this._h.size;
        let i = 0;
        for (let [k, v] of this._h) {
            out.consume('(');
            itemFn(out, k);
            out.consume(' . ');
            itemFn(out, v);
            out.consume(')');
            if (++i !== n) out.consume(' ');
        }
        out.consume(')');
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    displayNativeString(out) {
        this.writeToPort(out, displayNativeString);
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    writeNativeString(out) {
        this.writeToPort(out, writeNativeString);
    }

    isImmutable() {
        return !this._mutable;
    }

    ref(k, fail) {
        let result = this._h.get(k);
        if (result !== undefined) {
            return result;
        } else if (fail !== undefined) {
            return fail;
        } else {
            throw racketCoreError(
                'hash-ref: no value found for key\n  key:', k);
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
	    if (vv === undefined || !isEqual(val, vv)) {
		return false;
	    }
	}

	return true;
    }
}

function make(items, type, mutable) {
    let h = items.reduce((acc, item) => {
	let [k, v] = item;
	return acc.set(k, v);
    }, hamt.make(hashConfigs[type]));
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

function makeFromAssocs(assocs, type, mutable) {
    let items = []
    Pair.listForEach(assocs, (item) => {
	items.push([item.hd, item.tl]);
    });
    return make(items, type, mutable);
}

export function makeEqFromAssocs(assocs, mutable) {
    return makeFromAssocs(assocs, 'eq', mutable);
}

export function makeEqvFromAssocs(assocs, mutable) {
    return makeFromAssocs(assocs, 'eqv', mutable);
}

export function makeEqualFromAssocs(assocs, mutable) {
    return makeFromAssocs(assocs, 'equal', mutable);
}

export function map(hash, proc) {
    let result = Pair.EMPTY;
    hash._h.forEach((value, key) => {
        result = Pair.make(proc(key, value), result)
    });
    return result;
}

export function check(v1) {
    return (v1 instanceof Hash);
}

export function isEqualHash(h) {
    return check(h) && h._type === "equal";
}
export function isEqvHash(h) {
    return check(h) && h._type === "eqv";
}
export function isEqHash(h) {
    return check(h) && h._type === "eq";
}
