import * as $ from './lib.js';
import * as Pair from './pair.js';
import * as Values from './values.js';
import { PrintablePrimitive } from './printable_primitive.js';
import { displayNativeString, writeNativeString } from './print_native_string.js';
import { isEqual, isEqv, isEq } from './equality.js';
import { hashForEqual, hashForEqv, hashForEq } from './hashing.js';

// throwing exns moved to kernel.rkt, so they may be caught via with-handlers

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
};

class Hash extends PrintablePrimitive {
    constructor(hash, type, mutable) {
        super();
        this._h = hash;
        this._mutable = mutable;
        this._type = type;
        this._iterator = undefined;
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
        for (const [k, v] of this._h) {
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

    toRawString() {
        return `'${this.toString()}`;
    }

    isImmutable() {
        return !this._mutable;
    }

    ref(k, fail) {
        const result = this._h.get(k);
        if (result !== undefined) {
            return result;
        }
        return typeof fail === 'function' ? fail() : fail;
    }

    hasKey(k) { return this._h.has(k); }

    // implements hash-ref-key
    // error reporting moved to kernel.rkt
    refKey(k, fail) {
        for (const key of this._h.keys()) {
            if (hashConfigs[this._type].keyEq(key, k)) {
                return key;
            }
        }
        return typeof fail === 'function' ? fail() : fail;
    }

    set(k, v) { return new Hash(this._h.set(k, v), this._type, false); }

    remove(k) { return new Hash(this._h.delete(k), this._type, false); }

    // mutating operations
    doset(k, v) {
        // TODO: if there already exists entry for key equal to `k`,
        // this will change key to (new) `k`,
        // but Racket retains the existing (old) key
        // see `refKey` (hash-ref-key) fn for more details
        this._h = this._h.set(k, v);
        // TODO: what to do when mutated while iterating?
        // for now, invalidate iterator
        this._iterator = undefined;
    }

    doremove(k) {
        this._h = this._h.delete(k);
        // TODO: what to do when mutated while iterating?
        // for now, invalidate iterator
        this._iterator = undefined;
    }

    size() { return this._h.size; }

    // iteration operations, eg hash-iterate-first/next
    iterateFirst() {
        if (this._h.size === 0) return false;
        // must save iterator since next() is stateful
        this._iterator = this._h.entries();
        return this._iterator.next();
    }

    iterateNext() {
        if (this._iterator === undefined) {
            return false;
        }
        const j = this._iterator.next();
        if (j.done) {
            this._iterator = undefined;
            return false;
        }
        return j;
    }

    iterateKey(i) {
        return i.value[0];
    }

    iterateValue(i) {
        return i.value[1];
    }

    iteratePair(i) {
        return Pair.make(i.value[0], i.value[1]);
    }

    iterateKeyValue(i) {
        return Values.make(i.value);
    }

    union(h) {
        let newH = this._h;
        for (const [key, val] of h) {
            newH = newH.set(key, val);
        }

        if (this._mutable) {
            this._h = newH;
        } else {
            return new Hash(newH, this._type, false);
        }
    }

    isSameType(h) { return this._type === h._type; }

    isKeysSubset(v) {
        if (this._h.size > v._h.size) {
            return false;
        }

        for (const key of this._h.keys()) {
            const vv = v._h.get(key);
            if (vv === undefined) {
                return false;
            }
        }

        return true;
    }

    equals(v) {
        if (!check(v)) {
            return false;
        }

        if (this._h.size !== v._h.size || this._type !== v._type ||
            this._mutable !== v._mutable) {
            return false;
        }

        for (const [key, val] of this._h) {
            const vv = v._h.get(key);
            if (vv === undefined || !isEqual(val, vv)) {
                return false;
            }
        }

        return true;
    }
}

function make(items, type, mutable) {
    const h = items.reduce((acc, item) => {
        const [k, v] = item;
        return acc.set(k, v);
    }, $.hamt.make(hashConfigs[type]));
    return new Hash(h, type, mutable);
}

export function makeEq(items, mutable) {
    return make(items, 'eq', mutable);
}

export function makeEqv(items, mutable) {
    return make(items, 'eqv', mutable);
}

export function makeEqual(items, mutable) {
    return make(items, 'equal', mutable);
}

function makeFromAssocs(assocs, type, mutable) {
    const items = [];
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

// TODO: allow ordering of hash traversal
export function map(hash, proc) {
    let result = Pair.EMPTY;
    hash._h.forEach((value, key) => {
        result = Pair.make(proc(key, value), result);
    });
    return result;
}

export function check(v1) {
    return (v1 instanceof Hash);
}

export function isEqualHash(h) {
    return check(h) && h._type === 'equal';
}
export function isEqvHash(h) {
    return check(h) && h._type === 'eqv';
}
export function isEqHash(h) {
    return check(h) && h._type === 'eq';
}

export function isWeakHash() {
    return false; // TODO: implement weak hashes
}
