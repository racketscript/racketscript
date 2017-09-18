import { PrintablePrimitive } from './printable_primitive.js';
import { displayNativeString, writeNativeString } from './print_native_string.js';
import { isEqual } from './equality.js';

/** @singleton */
class Empty extends PrintablePrimitive {
    equals(v) {
        return this === v;
    }

    get length() {
        return 0;
    }

    /**
    * @param {!Ports.NativeStringOutputPort} out
    */
    displayNativeString(out) {
        out.consume('()');
    }

    /**
     * @return {false}
     */
    isImmutable() {
        // As per racket reference, this is always false for Pairs and Lists.
        // https://docs.racket-lang.org/reference/booleans.html#%28def._%28%28quote._~23~25kernel%29._immutable~3f%29%29
        return false;
    }
}

export const EMPTY = new Empty();

/**
 * @param {*} v
 * @return {boolean} true iff v is the empty list.
 */
export function isEmpty(v) {
    return v === EMPTY;
}

export class Pair extends PrintablePrimitive {
    /** @private */
    constructor(hd, tl) {
        super();
        this.hd = hd;
        this.tl = tl;
        this._listLength = isList(tl) ? tl.length + 1 : -1;
        this._cachedHashCode = null;
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     * @param {function(Ports.NativeStringOutputPort, *)} itemFn
     */
    writeToPort(out, itemFn) {
        out.consume('(');
        let rest = this;
        while (true) {
            if (check(rest)) {
                itemFn(out, rest.hd);
            } else {
                out.consume('. ');
                itemFn(out, rest);
                break;
            }
            rest = rest.tl;
            if (isEmpty(rest)) {
                break;
            } else {
                out.consume(' ');
            }
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

    equals(v) {
        if (!check(v) || this.length !== v.length) {
            return false;
        }

        let hd1 = this.hd;
        let tl1 = this.tl;
        let hd2 = v.hd;
        let tl2 = v.tl;

        while (true) {
            if (!isEqual(hd1, hd2)) {
                return false;
            }
            if (!check(tl1) || isEmpty(tl1)) {
                return isEqual(tl1, tl2);
            }
            hd1 = tl1.hd;
            tl1 = tl1.tl;
            hd2 = tl2.hd;
            tl2 = tl2.tl;
        }
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

    car() {
        return this.hd;
    }

    cdr() {
        return this.tl;
    }

    get length() {
        return this._listLength;
    }

    /**
     * @return {false}
     */
    isImmutable() {
        // As per racket reference, this is always false for Pairs and Lists.
        // https://docs.racket-lang.org/reference/booleans.html#%28def._%28%28quote._~23~25kernel%29._immutable~3f%29%29
        return false;
    }
}

/**
 * @param {*} v
 * @return {boolean} true iff v is a non-empty list or pair.
 */
export function check(v) {
    return typeof v === 'object' && v !== null && v.constructor === Pair;
}

export function make(hd, tl) {
    return new Pair(hd, tl);
}

export function makeList(...items) {
    return items.reduceRight((result, item) => make(item, result), EMPTY);
}

export function listToArray(lst) {
    const r = [];
    listForEach(lst, x => r.push(x));
    return r;
}

export function listFromArray(lst) {
    return makeList(...lst);
}

export function listForEach(lst, fn) {
    while (!isEmpty(lst)) {
        fn(lst.hd);
        lst = lst.tl;
    }
}

export function listFind(lst, fn) {
    while (!isEmpty(lst)) {
        const result = fn(lst.hd);
        if (result !== false) {
            return result;
        }
        lst = lst.tl;
    }
    return false;
}

export function listMap(lst, fn) {
    const result = [];
    const mapper = x => result.push(result, fn(x));
    listForEach(lst, mapper);
    return listFromArray(result);
}

/**
 * @param {*} v
 * @return {boolean} true iff v is a list.
 */
export function isList(v) {
    return v === EMPTY || (check(v) && v._listLength !== -1);
}
