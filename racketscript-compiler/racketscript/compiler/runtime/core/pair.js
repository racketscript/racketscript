import {Primitive} from "./primitive.js";
import {isEqual} from "./equality.js";
import {displayNativeString, writeNativeString} from './print_native_string.js';
import * as Ports from './ports.js';

/** @singleton */
class EmptyPair extends Primitive {
    /**
    * @param {!Ports.NativeStringOutputPort} out
    */
    displayNativeString(out) {
        out.consume('()');
    }

    equals(v) {
        return this === v;
    }

    get length() {
        return 0;
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

export const EMPTY = new EmptyPair();

export class Pair extends Primitive {
    /** @private */
    constructor(hd, tl) {
        super();
        this.hd = hd;
        this.tl = tl;
        this._listLength = tl === EMPTY ?
            1 : (check(tl) ? tl.length + 1 : 2);
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
                itemFn(out, rest)
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

/**
 * @param {*} v
 * @return {boolean} true iff v is the empty list.
 */
export function isEmpty(v) {
    return EMPTY === v;
}

export function make(hd, tl) {
    return new Pair(hd, tl);
}

export function makeList(...items) {
    return items.reduceRight((result, item) => {
        return make(item, result);
    }, EMPTY);
}

export function listToArray(lst) {
    let r = [];
    listForEach(lst, (x) => r.push(x));
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
        let result = fn(lst.hd);
        if (result !== false) {
            return result;
        }
        lst = lst.tl;
    }
    return false;
}

export function listMap(lst, fn) {
    let result = [];
    let mapper = (x) => result.push(result, fn(x));
    listForEach(lst, mapper);
    return listFromArray(result);
}
