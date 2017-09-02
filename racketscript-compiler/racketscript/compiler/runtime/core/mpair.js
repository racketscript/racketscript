import {Primitive} from "./primitive.js";
import {isEqual} from "./equality.js";
import {displayNativeString, writeNativeString} from './print_native_string.js';
import * as Pair from "./pair.js";
import * as Ports from './ports.js';

class MPair extends Primitive {
    /** @private */
    constructor(hd, tl) {
        super();
        this.hd = hd;
        this.tl = tl;
        this._listLength = Pair.isEmpty(tl) ? 1
            : (check(tl) ? tl.length + 1 : 2);
        this._cachedHashCode = null;
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     * @param {function(Ports.NativeStringOutputPort, *)} itemFn
     */
    dumpToPort(out, itemFn) {
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
            if (Pair.isEmpty(rest)) {
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
        this.dumpToPort(out, displayNativeString);
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    writeNativeString(out) {
        this.dumpToPort(out, writeNativeString);
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
            if (!check(tl1) || Pair.isEmpty(tl1)) {
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

    setCar(v) {
        if (this.hd !== v) {
            this.hd = v;
            this._cachedHashCode = null;
        }
    }

    setCdr(v) {
        if (this.tl !== v) {
            this.tl = v;
            this._listLength = Pair.isEmpty(v) ? 1
                : (check(v) ? v.length + 1 : 2);
            this._cachedHashCode = null;
        }
    }

    get length() {
        return this._listLength;
    }

    /**
     * @return {false}
     */
    isImmutable() {
        return false;
    }
}

/**
 * @param {*} v
 * @return {boolean} true iff v is a non-empty list or pair.
 */
export function check(v) {
    return typeof v === 'object' && v !== null && v.constructor === MPair;
}

export function make(hd, tl) {
    return new MPair(hd, tl);
}
