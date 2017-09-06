import { Primitive } from "./primitive.js";
import { isEqual } from "./equality.js";
import * as $ from "./lib.js";
import { EMPTY, isEmpty, isList } from "./pair.js";

class MPair extends Primitive {
    /** @private */
    constructor(hd, tl) {
        super();
        this.hd = hd;
        this.tl = tl;
        this._listLength = isList(tl) ? tl.length + 1 : -1;
        this._cachedHashCode = null;
    }

    toString() {
        const result = ['('];
        let rest = this;
        while (true) {
            if (check(rest)) {
                result.push($.toString(rest.hd));
            } else {
                result.push('. ', $.toString(rest));
                break;
            }
            rest = rest.tl;
            if (isEmpty(rest)) {
                break;
            } else {
                result.push(' ');
            }
        }
        result.push(')');
        return result.join('');
    }

    toRawString() {
        return "'" + this.toString();
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

    setCar(v) {
        if (this.hd !== v) {
            this.hd = v;
            this._cachedHashCode = null;
        }
    }

    setCdr(v) {
        if (this.tl !== v) {
            this.tl = v;
            this._listLength = isList(tl) ? tl.length + 1 : -1;
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
