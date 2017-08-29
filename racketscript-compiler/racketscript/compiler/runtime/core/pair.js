import {Primitive} from "./primitive.js";
import {isEqual} from "./equality.js";
import * as $ from "./lib.js";

// TODO: This should be a Primitive, not an Array.
export const Empty = [];

export function isEmpty(v) {
    return (v instanceof Array) && v.length === 0;
}

export class Pair extends Primitive {
    /** @private */
    constructor(hd, tl) {
        super();
        this.hd = hd;
        this.tl = tl;
        this._listLength = (tl === Empty)
            ? 1
            : isList(tl) && tl._listLength + 1;
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

    get length() {
        return this._listLength;
    }
}

export function check(v) {
    return (v instanceof Pair);
}

export function make(hd, tl) {
    return new Pair(hd, tl);
}

export function makeList(...items) {
    let len = items.length - 1;
    let result = Empty; /* TODO: wrap this? */
    while (len >= 0) {
	result = make(items[len--], result);
    }
    return result;
}

export function listToArray(lst) {
    let r = [];
    listForEach(lst, (x) => r.push(x));
    return r;
}

export function listFromArray(lst) {
    return makeList.apply(null, lst);
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
	if (result) {
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

export function _listLength(lst) {
    var len = 0;
    while (true) {
	if (isEmpty(lst)) {
            return len;
	}
	len += 1;
	lst = lst.cdr();
    }
    return len;
}

export function listLength(lst) {
    return (lst === Empty)
	? 0
	: lst._listLength;
}

export function isList(v) {
    return v === Empty || (check(v) && v._listLength !== false);
}
