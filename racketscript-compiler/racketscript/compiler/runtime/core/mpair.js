import {Primitive} from "./primitive.js";
import {isEqual} from "./equality.js";
import * as $ from "./lib.js";
import {Empty, isEmpty, isList} from "./pair.js";

class MPair extends Primitive {
    constructor(hd, tl) {
	super();
	this.hd = hd;
	this.tl = tl;
	this._listLength = (tl === Empty)
	    ? 1
	    : isList(tl) && tl._listLength + 1;
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

    car() {
	return this.hd;
    }

    cdr() {
	return this.tl;
    }

    setCar(v) {
	this.hd = v;
    }

    setCdr(v) {
	this.tl = v;
    }

    get length() {
        return this._listLength;
    }
}

export function check(v) {
    return (v instanceof MPair);
}

export function make(hd, tl) {
    return new MPair(hd, tl);
}
