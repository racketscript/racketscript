import {Primitive} from "./primitive.js";
import * as $ from "./lib.js";

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
	let result = "(";
	let rest = this;
	while (true) {
	    if (check(rest)) {
		let hd = rest.hd;
		result += $.toString(hd);
	    } else {
		result += ". " + $.toString(rest);
		break;
	    }
	    rest = rest.tl;
	    if (isEmpty(rest)) {
		break;
	    } else {
		result += " ";
	    }
	}
	result += ")";
	return result;
    }

    toRawString() {
	return "'" + this.toString();
    }

    equals(v) {
	if (!check(v)) {
	    return false;
	} else if (this._listLength !== v._listLength) {
	    return false;
	}

	let hd1 = this.hd;
	let tl1 = this.tl;
	let hd2 = v.hd;
	let tl2 = v.tl;

	while (true) {
	    if ($.isEqual(hd1, hd2)) {
		return $.isEqual(tl1, tl2);
	    } else {
		return false;
	    }
	}

	return true;
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
}

export function check(v) {
    return (v instanceof MPair);
}

export function make(hd, tl) {
    return new MPair(hd, tl);
}
