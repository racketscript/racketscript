import {default as Primitive} from "./primitive.js";
import * as rutils from "./utils.js";

export const Empty = [];

export
function isEmpty(v) {
    return (v instanceof Array) && v.length === 0;
}

class Pair extends Primitive {
    constructor(hd, tl) {
	super();
	this.hd = hd;
	this.tl = tl;
    }

    car() {
	return this.hd;
    }

    cdr() {
	return this.tl;
    }

    toString() {
	var result = "(";
	var rest = this;
	while (true) {
	    if (check(rest)) {
		var hd = rest.hd;
		result += rutils.toString(hd);
	    } else {
		result += ". " + rutils.toString(rest);
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
	var hd1 = this.hd;
	var tl1 = this.tl;
	var hd2 = v.hd;
	var tl2 = v.tl;

	while (true) {
	    if (rutils.isEqual(hd1, hd2)) {
		return rutils.isEqual(tl1, tl2);
	    } else {
		return false;
	    }
	}

	return true;
    }
}

export
function check(v) {
    return (v instanceof Pair);
}

export
function make(hd, tl) {
    return new Pair(hd, tl);
}

export
function makeList() {
    var len = arguments.length - 1;
    var result = Empty; /* TODO: wrap this? */
    while (len >= 0) {
	result = make(arguments[len--], result);
    }
    return result;
}

export
function listToArray(lst) {
    var r = [];
    while (!isEmpty(lst)) {
        r.push(lst.hd);
        lst = lst.tl;
    }
    return r;
}

export
function listFromArray(lst) {
    return makeList.apply(null, lst);
}
