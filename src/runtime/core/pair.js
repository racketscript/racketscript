import {Primitive} from "./primitive.js";
import * as $ from "./lib.js";

export const Empty = [];

export function isEmpty(v) {
    return (v instanceof Array) && v.length === 0;
}

class Pair extends Primitive {
    constructor(hd, tl) {
	super();
	this.hd = hd;
	this.tl = tl;
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
	check(v);

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
}

export function check(v) {
    return (v instanceof Pair);
}

export function make(hd, tl) {
    return new Pair(hd, tl);
}

export function makeList() {
    let len = arguments.length - 1;
    let result = Empty; /* TODO: wrap this? */
    while (len >= 0) {
	result = make(arguments[len--], result);
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

export function listMap(lst, fn) {
    let result = [];
    let mapper = (x) => result.push(result, fn(x));
    listForEach(lst, mapper);
    return listFromArray(result);
}


export function listLength(lst) {
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
