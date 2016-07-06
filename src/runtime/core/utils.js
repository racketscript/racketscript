import * as Primitive from "./primitive.js";

export
function isEqual(v1, v2) {
    if (Primitive.check(v1)) {
	return v1.equals(v2);
    } else {
	return v1 === v2;
    }
}


export
function isEq(v1, v2) {
    /* FIXME: We are not handling special case for Symbol */
    return v1 === v2;
}

export let isEqv = isEq;

export
function toString(v) {
    if (Primitive.check(v)) {
	return v.toString();
    } else {
	return "" + v;
    }
}
