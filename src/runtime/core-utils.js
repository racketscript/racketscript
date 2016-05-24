import * as Primitive from "./core-primitive.js";

export
function isEqual(v1, v2) {
    if (Primitive.check(v1)) {
	return v1.equals(v2);
    } else {
	return v1 === v2;
    }
}

export
function toString(v) {
    if (Primitive.check(v)) {
	return v.toString();
    } else {
	return "" + v;
    }
}
