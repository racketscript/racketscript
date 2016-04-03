"use strict"

import * as RLIB from "core.js";

function CheckNumber(v) {
    if (typeof v !== 'number') {
	throw new Error("TypeError: '" + v + "' is not a number");
    }
}

function zero_p(v) {
    return v === 0;
}

function car(lst) {
    RLIB.Pair.check(lst);
    return lst.car();
}

function cons(v1, v2) {
    return RLIB.Pair.make(v1, v2);
}

function cdr(lst) {
    RLIB.Pair.check(lst);
    return lst.cdr();
}

var list = RLIB.makeList

var first = car;
var rest = cdr;

function sub1(v) {
    CheckNumber(v);
    return v - 1;
}

function add1(v) {
    CheckNumber(v);
    return v + 1;
}

function displayln(v) {
    /* TODO: Real thing takes port as well */
    console.log(RLIB.toString(v));
}

function print_values(v) {
    // TODO: Print single or multiple values
    //console.log(RLIB.toString(v));
}

function equal_p(v1, v2) {
    /* TODO: compare values not references */
    var t1 = typeof v1;
    var t2 = typeof v2;
    if (t1 !== t2) {
	return false;
    } else if (t1 === 'object' && t2 === 'object') {
	return v1.equals(v2);
    } else {
	return v1 === v2;
    }
}

var values = RLIB.Values.make

function call_with_values(generator, receiver) {
    var values = generator();
    if (values instanceof RLIB.Values) {
	return receiver.apply(this, generator().getAll());
    } else {
	return receiver(values);
    }
}

function not(v) {
    if (v === false) {
	return true;
    } else {
	return false;
    }
}

function list_p(v) {
    /* TODO: Make this iterative */
    if (RLIB.isEmpty(v)) {
	return true;
    } else if (v instanceof RLIB.Pair) {
	return list_p(v.cdr());
    } else {
	return false;
    }
}

function empty_p(v) {
    return RLIB.isEmpty(v);
}

var null_p = empty_p;

export {
    zero_p,
    car,
    cdr,
    list,
    first,
    rest,
    sub1,
    add1,
    displayln,
    equal_p,
    values,
    call_with_values,
    not,
    empty_p,
    cons,
    null_p,
    print_values
}
