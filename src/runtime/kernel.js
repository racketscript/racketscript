"use strict"

import * as RLIB from "core.js";

function TypeCheck(v, t) {
    if (v instanceof t === false) {
        throw new Error("TypeError: '" + v + "' is not " + t);
    }
}

function zero_p(v) {
    return v === 0;
}

function car(lst) {
    RLIB.Cons.check(lst);
    return lst.car();
}

function cdr(lst) {
    RLIB.Cons.check(lst);
    return lst.cdr();
}

var list = RLIB.Cons.makeList

var first = car;
var rest = cdr;

function sub1(v) {
    TypeCheck(v, Number);
    return v - 1;
}

function add1(v) {
    TypeCheck(v, Number);
    return v + 1;
}

function displayln(v) {
    /* TODO: Real thing takes port as well */
    console.log(v);
}

function print_values(v) {
    console.log(v.getAll());
}

function equal_p(v1, v2) {
    /* TODO: compare values not references */
    return v1 === v2;
}

var values = RLIB.Values.make

function call_with_values(generator, receiver) {
    return receiver.apply(this, generator().getAll());
}

function not(v) {
    if (v === false) {
	return true;
    } else {
	return false;
    }
}

function empty_p(v) {
    /* TODO: Make this iterative */
    if (v instanceof Cons) {
	var tail = Cons.cdr(v);
	if (Cons.isEmpty(tail)) {
	    return true;
	} else {
	    return empty_p(tail);
	}
    } else {
	return false;
    }
}

var null_p = empty_p

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
    print_values
}
