"use strict"

import * as RLIB from "./core.js";

function CheckNumber(v) {
    if (typeof v !== 'number') {
	throw new Error("TypeError: '" + v + "' is not a number");
    }
}

function zero_p(v) {
    return v === 0;
}

function positive_p(v) {
    return v > 0;
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

function display(v) {
    /* TODO: this is still line */
    return displayln(v);
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

var racket_null = RLIB.Empty;
var null_p = empty_p;


function _a() {
    return [].reduce.call(arguments, function(x, r) {
	return r + RLIB.toString(x);
    }, "");
}

function string_append() {
    return "".concat.call(arguments);
}

function current_inspector() {
    /* stub */
    return false;
}

function make_struct_type(name, super_type, init_field_cnt, auto_field_cnt, auto_v,
			  props, inspector, proc_spec, immutables, guard,
			  constructor_name)
{
    return RLIB.makeStructType({
	name: name.toString(),
	super_type: super_type,
	init_field_cnt: init_field_cnt,
	auto_field_cnt: auto_field_cnt,
	auto_v: auto_v,
	props: props,
	inspector: inspector,
	proc_spec: proc_spec,
	immutables: immutables,
	guard: guard,
	constructor_name: constructor_name
    });
}

function make_struct_field_accessor(ref, index, field_name) {
    return function(s) {
	return ref(s, index);
    }
}

function length(lst) {
    var len = 0;
    while (true) {
        if (RLIB.isEmpty(lst)) {
            return len;
        }
        len += 1;
        lst = lst.cdr();
    }
}

function error(msg) {
    /** todo **/
    throw new Error(msg);
}

function apply() {
    var lam = arguments[0];
    var args = arguments[1];
    return lam.apply(null, RLIB.list_to_array(args));
}


var _times_ = RLIB.Number.mul;
var _by_ = RLIB.Number.div;
var _plus_ = RLIB.Number.add;
var _ = RLIB.Number.sub;
var _lt_ = RLIB.Number.lt;
var _gt_ = RLIB.Number.gt;
var _lt__eq_ = RLIB.Number.lte;
var _gt__eq_ = RLIB.Number.gte;
var _eq_ = RLIB.Number.equal;

export {
    racket_null,
    zero_p,
    positive_p,
    car,
    cdr,
    list,
    list_p,
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
    print_values,
    length,
    error,
    apply,

    // operators
    _times_,
    _plus_,
    _,
    _by_,
    _eq_,
    _lt_,
    _gt_,
    _lt__eq_,
    _gt__eq_,

    // non kernel functions
    string_append,
    _a,
    display,
    current_inspector,
    make_struct_type,
    make_struct_field_accessor
}
