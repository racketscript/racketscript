import * as rcore from "./core.js";

var RacketCoreError = rcore.RacketCoreError;

function CheckNumber(v) {
    if (typeof v !== 'number') {
	throw new RacketCoreError("TypeError",
				  "" + v + "' is not a number");
    }
}

function zero_p(v) {
    return v === 0;
}

function positive_p(v) {
    return v > 0;
}

function car(lst) {
    rcore.Pair.check(lst);
    return lst.car();
}

function cons(v1, v2) {
    return rcore.Pair.make(v1, v2);
}

function cdr(lst) {
    rcore.Pair.check(lst);
    return lst.cdr();
}

var list = rcore.Pair.makeList
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

var buffer = "";

function displayln(v) {
    /* TODO: Real thing takes port as well */
    if (v === true) {
	console.log(buffer + "#t");
    } else if (v === false) {
	console.log(buffer + "#f");
    } else if (v === undefined || v === null) {
	console.log(buffer + "#<void>");
    } else {
	console.log(buffer + rcore.toString(v));
    }
    buffer = "";
}

function display(v) {
    /* TODO: this is still line */
    if (v === true) {
	buffer += "#t";
    } else if (v === false) {
	buffer += "#f";
    } else if (v === undefined || v === null) {
	buffer += "#<void>";
    } else {
	buffer += rcore.toString(v);
    }
}

function newline() {
    return displayln("");
}

function print_values(v) {
    if (v !== undefined && v !== null) {
	if (typeof(v) == 'string') {
	    //TODO: Hack. Special cases
	    console.log('"' + v + '"');
	} else {
	    displayln(v);
	}
    }
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

function eqv_p(v1, v2) {
    return rcore.isEqv(v1, v2);
}

function eq_p(v1, v2) {
    return rcore.isEq(v1, v2);
}

function values() {
    return rcore.Values.make(arguments);
}

function call_with_values(generator, receiver) {
    var values = generator();
    if (rcore.Values.check(values)) {
	return receiver.apply(this, generator().getAll());
    } else if (values !== undefined && values !== null) {
	return receiver.apply(this, [values]);
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
    if (rcore.Pair.isEmpty(v)) {
	return true;
    } else if (rcore.Pair.check(v)) {
	return list_p(v.cdr());
    } else {
	return false;
    }
}

function empty_p(v) {
    return rcore.Pair.isEmpty(v);
}

var racket_null = rcore.Pair.Empty;
var null_p = empty_p;


function _a() {
    return [].reduce.call(arguments, function(x, r) {
	return r + rcore.toString(x);
    }, "");
}

function string_append() {
    return "".concat.call(arguments);
}

function current_inspector() {
    /* stub */
    return false;
}

function make_struct_type(name,
			  superType,
			  initFieldCount,
			  autoFieldCount,
			  autoV,
			  props,
			  inspector,
			  procSpec,
			  immutables,
			  guard,
			  constructorName)
{
    return rcore.Struct.makeStructType({
	name: name.toString(),
	superType: superType,
	initFieldCount: initFieldCount,
	autoFieldCount: autoFieldCount,
	autoV: autoV,
	props: props,
	inspector: inspector,
	procSpec: procSpec,
	immutables: immutables,
	guard: guard,
	constructorName: constructorName
    });
}

function make_struct_field_accessor(ref, index, fieldName) {
    return function(s) {
	return ref(s, index);
    }
}

function make_struct_field_mutator(set, index, fieldName) {
    return function(s, v) {
	return set(s, index, v);
    }
}

function length(lst) {
    var len = 0;
    while (true) {
        if (rcore.Pair.isEmpty(lst)) {
            return len;
        }
        len += 1;
        lst = lst.cdr();
    }
}

function error(msg) {
    /** todo **/
    throw new RacketCoreError(msg);
}

function apply() {
    var lam = arguments[0];
    var args = arguments[1];
    return lam.apply(null, rcore.Pair.listToArray(args));
}

function vector() {
    var items = rcore.argumentsToArray(arguments);
    return rcore.Vector.make(items, true);
}

function vector_ref(vec, i) {
    rcore.Vector.check(vec);
    return vec.ref(i);
}

function vector_set_bang_(vec, i, v) {
    rcore.Vector.check(vec);
    vec.set(i, v);
}

function map() {
    var fn = arguments[0];
    if (arguments.length < 2) {
	error("map: " + "needs at-least two arguments");
    }
    var lst_len = length(arguments[1]);
    for (var i = 1; i < arguments.length; i++) {
	if (length(arguments[i]) != lst_len) {
	    error("map: " + "all input lists must have equal length");
	}
    }

    var n_args = arguments.length - 1;
    var result = [];
    for (var i = 0; i < lst_len; i++) {
	var args = [];
	for (var j = 1; j <= n_args; j++) {
	    args.push(car(arguments[j]));
	    arguments[j] = cdr(arguments[j]);
	    result.push(fn.apply(null, args));
	}
    }

    return rcore.Pair.listFromArray(result);
}

var _times_ = rcore.Number.mul;
var _by_ = rcore.Number.div;
var _plus_ = rcore.Number.add;
var _ = rcore.Number.sub;
var _lt_ = rcore.Number.lt;
var _gt_ = rcore.Number.gt;
var _lt__eq_ = rcore.Number.lte;
var _gt__eq_ = rcore.Number.gte;
var _eq_ = rcore.Number.equals;

// Structs

function struct_type_p(v) {
    return rcore.Struct.isStructType(v);
}

function make_struct_type_property(name, guard, supers, canImpersonate) {
    return rcore.Struct.makeStructTypeProperty({
	name: name,
	guard: guard,
	supers: supers,
	canImpersonate: canImpersonate
    });
}

function check_struct_type(name, what) {
    // TODO: in define-struct.rkt. See struct/super.rkt
    if (what) {
	if (!rcore.Struct.isStructType(v)) {
	    throw new RacketCoreError("not a struct-type");
	}
	return what;
    }
}

// Hashes

function make_immutable_hash(assocs) {
    return rcore.Hash.makeFromAssocs(assocs, "equal", false);
}

function hash(...vals) {
    if (vals.length % 2 !== 0) {
	throw new Error("invalid number of arguments");
    }

    let items = [];
    for (let i = 0; i < vals.length; i += 2) {
	items.push([vals[i], vals[i + 1]]);
    }

    return rcore.Hash.makeEqual(items, false);
}

function hash_ref(h, k, fail) {
    return h.ref(k, fail);
}

function hash_set(h, k, v) {
    return h.set(k, v);
}

function string() {
    var result = "";
    for (var v in arguments) {
	result += arguments[v];
    }
    return result;
}

function for_each(lam, lst) {
    map(lam, lst);
    return null;
}

function rvoid() {
    return null;
}

var prop_evt = make_struct_type_property("prop:evt").getAt(0)
var prop_procedure = make_struct_type_property("prop:procedure").getAt(0)
var prop_checked_procedure = make_struct_type_property("prop:checked-procedure").getAt(0)
var prop_impersonator_of = make_struct_type_property("prop:impersonator-of").getAt(0)
var prop_method_arity_error = make_struct_type_property("prop:method-arity-error").getAt(0)
var prop_incomplete_arity = make_struct_type_property("prop:incomplete-arity").getAt(0)
var prop_arity_string = make_struct_type_property("prop:arity-string").getAt(0)

function current_output_port() {}
function output_port_p() { return false; }
function raise_argument_error() { return false; }
function current_print() { return displayln; }
function make_thread_cell() { return null; }
function make_continuation_prompt_tag() { return null; }
function gensym() { return null; }
function system_type() { return null; }
function format() { return null; }
function string__gt_bytes_by_utf_8() { return null; }
function byte_regexp() { return null; }
function make_weak_hash() { return null; }
function procedure_arity() { return 0; }
function make_keyword_checker() { return 0; }
function integer_p() { return 0; }
function arity_at_least_p() { return 0; }
function make_hasheq() { return 0; }
function hash_set_bang_() { return 0; }

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
    eqv_p,
    eq_p,
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
    vector,
    vector_ref,
    vector_set_bang_,
    string,
    newline,
    rvoid,

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
    map,
    for_each,

    //structs
    struct_type_p,
    make_struct_type,
    make_struct_type_property,
    make_struct_field_accessor,
    make_struct_field_mutator,
    check_struct_type,
    prop_evt,

    // hash
    hash_ref,
    hash_set,
    hash,
    make_immutable_hash,

    // dummies
    current_output_port,
    output_port_p,
    current_print,
    raise_argument_error,
    make_thread_cell,
    make_continuation_prompt_tag,
    gensym,
    system_type,
    format,
    string__gt_bytes_by_utf_8,
    byte_regexp,
    make_weak_hash,
    prop_checked_procedure,
    prop_impersonator_of,
    prop_method_arity_error,
    prop_incomplete_arity,
    prop_arity_string,
    prop_procedure,
    procedure_arity,
    make_keyword_checker,
    integer_p,
    arity_at_least_p,
    make_hasheq,
    hash_set_bang_
};
