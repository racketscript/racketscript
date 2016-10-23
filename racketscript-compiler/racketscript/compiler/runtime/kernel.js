import * as Core from "./core.js";

/* --------------------------------------------------------------------------*/
// Helpers

function typeCheckOrRaise(type, what) {
    if (!type.check(what)) {
	throw Core.racketContractError("expected a {0}, but given {1}", type, what);
    }
}

function isNumber(v) {
    return typeof v === 'number';
}

/* --------------------------------------------------------------------------*/
// All exports go in exports

export const exports = {};

/* --------------------------------------------------------------------------*/
// Equality

exports["equal?"] = Core.isEqual;
exports["eqv?"] = Core.isEqv;
exports["eq?"] = Core.isEq;

/* --------------------------------------------------------------------------*/
// Values

exports["values"] = function (...vals) {
    return Core.Values.make(vals);
}

var callWithValues = exports["call-with-values"] = function (generator, receiver) {
    let values = generator();
    if (Core.Values.check(values)) {
	return receiver.apply(this, generator().getAll());
    } else if (values !== undefined && values !== null) {
	return receiver.apply(this, [values]);
    }
}

/* --------------------------------------------------------------------------*/
// Void

exports["void"] = function () {
    return null;
}

exports["void?"] = function (v) {
    return v === null || v === undefined;
}

/* --------------------------------------------------------------------------*/
// Numbers

exports["number?"] = Core.Number.check;

exports["integer?"] = Number.isInteger;

exports["zero?"] = function (v) {
    return v === 0;
}

exports["positive?"] = function (v) {
    typeCheckOrRaise(Core.Number, v);
    return v > 0;
}

exports["negative?"] = function (v) {
    typeCheckOrRaise(Core.Number, v);
    return v < 0;
}

exports["add1"] = function (v) {
    typeCheckOrRaise(Core.Number, v);
    return v + 1;
}

exports["sub1"] = function (v) {
    typeCheckOrRaise(Core.Number, v);
    return v - 1;
}

exports["quotient"] = function (dividend, divisor) {
    return Math.floor(dividend / divisor);
}

exports["*"] = Core.Number.mul;
exports["/"] = Core.Number.div;
exports["+"] = Core.Number.add;
exports["-"] = Core.Number.sub;
exports["<"] = Core.Number.lt;
exports[">"] = Core.Number.gt;
exports["<="] = Core.Number.lte;
exports[">="] = Core.Number.gte;
exports["="] = Core.Number.equals;

/* TODO: Support bignums */
exports["exact->inexact"] = (v) => v;
exports["expt"] = (w, z) => Math.pow(w, z);
exports["sqrt"] = (v) => Math.sqrt(v);

/* -------------------------------------------------------------------------*/
// Boolean

exports["not"] = function (v) {
    return v === false || false;
}

/* --------------------------------------------------------------------------*/
// Pairs

exports["car"] = function (pair) {
    typeCheckOrRaise(Core.Pair, pair);
    return pair.car();
}

exports["cdr"] = function (pair) {
    typeCheckOrRaise(Core.Pair, pair);
    return pair.cdr();
}

exports["cons"] = function (hd, tl) {
    return Core.Pair.make(hd, tl);
}

exports["cons?"] = function (v) {
    return Core.Pair.check(v);
}

exports["pair?"] = exports["cons?"]

// Lists

exports["null"] = Core.Pair.Empty;

var list = exports["list"] = Core.Pair.makeList;
exports["first"] = exports["car"]; //TODO: Should be list
exports["second"] = function (lst) {
    return lst.cdr().car();
}
exports["rest"] = exports["cdr"];

exports["list?"] = function isList(v) {
    //TODO: Make this iterative
    if (Core.Pair.isEmpty(v)) {
	return true;
    } else if (Core.Pair.check(v)) {
	return isList(v.cdr());
    } else {
	return false;
    }
}

exports["list*"] = function () {
    return Core.argumentsToArray(arguments)
	.reverse()
	.reduce((acc, v) => Core.Pair.make(v, acc));
}

exports["empty?"] = Core.Pair.isEmpty;
exports["null?"] = Core.Pair.isEmpty;

var length = exports["length"] = function (lst) {
    return Core.Pair.listLength(lst);
}

exports["reverse"] = function(lst) {
    typeCheckOrRaise(Core.Pair, lst);
    let result = Core.Pair.Empty;
    while (Core.Pair.isEmpty(lst) === false) {
	result = Core.Pair.make(lst.hd, result);
	lst = lst.tl;
    }
    return result;
}

exports["for-each"] = function (lam, ...lsts) {
    map.apply(null, [lam].concat(lsts));
    return null;
}

/* --------------------------------------------------------------------------*/
// Structs

exports["make-struct-type"] = function (name,
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
    return Core.Struct.makeStructType({
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

exports["make-struct-field-accessor"] = function (ref, index, fieldName) {
    return function (s) {
	return ref(s, index);
    }
}

exports["make-struct-field-mutator"] = function (set, index, fieldName) {
    return function (s, v) {
	return set(s, index, v);
    }
}

exports["struct-type?"] = function (v) {
    return Core.Struct.isStructType(v);
}

exports["make-struct-type-property"] = function (name, guard, supers, canImpersonate) {
    return Core.Struct.makeStructTypeProperty({
	name: name,
	guard: guard,
	supers: supers,
	canImpersonate: canImpersonate
    });
}

exports["check-struct-type"] = function (name, what) {
    // TODO: in define-struct.rkt. See struct/super.rkt
    if (what) {
	if (!Core.Struct.isStructType(v)) {
	    throw racketCoreError("not a struct-type");
	}
	return what;
    }
}


/* --------------------------------------------------------------------------*/
// Vectors

exports["vector"] = function () {
    var items = Core.argumentsToArray(arguments);
    return Core.Vector.make(items, true);
}

exports["vector?"] = function(v) {
    return Core.Vector.check(v);
}

exports["vector-length"] = function(v) {
    return v.length(v);
}

exports["vector-ref"] = function (vec, i) {
    Core.Vector.check(vec);
    return vec.ref(i);
}

exports["vector-set!"] = function (vec, i, v) {
    Core.Vector.check(vec);
    vec.set(i, v);
}

/* --------------------------------------------------------------------------*/
// Hashes

exports["make-immutable-hash"] = function (assocs) {
    return rcore.Hash.makeFromAssocs(assocs, "equal", false);
}

exports["hash"] = function (...vals) {
    if (vals.length % 2 !== 0) {
	throw new Error("invalid number of arguments");
    }

    let items = [];
    for (let i = 0; i < vals.length; i += 2) {
	items.push([vals[i], vals[i + 1]]);
    }

    return Core.Hash.makeEqual(items, false);
}

exports["hasheq"] = function (...vals) {
    if (vals.length % 2 !== 0) {
	throw new Error("invalid number of arguments");
    }

    let items = [];
    for (let i = 0; i < vals.length; i += 2) {
	items.push([vals[i], vals[i + 1]]);
    }

    return Core.Hash.makeEq(items, false);
}

exports["hash-ref"] = function (h, k, fail) {
    return h.ref(k, fail);
}

exports["hash-set"] = function (h, k, v) {
    return h.set(k, v);
}

/* --------------------------------------------------------------------------*/
// Higher Order Functions

exports["apply"] = function (lam, ...args) {
    let lst = Core.Pair.listToArray(args.pop()); /* TODO: Check. Must be a list */
    return lam.apply(null, args.concat(lst));
}

var reverse = exports["reverse"] = function (lst) {
    let result = Core.Pair.Empty;
    while (Core.Pair.isEmpty(lst) === false) {
	result = Core.Pair.make(lst.hd, result);
	lst = lst.tl;
    }
    return result;
}


var map = exports["map"] = function map(fn, ...lists) {
    if (lists.length <= 0) {
	error("map: needs at-least two arguments");
    }

    var lst_len = length(lists[0]);
    for (let i = 0; i < lists.length; i++) {
	if (length(lists[i]) != lst_len) {
	    error("map: all input lists must have equal length");
	}
    }

    var result = [];
    for (let i = 0; i < lst_len; i++) {
	let args = [];
	for (var j = 0; j < lists.length; j++) {
	    args.push(lists[j].car());
	    lists[j] = lists[j].cdr();
	}
	result.push(fn.apply(null, args));
    }

    return Core.Pair.listFromArray(result);
}

var foldl = exports["foldl"] = function (fn, init, ...lists) {
    if (lists.length <= 0) {
	error("foldl: foldl needs at-least one list");
    }

    var lst_len = length(lists[0]);
    for (let i = 0; i < lists.length; i++) {
	if (length(lists[i]) != lst_len) {
	    error("foldl: all input lists must have equal length");
	}
    }

    var result = init;
    for (let i = 0; i < lst_len; i++) {
	let args = [];
	for (var j = 0; j < lists.length; j++) {
	    args.push(lists[j].car());
	    lists[j] = lists[j].cdr();
	}
	args.push(result);
	result = fn.apply(null, args);
    }

    return result;
}

function _foldr(fn, init, lists) {
    if (Core.Pair.isEmpty(lists[0])) {
	return init;
    } else {
	let args = [];
	for (var ii = 0; ii < lists.length; ii++) {
	    args.push(lists[ii].car());
	    lists[ii] = lists[ii].cdr();
	}

	args.push(_foldr(fn, init, lists));
	return fn.apply(null, args)
    }
}

var foldr = exports["foldr"] = function (fn, init, ...lists) {
    if (lists.length <= 0) {
	error("foldl: foldl needs at-least one list");
    }

    var lst_len = length(lists[0]);
    for (let i = 0; i < lists.length; i++) {
	if (length(lists[i]) != lst_len) {
	    error("foldl: all input lists must have equal length");
	}
    }

    return _foldr(fn, init, lists);
}

var range = exports["range"] = function (start, end, step) {
    typeCheckOrRaise(Core.Number, start);
    if (end === undefined) {
	end = start;
	start = 0;
    }
    if (step === undefined) {
	step = 1;
    }
    typeCheckOrRaise(Core.Number, end);
    typeCheckOrRaise(Core.Number, step);
    var result = [];
    if (step >= 0 && start < end) {
	for (let last = start; last < end; last += step) {
	    result.push(last);
	}
    } else if (step <= 0 && end < start) {
	for (let last = start; last > end; last += step) {
	    result.push(last);
	}
    }
    return Core.Pair.listFromArray(result);
}


/* --------------------------------------------------------------------------*/
// Strings

exports["~a"] = function () {
    return [].reduce.call(arguments, function (x, r) {
	return r + Core.toString(x);
    }, "");
}

exports["string-append"] = function (...args) {
    switch (args.length) {
    case 1: return args[0];
    case 2: return args[0] + args[1];
    case 3: return args[0] + args[1] + args[2];
    default: return args.join("");
    }
}

exports["string"] = function () {
    var resultn = "";
    for (var v in arguments) {
	result += arguments[v];
    }
    return result;
}

exports["string=?"] = function (sa, sb) {
    return sa === sb;
}

exports["string?"] = function (v) {
    return typeof(v) === 'string';
}

var format = exports["format"] = function (pattern, ...args) {
    //TODO: Only ~a is supported
    var matched = 0;
    return pattern.replace(/~a/g, function(match) {
	if (args[matched] == 'undefined') {
	    throw Core.racketContractError("insufficient pattern arguments");
        } else {
            return args[matched++];
        }
    });
}

exports["symbol->string"] = function (v) {
    typeCheckOrRaise(Core.Symbol, v);
    return v.toString();
}

exports["symbol?"] = function (v) {
    return Core.Symbol.check(v);
}

exports["symbol=?"] = function (s, v) {
    typeCheckOrRaise(Core.Symbol, s);
    return s.equals(v);
}

exports["string-length"] = function (v) {
    if (typeof v !== 'string') {
	throw Core.racketContractError("expected a string");
    }
    return v.length;
}

exports["string-downcase"] = (v) => v.toLowerCase(v);
exports["string-upcase"] = (v) => v.toUpperCase(v);

exports["substring"] = function (str, start, end = false) {
    if (typeof str !== 'string') {
	throw Core.racketContractError("expected a string");
    } else if (start < 0) {
	throw Core.racketContractError("invalid start index");
    } else if (end !== false && (end < 0 || end > str.length)) {
	throw Core.racketContractError("invalid end index");
    } else if (end === false) {
	end = str.length;
    }
    return str.substring(start, end);
}

exports["string-split"] = function (str, sep) {
    if (typeof str !== 'string') {
	throw Core.racketContractError("expected a string");
    }
    return Core.Pair.listFromArray(str.split(sep));
}

/* --------------------------------------------------------------------------*/
// Box

exports["box"] = function(v) {
    return Core.Box.make(v);
}

exports["unbox"] = function(v) {
    return v.get();
}

exports["set-box!"] = function(b, v) {
    b.set(v);
}

/* --------------------------------------------------------------------------*/
// Printing to Console

let __buffer = ""; //HACK

var displayln = exports["displayln"] = function (v) {
    /* TODO: Real thing takes port as well */
    if (v === true) {
	console.log(__buffer + "#t");
    } else if (v === false) {
	console.log(__buffer + "#f");
    } else if (v === undefined || v === null) {
	console.log(__buffer + "#<void>");
    } else {
	console.log(__buffer + Core.toString(v));
    }
    __buffer = "";
}

exports["display"] = function (v) {
    /* TODO: this is still line */
    if (v === true) {
	__buffer += "#t";
    } else if (v === false) {
	__buffer += "#f";
    } else if (v === undefined || v === null) {
	__buffer += "#<void>";
    } else {
	__buffer += Core.toString(v);
    }
}

exports["newline"] = function () {
    return exports["displayln"]("");
}

exports["print-values"] = function (v) {
    if (v !== undefined && v !== null) {
	if (typeof v == 'string') {
	    //TODO: Hack. Special cases
	    console.log('"' + v + '"');
	} else {
	    exports["displayln"](v);
	}
    }
}

/* --------------------------------------------------------------------------*/
// Errors

var error = exports["error"] = function (...args) {
    if (args.length === 1 && Core.Symbol.check(args[0])) {
	throw Core.racketCoreError(args[0].toString());
    } else if (args.length > 0 && typeof args[0] === 'string') {
	throw Core.racketCoreError(args.map((v) => v.toString()).join(" "));
    } else if (args.length > 0 && Core.Symbol.check(args[0])) {
	let pattern = args.shift().toString()
	    .concat(" ")
	    .concat(args.shift());
	throw Core.racketCoreError(pattern, args);
    } else {
	throw Core.racketContractError("error: invalid arguments");
    }
}

/* --------------------------------------------------------------------------*/
// Not Implemented/Unorganized/Dummies

exports["current-inspector"] = () => true;

exports["raise-argument-error"] = function() {
}

exports["check-method"] = function() {
}

exports["random"] = function (...args) {
    switch (args.length) {
    case 0: return Math.random();
    case 1:
	if (args[0] > 0) {
	    return Math.floor(Math.random() * args[0]);
        } else {
	    error("random: argument should be positive");
        }
    case 2:
	if (args[0] > 0 && args[1] > args[0]) {
	    return Math.floor(args[0] + Math.random() * (args[1] - args[0]));
	} else {
	    error("random: invalid arguments");
	}
    default:
	error("random: invalid number of arguments")
    }
}

exports["member"] = function (v, lst) {
    while (Core.Pair.isEmpty(lst) == false) {
	if (Core.isEqual(v, lst.hd)) {
	    return lst;
	}
	lst = lst.tl;
	continue;
    }
    return false;
}

exports["false"] = false;
exports["true"] = true;
exports["empty"] = Core.Pair.Empty;

exports["number->string"] = function (n) {
    typeCheckOrRaise(Core.Number, n);
    return n.toString();
}

exports["ormap"] = function (fn, ...lists) {
    return foldl.apply(this, [(v, a) => a || fn(v), false].concat(lists));
}

exports["andmap"] = function (fn, ...lists) {
    return foldl.apply(this, [(v, a) => a && fn(v), true].concat(lists));
}

exports["filter"] = function (fn, lst) {
    let result = Core.Pair.Empty;
    while (Core.Pair.isEmpty(lst) == false) {
	if (fn(lst.hd)) {
	    result = Core.Pair.make(lst.hd, result);
	}
	lst = lst.tl;
    }
    return reverse(result);
}

exports["abs"] = Math.abs;
exports["floor"] = Math.floor;
exports["sin"] = Math.sin;
exports["cos"] = Math.cos;
exports["tan"] = Math.tan;
exports["ceiling"] = Math.ceil;
exports["round"] = Math.round;
exports["min"] = Math.min;
exports["max"] = Math.max;
exports["false?"] = (v) => v === false;

exports["list-ref"] = function (lst, pos) {
    let i = 0;
    while (Core.Pair.isEmpty(lst) === false) {
	if (i === pos) {
	    return lst.hd;
	}
	lst = lst.tl;
	i += 1;
    }
    error("list-ref?: insufficient elements");
}


var append = exports["append"] = function (...lists) {
    let result = Core.Pair.Empty;
    for (let list of lists) {
	result = foldr(Core.Pair.make, list, result);
    }
    return result;
}

exports["build-list"] = function (n, proc) {
    let result = Core.Pair.Empty;
    for (let i = 0; i < n; ++i) { 
	result = Core.Pair.make(proc(i), result);
    }
    return reverse(result);
}

exports["make-list"] = function (n, v) {
    let result = Core.Pair.Empty;
    for (let i = 0; i < n; ++i) {
	result = Core.Pair.make(v, result);
    }
    return result;
}

exports["assoc"] = function (k, lst) {
    while (Core.Pair.isEmpty(lst) === false) {
	if (Core.isEqual(k, lst.hd.hd)) {
	    return lst.hd;
	}
	lst = lst.tl;
    }
    return false;
}

var flatten = exports["flatten"] = function (lst) {
    if (Core.Pair.isEmpty(lst)) {
        return lst;
    } else if (Core.Pair.check(lst)) {
        return append(flatten(lst.hd), flatten(lst.tl));
    } else {
        return list(lst);
    }
};

exports["current-seconds"] = function() {
    return Math.floor(Date.now() / 1000);
}

exports["sqr"] = function (v) {
    return v * v;
}

exports["remainder"] = function (a, b) {
    return a % b;
}

exports["compose"] = function (...procs) {
    return function () {
	let result = Core.argumentsToArray(arguments);
	for (let p of procs.reverse()) {
	    result = p.apply(null, result);
	    if (Core.Values.check(result)) {
		result = result.getAll();
	    } else {
		result = [result]
	    }
	}
	if (result.length === 1) {
	    return result[0];
	} else {
	    return Core.Values.make(result);
	}
    }
}

exports["compose1"] = function(...procs) {
    return function(v) {
	let result = v;
	for (let p of procs.reverse()) {
	    result = p(result);
	}
	return result;
    }
}
