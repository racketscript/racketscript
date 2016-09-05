import * as Core from "./core.js";

/* --------------------------------------------------------------------------*/
// Helpers

function checkContractExn(type, what) {
    if (!type.check(what)) {
	throw new Core.RacketContractError("expected a {0}, but given {1}", type, what);
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

exports["call-with-values"] = function (generator, receiver) {
    var values = generator();
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
    checkContractExn(Core.Number, v);
    return v > 0;
}

exports["negative?"] = function (v) {
    checkContractExn(Core.Number, v);
    return v < 0;
}

exports["add1"] = function (v) {
    checkContractExn(Core.Number, v);
    return v + 1;
}

exports["sub1"] = function (v) {
    checkContractExn(Core.Number, v);
    return v - 1;
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

/* -------------------------------------------------------------------------*/
// Boolean

exports["not"] = function (v) {
    return v === false || false;
}

/* --------------------------------------------------------------------------*/
// Pairs

exports["car"] = function (pair) {
    checkContractExn(Core.Pair, pair);
    return pair.car();
}

exports["cdr"] = function (pair) {
    checkContractExn(Core.Pair, pair);
    return pair.cdr();
}

exports["cons"] = function (hd, tl) {
    return Core.Pair.make(hd, tl);
}

// Lists

exports["null"] = Core.Pair.Empty;

exports["list"] = Core.Pair.makeList;
exports["first"] = exports["car"]; //TODO: Should be list
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

exports["empty?"] = Core.Pair.isEmpty;
exports["null?"] = Core.Pair.isEmpty;

var length = exports["length"] = function (lst) {
    return Core.Pair.listLength(lst);
}

exports["for-each"] = function (lam, lst) {
    map(lam, lst);
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
	    throw new RacketCoreError("not a struct-type");
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

exports["hash-ref"] = function (h, k, fail) {
    return h.ref(k, fail);
}

exports["hash-set"] = function (h, k, v) {
    return h.set(k, v);
}

/* --------------------------------------------------------------------------*/
// Higher Order Functions

exports["apply"] = function (lam, args) {
    return lam.apply(null, Core.Pair.listToArray(args));
}


exports["map"] = function map(fn, ...lists) {
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

/* --------------------------------------------------------------------------*/
// Strings

exports["~a"] = function () {
    return [].reduce.call(arguments, function (x, r) {
	return r + Core.toString(x);
    }, "");
}

exports["string-append"] = function () {
    switch (arguments.length) {
    case 1: return arguments[0];
    case 2: return arguments[0] + arguments[1];
    case 3: return arguments[0] + arguments[1] + arguments[3];
    default: return "".concat.call(arguments);
    }
}

exports["string"] = function () {
    var resultn = "";
    for (var v in arguments) {
	result += arguments[v];
    }
    return result;
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

exports["displayln"] = function (v) {
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

exports["error"] = function (...args) {
    // TODO
    throw new Core.RacketCoreError.apply(this, args);
}

/* --------------------------------------------------------------------------*/
// Not Implemented/Unorganized/Dummies

exports["current-inspector"] = () => false;
