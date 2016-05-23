"use strict"

class Primitive {
    constructor() {
	//
    }

    toString() {
	throw new Error("Not Implemented");
    }

    toRawString() {
	throw new Error("Not Implemented");
    }

    equals(v2) {
	return this.isEqual(this, v2);
    }

    static make() {
	throw new Error("Not Implemented");
    }
    
    static check(v1) {
	throw new Error("Not Implemented");
    }

    static isEqual(v1, v2) {
	throw new Error("Not Implemented");
    }
}

class Symbol extends Primitive {
    constructor(v) {
	super();
	this.v = v;
    }

    toString() {
	return this.v;
    }

    toRawString() {
	return "'" + this.v;
    }

    static make(v) {
	return new Symbol(v);
    }

    static isEqual(v1, v2) {
	return v1.v === v2.v;
    }

    static check(v) {
	if (v instanceof Symbol === false) {
	    throw new Error("given value is not symbol");
	}
    }
}

class Values extends Primitive {
    constructor(vals) {
	super();
	this.v = vals;
    }

    toString() {
	throw new Error("Not Implemented");
    }

    toRawString() {
	return this.toString();
    }

    getAt(i) {
	return this.v[i];
    }

    getAll() {
	return this.v;
    }

    static make() {
	return new Values(arguments);
    }
    
    static check(v1) {
	if (v instanceof Values === false) {
	    throw new Error("given value is not a values")
	}
    }

    static isEqual(v1, v2) {
	throw new Error("Not Implemented");
    }
}

const Empty = [];

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
	    if (rest instanceof Pair) {
		var hd = rest.hd;
		result += toString(hd);
	    } else {
		result += " . " + toString(rest);
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
	return this.toString();
    }

    static make(hd, tl) {
	return new Pair(hd, tl);
    }

    static check(v) {
	if (v instanceof Pair === false) {
	    throw new Error("given value is not a Pair");
	}
    }

    static isEqual(v1, v2) {
	if (v1 instanceof Pair === false || v2 instanceof Pair === false) {
	    return false;
	}
	
	var hd1 = v1.hd;
	var tl1 = v1.tl;
	var hd2 = v2.hd;
	var tl2 = v2.tl;

	while (true) {
	    if (hd1.equals(h2)) {
		return isEqual(tl1, tl2);
	    } else {
		return false;
	    }
	}

	return true;
    }
}

function makeList() {
    var len = arguments.length - 1;
    var result = Empty; /* TODO: wrap this? */
    while (len >= 0) {
	result = Pair.make(arguments[len--], result);
    }
    return result;
}

function array_to_list(lst) {
    return makeList.apply(null, lst);
}

function list_to_array(lst) {
    var r = [];
    while (!isEmpty(lst)) {
        r.push(lst.car());
        lst = lst.cdr();
    }
    return r;
}

class Vector extends Primitive {
    constructor(items, mutable) {
	super();
	this.mutable = mutable;
	this.items = items;
    }

    isMutable() {
	return this.mutable;
    }

    ref(n) {
	if (n < 0 || n > this.items.length) {
	    throw new Exception("vector-ref: index out of range");
	}

	return this.items[n];
    }

    set(n, v) {
	if (n < 0 || n > this.items.length) {
	    throw new Exception("vector-set: index out of range");
	} else if (!this.mutable) {
	    throw new Exception("vector-set: immutable vector");
	}
	this.items[n] = v;
    }

    length() {
	return this.size;
    }

    toString() {
	var items = "";
	for (var i = 0; i < this.items.length; i++) {
	    items += this.items[i].toString();
	    if (i != this.items.length - 1) {
		items += " ";
	    }
	}
	return "#(" + items + ")";
    }

    toRawString() {
	return this.toString();
    }

    static make(items, mutable) {
	return new Vector(items, mutable);
    }
}

function makeVector(size, init) {
    var r = new Array(size);
    return r.fill(init);
}

class Struct extends Primitive {
    constructor(name, fields) {
	super();
	this.name = name;
	this.fields = [];
	for (var i = 0; i < fields.length; i++) {
	    this.fields.push(fields[i]);
	}
    }

    toString() {
	var fields = "";
	for (var i = 0; i < this.fields.length; i++) {
	    fields += this.fields[i].toString();
	    if (i != this.fields.length - 1) {
		fields += " ";
	    }
	}
	return "(struct:" + this.name + " " + fields + ")";
    }

    toRawString() {
	return this.toString();
    }

    getName() {
	return this.name;
    }

    getField(n) {
	if (n >= this.fields.length) {
	    throw new Error("TypeError: invalid field at position " + n);
	}
	return this.fields[n];
    }
}

function makeStructType(options) {
    var constructor = function() {
	if (arguments.length != options.init_field_cnt) {
	    throw new Error("Error: invalid number of arguments");
	}
	return new Struct(options.name, arguments);
    }

    var predicate = function(v) {
	return v.getName() == options.name;
    }

    var accessor = function(s, pos) {
	return s.getField(pos);
    }

    var mutator = function(s, pos) {
	throw new Error("Error: Not implemented error");
    }

    var desc = "struct:" + options.name;

    return new Values([desc, constructor, predicate, accessor, mutator])
}

/******* Numbers *******/

var Number = {
    add: function() {
	return [].reduce.call(arguments, function(a, b) { return a + b });
    },
    sub: function() {
	return [].reduce.call(arguments, function(a, b) { return a - b }, 0);
    },
    mul: function() {
	return [].reduce.call(arguments, function(a, b) { return a * b }, 1);
    },
    div: function() {
	return [].reduce.call(arguments, function(a, b) { return a / b }, 1);
    },
    compare: function(cmp, operands) {
	if (operands.length < 2) {
	    throw new Error("Error: atleast 2 arguments required");
	}
	for (var i = 1; i < operands.length; i++) {
	    if (!cmp(operands[i - 1], operands[i])) {
		return false;
	    }
	}
	return true;
    },
    lt: function() {
	return Number.compare(function(a, b) { return a < b }, arguments)
    },
    lte: function() {
	return Number.compare(function(a, b) { return a <= b }, arguments)
    },
    gt: function() {
	return Number.compare(function(a, b) { return a > b }, arguments)
    },
    gte: function() {
	return Number.compare(function(a, b) { return a >= b }, arguments)
    },
    equal: function() {
	return Number.compare(function(a, b) { return a === b }, arguments)
    }
}

/******** Some polymorphic functions for convenience ********/

function toString(v) {
    if (v instanceof Primitive) {
	return v.toString();
    } else {
	return "" + v; //TODO: Figure out better way to do this? 
    }
}

function isEqual(v1, v2) {
    if (v instanceof Primitve) {
	return v1.equals(v2);
    } else {
	return v1 === v2;
    }
}

function arguments_to_array(a) {
    // arguments are not exactly array
    return (a.length === 1 ? [a[0]] : Array.apply(null, a));
}

function arguments_slice(a, i) {
    return [].slice.call(a, i);
}

export {
    Symbol,
    Values,
    Empty,
    isEmpty,
    Pair,
    Vector,
    makeVector,
    makeList,
    Number,
    toString,
    isEqual,
    arguments_to_array,
    arguments_slice,
    array_to_list,
    list_to_array,
    makeStructType
}
