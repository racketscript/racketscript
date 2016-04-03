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
	while (!isEmpty(rest)) {
	    if (rest instanceof Pair) {
		var hd = rest.hd;
		result += toString(hd) + " "
	    } else {
		result += " . " + toString(rest);
		break;
	    }
	    rest = rest.tl;
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

/******* Numbers *******/

var Number = {
    add: function() {
	return [].reduce.call(arguments, function(a, b) { return a + b });
    },
    subtract: function() {
	return [].reduce.call(arguments, function(a, b) { return a - b }, 0);
    },
    multiply: function() {
	return [].reduce.call(arguments, function(a, b) { return a * b }, 1);
    },
    divide: function() {
	return [].reduce.call(arguments, function(a, b) { return a / b }, 1);
    },
    compare: function(compare, operands) {
	if (operands.length < 2) {
	    throw new Error("Error: atleast 2 arguments required");
	}
	for (var i = 1; i < operands.length; i++) {
	    if (!compare(operands[i - 1], operands[i])) {
		return false;
	    }
	}
	return true;
    },
    lt: function() {
	return this.compare(function(a, b) { return a < b }, arguments)
    },
    lte: function() {
	return this.compare(function(a, b) { return a <= b }, arguments)
    },
    gt: function() {
	return this.compare(function(a, b) { return a > b }, arguments)
    },
    gte: function() {
	return this.compare(function(a, b) { return a >= b }, arguments)
    },
    equal: function() {
	return this.compare(function(a, b) { return a === b }, arguments)
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

export {
    Symbol,
    Values,
    Empty,
    isEmpty,
    Pair,
    makeList,
    Number,
    toString,
    isEqual
}
