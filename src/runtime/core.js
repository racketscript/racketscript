"use strict"

function TypeCheck(v, t) {
    if (v[0] !== t) {
	throw new Error("given value is not " + t);
    }
}

/******* Numbers *******/

export let Number = {
    add: function() {
	return [].reduce.call(arguments, function(a, b) { return a + b; });
    },
    subtract: function() {
	return [].reduce.call(arguments, function(a, b) { return a - b; });
    },
    multiply: function() {
	return [].reduce.call(arguments, function(a, b) { return a * b; });
    },
    divide: function() {
	return [].reduce.call(arguments, function(a, b) { return a / b; });
    }
}
    
/******* Symbols *******/

export function Symbol(v) {
    this.v = v;
}

Symbol.prototype.check = function(v) {
    if (v instanceof Symbol === false) {
	throw new Error("given value is not symbol");
    }
}

Symbol.prototype.equal = function(v1, v2) {
    /* NOTE: check if both are symbol? */
    return v1.v == v2.v;
}

Symbol.prototype.make = function(v) {
    return new Symbol(v);
}

/******* Values *******/

export function Values() {
    this.v = arguments
}

Values.prototype.check = function(v) {
    if (v instanceof Values === false) {
	throw new Error("not values");
    }
}

Values.prototype.make = function() {
    return new Values(arguments);
}

Values.prototype.get = function(i) {
    return this.v[i];
}

Values.prototype.getAll = function() {
    return this.v;
}

/******* Pairs *******/

export function Cons(hd, tl) {
    this.hd = hd;
    this.tl = tl;
}

Cons.prototype.empty = []

Cons.prototype.check = function(v) {
    if (v instanceof Cons === false) {
	throw new Error("not a pair");
    }
}

Cons.prototype.make = function(hd, tl) {
    return new Cons(hd, tl);
}

Cons.prototype.car = function() {
    return this.hd;
}

Cons.prototype.cdr = function() {
    return this.tl;
}

Cons.prototype.isEmpty = function(v) {
    return (v instanceof Array) && v.length === 0;
}

Cons.prototype.makeList = function() {
    var len = arguments.length;
    var result = []; /* TODO: wrap this? */
    while (--len) {
	result = Cons.make(arguments[len], result);
    }
    return result;
}

/******* Vector *******/
