import * as $ from "./lib.js";

/* Arithmatic */

export function add() {
    return [].reduce.call(arguments, function(a, b) {
	return a + b
    });
}

export function sub() {
    if (arguments.length === 1) {
	return -arguments[0];
    } else {
	let result = arguments[0];
	for (var i = 1; i < arguments.length; ++i) {
	    result -= arguments[i];
	}
	return result;
    }
}

export function mul() {
    return [].reduce .call(arguments, function(a, b) {
	return a * b
    }, 1);
}

export function div() {
    if (arguments.length === 1) {
	return 1 / arguments[0];
    } else {
	var result = arguments[0];
	for (var i = 1; i < arguments.length; ++i) {
	    result /= arguments[i];
	}
	return result;
    }
}

/* Comparision */

export function compare(cmp, operands) {
    if (operands.length < 2) {
	throw new $.RacketCoreError("compare {0}",
				    "atleast 2 arguments required");
    }
    for (var i = 1; i < operands.length; i++) {
	if (!cmp(operands[i - 1], operands[i])) {
	    return false;
	}
    }
    return true;
}

export function lt() {
    return compare(function(a, b) { return a < b }, arguments)
}

export function lte() {
    return compare(function(a, b) { return a <= b }, arguments)
}

export function gt() {
    return compare(function(a, b) { return a > b }, arguments)
}

export function gte() {
    return compare(function(a, b) { return a >= b }, arguments)
}

export function equals() {
    return compare(function(a, b) { return a === b }, arguments)
}

export function check(v) {
    return typeof v === 'number';
}
