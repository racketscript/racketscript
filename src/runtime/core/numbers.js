import {RacketCoreError} from "./error.js";

export default Number = {
    add: function() {
	return [].reduce.call(arguments, function(a, b) {
	    return a + b
	});
    },
    sub: function() {
	if (arguments.length === 1) {
	    return -arguments[0];
	} else {
	    let result = arguments[0];
	    for (var i = 1; i < arguments.length; ++i) {
		result -= arguments[i];
	    }
	    return result;
	}
    },
    mul: function() {
	return [].reduce .call(arguments, function(a, b) {
	    return a * b
	}, 1);
    },
    div: function() {
	if (arguments.length === 1) {
	    return 1 / arguments[0];
	} else {
	    var result = arguments[0];
	    for (var i = 1; i < arguments.length; ++i) {
		result /= arguments[i];
	    }
	    return result;
	}
    },
    compare: function(cmp, operands) {
	if (operands.length < 2) {
	    throw new RacketCoreError("compare",
				      "atleast 2 arguments required");
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
