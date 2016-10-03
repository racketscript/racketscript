export function raise(exp, ...args) {
    throw exp.apply(this, args);
}

export function truthy(val, exp, msg="") {
    if (val !== true) {
	raise(exp, msg);
    }
    return true;
}

export function falsy(val, exp, msg="") {
    return truthy(val === false, exp, msg);
}

export function type(val, type, msg="") {
    if  (val instanceof type) {
	return true;
    }
    raise(TypeError, msg + "(" + val + " : " + typeof(val) + " != " + type.name + ")");
}

export function eq(val1, val2, exp, msg) {
    if (val1 !== val2) {
	raise(exp, msg);
    }
    return true;
}
