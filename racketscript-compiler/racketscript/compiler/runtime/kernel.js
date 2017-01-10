import * as Core from "./core.js";


export function format(pattern, ...args) {
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

/* --------------------------------------------------------------------------*/
// Printing to Console

let __buffer = ""; //HACK

export function displayln(v) {
    /* TODO: Real thing takes port as well */
    if (v === true) {
	console.log(__buffer + "#t");
    } else if (v === false) {
	console.log(__buffer + "#f");
    } else if (v === undefined || v === null) {
	console.log(__buffer + "#<void>");
    } else if (isBytes(v)) {
	console.log(__buffer + utf8ToString(v));
    } else {
	console.log(__buffer + Core.toString(v));
    }
    __buffer = "";
}

export function display(v) {
    /* TODO: this is still line */
    if (v === true) {
	__buffer += "#t";
    } else if (v === false) {
	__buffer += "#f";
    } else if (v === undefined || v === null) {
	__buffer += "#<void>";
    } else if (isBytes(v)) {
	__buffer = __buffer + utf8ToString(v);
    } else {
	__buffer += Core.toString(v);
    }
}

/* --------------------------------------------------------------------------*/
// Errors

export function error(...args) {
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

export function random(...args) {
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

// TODO: add optional equal? pred

export function memv(v, lst) {
    while (Core.Pair.isEmpty(lst) == false) {
	if (Core.isEqv(v, lst.hd)) {
	    return lst;
	}
	lst = lst.tl;
	continue;
    }
    return false;
}

export function memq(v, lst) {
    while (Core.Pair.isEmpty(lst) == false) {
	if (Core.isEq(v, lst.hd)) {
	    return lst;
	}
	lst = lst.tl;
	continue;
    }
    return false;
}

export function memf(f, lst) {
    while (Core.Pair.isEmpty(lst) == false) {
	if (f(lst.hd)) {
	    return lst;
	}
	lst = lst.tl;
	continue;
    }
    return false;
}

export function findf(f, lst) {
    while (Core.Pair.isEmpty(lst) == false) {
	if (f(lst.hd)) {
	    return list.hd;
	}
	lst = lst.tl;
	continue;
    }
    return false;
}


// TODO: more faithfully reproduce Racket sort?
// this implements raw-sort from #%kernel, see racket/private/list
export function sort9(lst, cmp) {
    var arr = Core.Pair.listToArray(lst);
    var x2i = new Map();
    arr.forEach(function (x,i) { x2i.set(x,i); });
    var srted = arr.sort(function (x,y) {
	if (cmp(x,y)) {
	    return -1;
	} else if (cmp(y,x)) {
	    return 1;
	} else { // x = y, simulate stable sort by comparing indices
	    return x2i.get(x) - x2i.get(y);
	}});
 
    return Core.Pair.listFromArray(srted);
}

export function assv(k, lst) {
    while (Core.Pair.isEmpty(lst) === false) {
	if (Core.isEqv(k, lst.hd.hd)) {
	    return lst.hd;
	}
	lst = lst.tl;
    }
    return false;
}

export function assq(k, lst) {
    while (Core.Pair.isEmpty(lst) === false) {
	if (Core.isEq(k, lst.hd.hd)) {
	    return lst.hd;
	}
	lst = lst.tl;
    }
    return false;
}

export function assf(f, lst) {
    while (Core.Pair.isEmpty(lst) === false) {
	if (f(lst.hd.hd)) {
	    return lst.hd;
	}
	lst = lst.tl;
    }
    return false;
}

// Bytes (ported to kernel.rkt. Here for regexp stuff)

function isBytes(bs) {
    return bs instanceof Uint8Array;
}

function utf8ToString(bs) {
    if (!isBytes(bs)) {
    	throw Core.racketContractError("expected bytes");
    }
    return String.fromCharCode.apply(null,bs);
}

function stringToUtf8(str) {
    if (!(typeof str) == 'string') {
    	throw Core.racketContractError("expected string");
    }
     return new Uint8Array(Array.prototype.map.call(str,(x)=>x.charCodeAt(0)));
 }

// Regexp

// TODO: both regexps and pregexps currently compile to js regexps,
//       but js doesnt support posix patterns
export function isRegExp(x) {
    return x instanceof RegExp;
}

export function isPRegExp(x) {
    return x instanceof RegExp;
}

export function isByteRegExp(x) {
    return x instanceof RegExp;
}

export function isBytePRegExpx(x) {
    return x instanceof RegExp;
}

// TODO: support optional handler arg
export function regexp(str) {
    if ((typeof str) !== 'string') {
    	throw Core.racketContractError("expected string");
    }
    return new RegExp(str);
}

export function pregexp (str) {
    if ((typeof str) !== 'string') {
    	throw Core.racketContractError("expected string");
    }
    return new RegExp(str);
}

export function byteRegExp(bs) {
    if (isBytes(bs)) {
    	throw Core.racketContractError("expected bytes");
    }
    return new RegExp(utf8ToString(bs));
}

export function bytePRegExp(bs) {
    if (isBytes(bs)) {
    	throw Core.racketContractError("expected bytes");
    }
    return new RegExp(utf8ToString(bs));
}

export function regexpMatch(p, i) {
    var is_rx_p = isRegExp(p);
    var is_bytes_p = isBytes(p);
    var is_bytes_i = isBytes(i);
    var is_str_p = (typeof p) === 'string';
    var is_str_i = (typeof i) === 'string';

    if (!(is_rx_p || is_bytes_p || is_str_p) && !(is_bytes_i || is_str_i)) {
	throw Core.racketContractError("expected regexp, string or byte pat,"
				       + " and string or byte input");
    }
    var str = is_str_i ? i : utf8ToString(i);
    var pat = is_rx_p ? p : (is_str_p ? p : utf8ToString(p));
    var res = str.match(pat);
    if (res === null) return false;
    else if ((is_str_p || is_rx_p) && is_str_i) { // result as list of strs
	return Core.Pair.listFromArray(res.map((x)=>(x === undefined) ? false : x));
    } else { // result as list of bytes
	return Core.Pair.listFromArray(res.map((x)=>(x === undefined) ? false : stringToUtf8(x)));
    }
}
