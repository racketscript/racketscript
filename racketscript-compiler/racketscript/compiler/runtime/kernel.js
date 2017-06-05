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

export function display(v, out) {
    /* TODO: this is still line */
    out = out || Core.Ports.standardOutputPort;
    if (v === true) {
	out.write("#t");
    } else if (v === false) {
	out.write("#f");
    } else if (v === undefined || v === null) {
	out.write("#<void>");
    } else if (isBytes(v)) {
	out.write(utf8ToString(v));
    } else {
	out.write(Core.toString(v));
    }
}

export function print(v, out) {
    /* TODO: this is still line */
    out = out || Core.Ports.standardOutputPort;
    if (v === true) {
	out.write("#t");
    } else if (v === false) {
	out.write("#f");
    } else if (v === undefined || v === null) {
	out.write("#<void>");
    } else if (isBytes(v)) {
	out.write(utf8ToString(v));
    } else {
	out.write(Core.toString(v));
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
