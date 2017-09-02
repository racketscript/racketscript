import * as Core from "./core.js";

/* --------------------------------------------------------------------------*/
// Immutable

export function isImmutable(v) {
    if (Core.Primitive.check(v)) {
        return v.isImmutable();
    } else if (Core.Bytes.check(v)) {
        return true;
    } else {
        racketCoreError(`isImmutable not implemented for ${v}`);
    }
}

/* --------------------------------------------------------------------------*/
// String construction and manipulation

export function fprintf(out, form, ...args) {
    // TODO: Missing forms: ~.[asv], ~e, ~c.
    // TODO: The ~whitespace form should match Unicode whitespace.
    const regex = /~(?:[aAsSvVbBoOxX~n%]|\s+)/g;
    const formStr = form.toString();
    let reExecResult;
    let currentMatchIndex = 0;
    let prevIndex = 0;
    let lastMatch = '';
    while ((reExecResult = regex.exec(formStr)) !== null) {
        if (currentMatchIndex >= args.length) {
            throw Core.racketContractError('insufficient pattern arguments');
        }
        Core.display(out, formStr.slice(prevIndex + lastMatch.length, reExecResult.index));
        prevIndex = reExecResult.index;
        lastMatch = reExecResult[0];
        if (/^~\s/.test(lastMatch)) continue;
        switch (lastMatch.charAt[1]) {
            case '~':
                Core.display(out, '~');
                continue;
            case 'n':
            case '%':
                Core.display(out, '\n');
                continue;
        }
        const v = args[currentMatchIndex++];
        switch (lastMatch.charAt(1)) {
            case 'a':
            case 'A':
                Core.display(out, v);
                break;
            case 's':
            case 'S':
                Core.write(out, v);
                break;
            case 'v':
            case 'V':
                Core.print(out, v, Core.isPrintAsExpression(), 0);
                break;
            case 'b':
            case 'B':
                // TODO: raise exn:fail:contract if the number is not exact.
                Core.display(out, v.toString(2));
                break;
            case 'o':
            case 'O':
                // TODO: raise exn:fail:contract if the number is not exact.
                Core.display(out, v.toString(8));
                break;
            case 'x':
            case 'X':
                // TODO: raise exn:fail:contract if the number is not exact.
                Core.display(out, v.toString(16));
                break;
            default:
                throw racketContractError('Unsupported format:', lastMatch);
        }
    }
    if (lastMatch.length + prevIndex < form.length) {
        Core.display(out, formStr.slice(lastMatch.length + prevIndex));
    }
}

/**
 * @return {!Core.UString.UString}
 */
export function format(form, ...args) {
    const strOut = Core.openOutputString();
    fprintf(strOut, form, ...args);
    return Core.getOutputString(strOut);
}

/**
 * @param {!Core.Pair.Pair} charsList a list of chars
 * @return {!Core.UString.MutableUString}
 */
export function listToString(charsList) {
	return Core.UString.makeMutableFromChars(
        Core.Pair.listToArray(charsList));
}

/* --------------------------------------------------------------------------*/
// Errors

/**
 * @param {Core.Symbol|Core.UString|String} firstArg
 * @param {*[]} rest
 */
export function error(firstArg, ...rest) {
    if (Core.Symbol.check(firstArg)) {
        if (rest.length === 0) {
            throw Core.racketCoreError(firstArg.toString());
        } else {
            throw Core.racketCoreError(
                `${firstArg.toString()}:`, ...rest);
        }
    } else if (Core.UString.check(firstArg) || typeof firstArg === 'string') {
        throw Core.racketCoreError(firstArg.toString(), ...rest);
    } else {
        throw Core.racketContractError('error: invalid arguments');
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
    while (!Core.Pair.isEmpty(lst)) {
	if (Core.isEqv(v, lst.hd)) {
	    return lst;
	}
	lst = lst.tl;
	continue;
    }
    return false;
}

export function memq(v, lst) {
    while (!Core.Pair.isEmpty(lst)) {
	if (Core.isEq(v, lst.hd)) {
	    return lst;
	}
	lst = lst.tl;
	continue;
    }
    return false;
}

export function memf(f, lst) {
    while (!Core.Pair.isEmpty(lst)) {
	if (f(lst.hd) !== false) {
	    return lst;
	}
	lst = lst.tl;
	continue;
    }
    return false;
}

export function findf(f, lst) {
    while (!Core.Pair.isEmpty(lst)) {
	if (f(lst.hd) !== false) {
	    return lst.hd;
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
    while (!Core.Pair.isEmpty(lst)) {
	if (Core.isEqv(k, lst.hd.hd)) {
	    return lst.hd;
	}
	lst = lst.tl;
    }
    return false;
}

export function assq(k, lst) {
    while (!Core.Pair.isEmpty(lst)) {
	if (Core.isEq(k, lst.hd.hd)) {
	    return lst.hd;
	}
	lst = lst.tl;
    }
    return false;
}

export function assf(f, lst) {
    while (!Core.Pair.isEmpty(lst)) {
	if (f(lst.hd.hd) !== false) {
	    return lst.hd;
	}
	lst = lst.tl;
    }
    return false;
}
