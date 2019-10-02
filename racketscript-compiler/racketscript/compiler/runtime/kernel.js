import * as Core from './core.js';
import * as Paramz from './paramz.js';
import { MiniNativeOutputStringPort } from './core/mini_native_output_string_port.js';
import { displayNativeString, writeNativeString, printNativeString } from './core/print_native_string.js';

/* --------------------------------------------------------------------------*/
// Immutable

export function isImmutable(v) {
    if (Core.Primitive.check(v)) {
        return v.isImmutable();
    } else if (Core.Bytes.check(v) || typeof v === 'string') {
        return true;
    } else if (typeof v === 'number' || typeof v === 'boolean' ||
        typeof v === 'undefined' || v === null) {
        return false;
    }
    throw Core.racketCoreError('isImmutable not implemented for', v);
}

/* --------------------------------------------------------------------------*/
// String construction and manipulation

// fprintf forms that do not require an argument.
const NO_ARG_FORM_RE = /^~[\s~n%]/;

export function fprintf(isPrintAsExpression, out, form, ...args) {
    // TODO: Missing forms: ~.[asv], ~e, ~c.
    // TODO: The ~whitespace form should match Unicode whitespace.
    const regex = /~(?:[aAeEsSvVbBoOxX~n%]|\s+)/g;
    const formStr = form.toString();
    let reExecResult;
    let currentMatchIndex = 0;
    let prevIndex = 0;
    let lastMatch = '';

    const matches = formStr.match(regex);
    const numExpected = matches ?
        matches.filter(m => !NO_ARG_FORM_RE.test(m)).length : 0;
    if (numExpected !== args.length) {
        throw Core.racketContractError(`fprintf: format string requires ${numExpected} arguments, ` +
            `given ${args.length}; arguments were:`, out, form, ...args);
    }

    // eslint-disable-next-line no-cond-assign
    while ((reExecResult = regex.exec(formStr)) !== null) {
        Core.display(out, formStr.slice(prevIndex + lastMatch.length, reExecResult.index));
        prevIndex = reExecResult.index;
        lastMatch = reExecResult[0]; // eslint-disable-line prefer-destructuring
        if (/^~\s/.test(lastMatch)) continue; // eslint-disable-line no-continue
        switch (lastMatch.charAt(1)) { // eslint-disable-line default-case
        case '~':
            Core.display(out, '~');
            continue; // eslint-disable-line no-continue
        case 'n':
        case '%':
            Core.display(out, '\n');
            continue; // eslint-disable-line no-continue
        }
        const v = args[currentMatchIndex];
        currentMatchIndex += 1;
        switch (lastMatch.charAt(1)) {
        case 'a':
        case 'A':
            Core.display(out, v);
            break;
        case 'e': // FIXME: should use error-value->string-handler
        case 'E':
            Core.display(out, v);
            break;
        case 's':
        case 'S':
            Core.write(out, v);
            break;
        case 'v':
        case 'V':
            Core.print(out, v, isPrintAsExpression, 0);
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
            throw Core.racketContractError('Unsupported format:', lastMatch);
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
    const strOut = Core.Ports.openOutputString();
    fprintf(strOut, form, ...args);
    return Core.Ports.getOutputString(strOut);
}

/**
 * @param {!Core.Pair.Pair} charsList a list of chars
 * @return {!Core.UString.MutableUString}
 */
export function listToString(charsList) {
    return Core.UString.makeMutableFromChars(Core.Pair.listToArray(charsList));
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
            throw Core.racketCoreError(`${firstArg.toString()}:`, ...rest);
        }
    } else if (Core.UString.check(firstArg) || typeof firstArg === 'string') {
        throw Core.racketCoreError(firstArg.toString(), ...rest);
    } else {
        throw Core.racketContractError('error: invalid arguments');
    }
}

/**
 * @param {Core.Symbol|Core.UString|String} name
 * @param {Core.Symbol|Core.UString|String} expected
 * @param {*[]} rest
 */
// analogous to Racket raise-argument-error
export function argerror(name, expected, ...rest) {
    // duplicates continuation-mark-set-first
    const markset = Core.Marks.getContinuationMarks();
    const marks = Core.Marks.getMarks(markset, Paramz.ExceptionHandlerKey);
    var theerr;
    if (Core.Symbol.check(name)) {
        if (rest.length === 0) {
            theerr = Core.racketContractError(name.toString());
        } else {
            theerr = Core.makeContractError(name, expected, ...rest);
        }
    } else if (Core.UString.check(name) || typeof name === 'string') {
        theerr = Core.racketContractError(name.toString(), ...rest);
    } else {
        theerr = Core.racketContractError('error: invalid arguments');
    }

    if (marks.length === 0) {
        throw theerr;
    } else {
        marks.hd(theerr);
    }
}

/**
 * @param {Core.Symbol|Core.UString|String} name
 * @param {Core.Symbol|Core.UString|String} expected
 * @param {*[]} rest
 */
// analogous to Racket raise-mismatch-error
// usage: raise-mismatch-error name, (~seq msg v ...) ...
// so ...rst might have additional msg, v ...
export function mismatcherror(name, msg, ...rest) {
    // duplicates continuation-mark-set-first
    const markset = Core.Marks.getContinuationMarks();
    const marks = Core.Marks.getMarks(markset, Paramz.ExceptionHandlerKey);
    var theerr;
    if (Core.Symbol.check(name) || Core.UString.check(msg)) {
        if (rest.length === 0) {
            theerr = Core.racketContractError(name.toString(), msg);
        } else {
            const stringOut = new MiniNativeOutputStringPort();
            stringOut.consume(`${name.toString()}: `);
            stringOut.consume(msg);
            for (let i = 0; i < rest.length; i++) {
                //string indicates another "msg" format str, see usage above
                if (Core.UString.check(rest[i])) {
                    stringOut.consume(rest[i]);
                } else {
                    printNativeString(stringOut, rest[i], true, 0);
                }
            }
            theerr = Core.racketContractError(stringOut.getOutputString());
        }
    } else {
        theerr = Core.racketContractError('error: invalid arguments');
    }

    if (marks.length === 0) {
        throw theerr;
    } else {
        marks.hd(theerr);
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
        }
        error('random: argument should be positive');

    case 2:
        if (args[0] > 0 && args[1] > args[0]) {
            return Math.floor(args[0] + Math.random() * (args[1] - args[0]));
        }
        error('random: invalid arguments');

    default:
        error('random: invalid number of arguments');
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
    const arr = Core.Pair.listToArray(lst);
    const x2i = new Map();
    arr.forEach((x, i) => { x2i.set(x, i); });
    const srted = arr.sort((x, y) => {
        if (cmp(x, y)) {
            return -1;
        } else if (cmp(y, x)) {
            return 1;
        } // x = y, simulate stable sort by comparing indices
        return x2i.get(x) - x2i.get(y);
    });

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
