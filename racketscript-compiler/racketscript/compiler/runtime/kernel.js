import * as Core from './core.js';
import * as Paramz from './paramz.js';

/* --------------------------------------------------------------------------*/
// Immutable

export function isImmutable(v) {
    if (Core.Primitive.check(v)) {
        return v.isImmutable();
    }
    if (Core.Bytes.check(v) || typeof v === 'string') {
        return true;
    }
    if (typeof v === 'number' || typeof v === 'boolean' || typeof v === 'undefined' || v === null) {
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
    const numExpected = matches
        ? matches.filter(m => !NO_ARG_FORM_RE.test(m)).length
        : 0;
    if (numExpected !== args.length) {
        throw Core.racketContractError(
            `fprintf: format string requires ${numExpected} arguments, `
            + `given ${args.length}; arguments were:`,
            out,
            form,
            ...args
        );
    }

    // eslint-disable-next-line no-cond-assign
    while ((reExecResult = regex.exec(formStr)) !== null) {
        Core.display(
            out,
            formStr.slice(prevIndex + lastMatch.length, reExecResult.index)
        );
        prevIndex = reExecResult.index;
        lastMatch = reExecResult[0]; // eslint-disable-line prefer-destructuring
        if (/^~\s/.test(lastMatch)) continue; // eslint-disable-line no-continue
        // eslint-disable-next-line default-case
        switch (
            lastMatch.charAt(1)
        ) {
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
 * @param {Core.PrimitiveSymbol|Core.UString|String} firstArg
 * @param {*[]} rest
 */
export function error(firstArg, ...rest) {
    if (Core.PrimitiveSymbol.check(firstArg)) {
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
 * @param {Core.Error} e
 */
// somewhat duplicates continuation-mark-set-first in kernel.rkt
// (but less general, eg ignore prompt tag for now);
// must be here to avoid circular dependency
export function doraise(e) {
    const markset = Core.Marks.getContinuationMarks();
    const marks = Core.Marks.getMarks(markset, Paramz.ExceptionHandlerKey);
    if (marks.length === 0) {
        throw e;
    } else {
        marks.hd(e);
    }
}

/**
 * @param {Core.PrimitiveSymbol} name
 * @param {Core.UString|String} expected
 * @param {*[]} rest
 */
// analogous to Racket raise-argument-error
// usage:
//  (raise-argument-error name expected arg)
//  (raise-argument-error name expected bad-pos arg ...)
export function argerror(name, expected, ...rest) {
    let theerr;
    if (
        Core.PrimitiveSymbol.check(name)
        && (Core.UString.check(expected) || typeof expected === 'string')
        && rest.length >= 1
    ) {
        theerr = Core.makeArgumentError(name, expected, ...rest);
    } else {
        theerr = Core.racketContractError('raise-argument-error: invalid arguments');
    }

    doraise(theerr);
}

/**
 * @param {Core.PrimitiveSymbol} name
 * @param {Core.UString|String} expected
 * @param {*[]} rest
 */
// analogous to Racket raise-result-error
// usage:
//  (raise-result-error name expected arg)
//  (raise-result-error name expected bad-pos arg ...)
export function resulterror(name, expected, ...rest) {
    let theerr;
    if (
        Core.PrimitiveSymbol.check(name)
        && (Core.UString.check(expected) || typeof expected === 'string')
        && rest.length >= 1
    ) {
        theerr = Core.makeResultError(name, expected, ...rest);
    } else {
        theerr = Core.racketContractError('raise-result-error: invalid result');
    }

    doraise(theerr);
}

/**
 * @param {Core.PrimitiveSymbol} name
 * @param {Core.UString|String} msg
 * @param {Core.UString|String} field
 * @param {*[]} rest
 */
// analogous to Racket raise-arguments-error,
// so rest must be at least 1 and must be odd bc each field must have matching v
export function argserror(name, msg, field, ...rest) {
    let theerr;
    if (
        Core.PrimitiveSymbol.check(name)
        && (Core.UString.check(msg) || typeof msg === 'string')
        && (Core.UString.check(field) || typeof field === 'string')
        && rest.length >= 1 && rest.length % 2 === 1
    ) {
        theerr = Core.makeArgumentsError(name, msg, field, ...rest);
    } else {
        theerr = Core.racketContractError('raise-arguments-error: invalid arguments');
    }

    doraise(theerr);
}

/**
 * @param {Core.PrimitiveSymbol} name
 * @param {Core.UString|String} msg
 * @param {*[]} rest
 */
// analogous to Racket raise-mismatch-error
// usage: raise-mismatch-error name, (~seq msg v ...) ...
// so ...rst might have additional msg, v ...
export function mismatcherror(name, msg, ...rest) {
    let theerr;
    if (
        Core.PrimitiveSymbol.check(name)
        && (Core.UString.check(msg) || typeof msg === 'string')
    ) {
        theerr = Core.makeMismatchError(name, msg, ...rest);
    } else {
        theerr = Core.racketContractError('error: invalid arguments');
    }

    doraise(theerr);
}

/**
 * @param {String} name
 * @param {String} type
 * @param {*} v
 * @param {!number} length
 * @param {!number} index
 */
// analogous to Racket raise-range-error
// usage: raise-range-error name, type, v len, i
export function outofrangeerror(name, type, v, len, i) {
    let theerr;
    if (
        typeof name === 'string'
        && typeof type === 'string'
        && typeof len === 'number'
        && typeof i === 'number'
    ) {
        theerr = Core.makeOutOfRangeError(name, type, v, len, i);
    } else {
        theerr = Core.racketContractError('error: invalid arguments');
    }

    doraise(theerr);
}

/* --------------------------------------------------------------------------*/
// Not Implemented/Unorganized/Dummies

export function random(...args) {
    switch (args.length) {
    case 0:
        return Math.random();
    case 1:
        if (args[0] > 0) {
            return Math.floor(Math.random() * args[0]);
        }
        error('random: argument should be positive');
        break;

    case 2:
        if (args[0] > 0 && args[1] > args[0]) {
            return Math.floor(args[0] + Math.random() * (args[1] - args[0]));
        }
        error('random: invalid arguments');
        break;

    default:
        error('random: invalid number of arguments');
        break;
    }
}

// TODO: add optional equal? pred

export function memv(v, lst) {
    while (Core.Pair.isEmpty(lst) === false) {
        if (Core.isEqv(v, lst.hd)) {
            return lst;
        }
        lst = lst.tl;
    }
    return false;
}

export function memq(v, lst) {
    while (Core.Pair.isEmpty(lst) === false) {
        if (Core.isEq(v, lst.hd)) {
            return lst;
        }
        lst = lst.tl;
    }
    return false;
}

export function memf(f, lst) {
    while (Core.Pair.isEmpty(lst) === false) {
        if (f(lst.hd)) {
            return lst;
        }
        lst = lst.tl;
    }
    return false;
}

export function findf(f, lst) {
    while (Core.Pair.isEmpty(lst) === false) {
        if (f(lst.hd)) {
            return lst.hd;
        }
        lst = lst.tl;
    }
    return false;
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
