import { MiniNativeOutputStringPort } from './mini_native_output_string_port.js';
import { printNativeString } from './print_native_string.js';

function printError(out, msg, args) {
    out.consume(msg);
    for (const arg of args) {
        out.consume(' ');
        if (typeof arg === 'string') {
            out.consume(arg);
        } else {
            printNativeString(out, arg, /* printAsExpression */ true, /* quoteDepth */ 0);
        }
    }
}

function makeError(name) {
    /**
     * The "(error msg v ...)" form.
     * Besides Racket values, also allows native strings.
     */
    const e = function (msg, ...args) {
        this.name = name;

        const stringOut = new MiniNativeOutputStringPort();
        printError(stringOut, msg, args);
        this.message = stringOut.getOutputString();

        this.stack = (new Error()).stack;
        if (Error.captureStackTrace) {
            Error.captureStackTrace(this, this.constructor);
        } else {
            this.stack = (new Error()).stack;
        }
    };
    e.prototype = Object.create(Error.prototype);
    e.prototype.constructor = e;

    return (...args) =>
        new (Function.prototype.bind.apply(e, [this].concat(args)))();
}

export const racketCoreError = makeError('RacketCoreError');
export const racketContractError = makeError('RacketContractError');

export function isContractErr(e) {
    const nm = e.name;
    return nm !== undefined && nm === "RacketContractError";
}
export function errName(e) { return e.name; }
export function errMsg(e)  { return e.message; }
