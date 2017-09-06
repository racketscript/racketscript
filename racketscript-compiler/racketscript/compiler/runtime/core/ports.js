import {PrintablePrimitive} from './printable_primitive.js';

/**
 * @abstract
 */
class Port extends PrintablePrimitive {
    isOutputPort() {
        return false;
    }

    isInputPort() {
        return false;
    }
}

/**
 * @abstract
 * @api private
 */
export class OutputPort extends Port {
    isOutputPort() {
        return true;
    }

    /**
     * @param {function(String)} out
     */
    displayNativeString(out) {
        out(`#<output-port:${this.name}>`);
    }
}

export function check(v) {
    return v instanceof Port;
}

export function isInputPort(v) {
    return check(v) && v.isInputPort();
}

export function isOutputPort(v) {
    return check(v) && v.isOutputPort();
}

/**
 * @abstract
 */
export class NativeStringOutputPort extends OutputPort {
    /**
     * @abstract
     */
    consume(nativeString) {
        throw new Error('Unimplemented');
    }

    isUStringPort() {
        return false;
    }
}

// Only consumes output via the given `consumeFn` when encountering a newline,
// othewise buffers the output.
// Writes *native* strings to the output.
class NewlineFlushingOutputPort extends NativeStringOutputPort {
    /**
     * @param {function(String)} consumeFn
     * @param {!String} name
     */
    constructor(consumeFn, name) {
        super();
        this._buffer = [];
        this._consumeFn = consumeFn;
        this.name = name;
    }

    /**
     * @param {!String} nativeString
     */
    consume(nativeString) {
        const lastNewlineIndex = nativeString.lastIndexOf('\n');
        if (lastNewlineIndex >= 0) {
            this._buffer.push(nativeString.slice(0, lastNewlineIndex));
            this._consumeFn(this._buffer.join(''));
            const restChars = nativeString.slice(lastNewlineIndex + 1);
            this._buffer = [];
            if (restChars !== '') {
                this._buffer.push(restChars);
            }
        } else {
            this._buffer.push(nativeString);
        }
    }

    /**
     * @param {function(String)} out
     */
    display(out) {
        out(`#<output-port:${this.name}>`);
    }
}

export const standardOutputPort = new NewlineFlushingOutputPort((str) => console.log(str), 'stdout');
export let currentOutputPort = standardOutputPort;

export const standardErrorPort = new NewlineFlushingOutputPort((str) => console.log(str), 'stderr');
export let currentErrorPort = standardErrorPort;

/**
 * Like {OutputStringPort}, but returns native Strings instead of UStrings.
 */
export class NativeOutputStringPort extends OutputPort {
    constructor() {
        super();
        this._buffer = [];
    }

    /**
     * @param {!String} s
     */
    consume(s) {
        this._buffer.push(s);
    }

    /**
     * @return {!String}
     */
    getOutputString() {
        if (this._buffer.length === 0) {
            return "";
        }
        if (this._buffer.length > 1) {
            // Concat vs Array#join:
            // https://jsperf.com/arr-join-vs-string-prototype-concat-apply-arr
            this._buffer = ["".concat(...this._buffer)];
        }
        return this._buffer[0];
    }

    get name() {
        return 'js-string';
    }
}
