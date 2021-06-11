import { PrintablePrimitive } from './printable_primitive.js';
import * as UString from './unicode_string.js';

/** @abstract */
class Port extends PrintablePrimitive {
    isOutputPort() {
        return false;
    }

    isInputPort() {
        return false;
    }

    isStringPort() {
        return false;
    }
}


/** @abstract */
class InputPort extends PrintablePrimitive {
    isInputPort() {
        return true;
    }
}

/**
 * @abstract
 * @api private
 */
class OutputPort extends Port {
    isOutputPort() {
        return true;
    }

    /**
     * @param {!NativeOutputStringPort} out
     */
    displayNativeString(out) {
        out.consume(`#<output-port:${this.name}>`);
    }

    /** @abstract isUStringPort(): boolean; */
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

export function isStringPort(v) {
    return check(v) && v.isStringPort();
}

// Only consumes output via the given `consumeFn` when encountering a newline,
// othewise buffers the output.
// Writes *native* strings to the output.
class NewlineFlushingOutputPort extends OutputPort {
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

    isUStringPort() {
        return false;
    }
}

// eslint-disable-next-line no-console
export const standardOutputPort = new NewlineFlushingOutputPort(str => console.log(str), 'stdout');
export const standardInputPort = new InputPort();
// eslint-disable-next-line no-console
export const standardErrorPort = new NewlineFlushingOutputPort(str => console.log(str), 'stderr');

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
            return '';
        }
        if (this._buffer.length > 1) {
            // Concat vs Array#join:
            // https://jsperf.com/arr-join-vs-string-prototype-concat-apply-arr
            this._buffer = [''.concat(...this._buffer)];
        }
        return this._buffer[0];
    }

    get name() {
        return 'js-string';
    }

    isStringPort() {
        return true;
    }

    isUStringPort() {
        return false;
    }
}

class OutputStringPort extends OutputPort {
    constructor() {
        super();
        this._buffer = [];
    }

    /**
     * @param {!UString.UString} s
     */
    consume(s) {
        this._buffer.push(s);
    }

    /**
     * @return {!UString.UString}
     */
    getOutputString() {
        if (this._buffer.length === 0) {
            return UString.makeMutable('');
        }
        if (this._buffer.length > 1) {
            this._buffer = [UString.stringAppend(...this._buffer)];
        }
        return UString.copyAsMutable(this._buffer[0]);
    }

    get name() {
        return 'string';
    }

    isStringPort() {
        return true;
    }

    isUStringPort() {
        return true;
    }
}

/**
 * @return {!OutputStringPort}
 */
export function openOutputString() {
    return new OutputStringPort();
}

/**
 * @param {!OutputStringPort} outputStringPort
 * @return {!UString.UString}
 */
export function getOutputString(outputStringPort) {
    return outputStringPort.getOutputString();
}
