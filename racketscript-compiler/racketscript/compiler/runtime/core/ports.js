import {Primitive} from "./primitive.js";
import * as $ from "./lib.js";
import * as UString from "./unicode_string.js";

class AbstractPort extends Primitive {
    isOutputPort() {
        return false;
    }

    isInputPort() {
        return false;
    }
}

class AbstractOutputPort extends AbstractPort {
    write() {
        throw new Error(`Expected ${this.constructor.name} to implement write(chars)`)
    }

    isOutputPort() {
        return true;
    }
}

export function isPort(v) {
    // When compiling with traceur, the `instanceof` check would fail,
    // due to a limitation of traceur.
    // TODO: Replace with `v instanceof AbstractPort` once this is fixed.
    return v && (
        v.constructor === NewlineFlushingOutputPort ||
        v.constructor === OutputStringPort);
}

export function isInputPort(v) {
    return isPort(v) && v.isInputPort();
}

export function isOutputPort(v) {
    return isPort(v) && v.isOutputPort();
}

// Only writes output via the given `writeFn` when encountering a newline,
// othewise buffers the output.
// Writes *native* strings to the output.
class NewlineFlushingOutputPort extends AbstractOutputPort {
    constructor(writeFn) {
        super();
        this._buffer = [];
        this._writeFn = writeFn;
    }

    write(datum) {
        const nativeString = $.toString(datum);
        const lastNewlineIndex = nativeString.lastIndexOf('\n');
        if (lastNewlineIndex >= 0) {
            this._buffer.push(nativeString.slice(0, lastNewlineIndex));
            this._writeFn(this._buffer.join(''));
            const restChars = nativeString.slice(lastNewlineIndex + 1);
            this._buffer = [];
            if (restChars !== '') {
                this._buffer.push(restChars);
            }
        } else {
            this._buffer.push(nativeString);
        }
    }
}

export const standardOutputPort = new NewlineFlushingOutputPort((str) => console.log(str));
export let currentOutputPort = standardOutputPort;

export const standardErrorPort = new NewlineFlushingOutputPort((str) => console.log(str));
export let currentErrorPort = standardErrorPort;


class OutputStringPort extends AbstractOutputPort {
    constructor() {
        super();
        this.__buffer = [];
    }

    write(datum) {
        this.__buffer.push(UString.toUString(datum));
    }

    /**
     * @return {!UString.UString}
     */
    getOutputString() {
        if (this.__buffer.length === 0) {
            return UString.makeMutable('');
        }
        if (this.__buffer.length > 1) {

            this.__buffer = [UString.stringAppend(...this.__buffer)];
        }
        return UString.copyAsMutable(this.__buffer[0]);
    }
}

export function openOutputString() {
    return new OutputStringPort();
}

export function getOutputString(outputStringPort) {
    return outputStringPort.getOutputString();
}
