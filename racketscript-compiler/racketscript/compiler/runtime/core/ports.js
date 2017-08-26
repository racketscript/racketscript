import {Primitive} from "./primitive.js";
import * as $ from "./lib.js";

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
class NewlineFlushingOutputPort extends AbstractOutputPort {
    constructor(writeFn) {
        super();
        this._buffer = [];
        this._writeFn = writeFn;
    }

    write(chars) {
        const lastNewlineIndex = chars.lastIndexOf("\n");
        if (lastNewlineIndex >= 0) {
            let flushchars = this._buffer.join("") + chars.slice(0, lastNewlineIndex);
            let restChars = chars.slice(lastNewlineIndex + 1);
            this._buffer = [];
            if (restChars !== "") {
                this._buffer.push(restChars);
            }
            this._writeFn(flushchars);
        } else {
            this._buffer.push(chars);
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
        this.__cachedString = null;
    }

    write(chars) {
        this.__buffer.push(chars);
        this.__cachedString = null;
    }

    getOutputString() {
        if (this.__buffer.length > 1) {
            this.__buffer = [this.__buffer.join('')];
        }
        return this.__buffer[0];
    }
}

export function openOutputString() {
    return new OutputStringPort();
}

export function getOutputString(outputStringPort) {
    return outputStringPort.getOutputString();
}
