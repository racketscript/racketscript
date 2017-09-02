// UString ports are defined in a separate module to avoid circular dependency
// of `errors.js` with `ports.js` via `unicode_string.js`.

import {OutputPort} from './ports.js';
import * as UString from './unicode_string.js';

class UStringOutputPort extends OutputPort {
    isUStringPort() {
        return true;
    }
}

class OutputStringPort extends UStringOutputPort {
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
}

/**
 * @return {!OutputStringPort}
 */
export function openOutputString() {
    return new OutputStringPort();
}

/**
 *
 * @param {!OutputStringPort} outputStringPort
 */
export function getOutputString(outputStringPort) {
    return outputStringPort.getOutputString();
}
