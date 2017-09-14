/**
 * A simplified internal-only version of Ports.NativeOutputStringPort.
 *
 * This can be used where dependency on Ports is not possible,
 * e.g. because Ports are Printable,
 * and provides a faster no-frills internal implementation.
 */
export class MiniNativeOutputStringPort {
    constructor() {
        this._buffer = [];
    }

    /**
     * @param {String} nativeString
     */
    consume(nativeString) {
        this._buffer.push(nativeString);
    }

    /**
     * @return {String} nativeString
     */
    getOutputString() {
        return this._buffer.join('');
    }
}
