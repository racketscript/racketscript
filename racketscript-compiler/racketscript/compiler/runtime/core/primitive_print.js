// Primitive printing extensions are defined in a separate file
// to avoid circular dependency with UString and Ports.

import {Primitive} from './primitive.js';
import * as Ports from './ports.js';
import * as UString from './unicode_string.js';

/**
 * Writes a String representation similar to Racket's `display` to the given port.
 *
 * @param {!Ports.NativeStringOutputPort} out
 */
Primitive.prototype.displayNativeString = function(out) {
    throw new Error("Unimplemented: displayNativeString");
};

/**
 * Writes a UString representation similar to Racket's `display` to the given port.
 *
 * @param {!Ports.UStringOutputPort} out
 */
Primitive.prototype.displayUString = function(out) {
    const stringOut = new Ports.NativeOutputStringPort();
    this.displayNativeString(stringOut);
    out.consume(UString.makeMutable(stringOut.getOutputString()));
};

/**
 * Writes a string representation that can be read by Racket's `read` to the given port.
 *
 * @param {!Ports.NativeStringOutputPort} out
 */
Primitive.prototype.writeNativeString = function(out) {
    this.displayNativeString(out);
};

/**
 * Writes a UString representation that can be read by Racket's `read` to the given port.
 *
 * @param {!Ports.UStringOutputPort} out
 */
Primitive.prototype.writeUString = function(out) {
    const stringOut = new Ports.NativeOutputStringPort();
    this.writeNativeString(stringOut);
    out.consume(UString.makeMutable(stringOut.getOutputString()));
};

/**
 * Writes a string representation similar to Racket's `print` to the given port.
 *
 * @param {!Ports.NativeStringOutputPort} out
 */
Primitive.prototype.printNativeString = function(out) {
    out.consume("'");
    this.writeNativeString(out);
};

const PRINT_PREFIX_USTRING = UString.makeInternedImmutable("'");

/**
 * Writes a UString representation similar to Racket's `print` to the given port.
 *
 * @param {!Ports.UStringOutputPort} out
 */
Primitive.prototype.printUString = function(out) {
    out.consume(PRINT_PREFIX_USTRING);
    this.writeUString(out);
};


/**
 * @return {!String} a string representation similar to Racket's `display`.
 */
Primitive.prototype.toString = function() {
    const out = new Ports.NativeOutputStringPort();
    this.displayNativeString(out);
    return out.getOutputString();
};
