// PrintablePrimitive subclass avoids circular dependency with UString and Ports.

import { Primitive } from './primitive.js';
import { MiniNativeOutputStringPort } from './mini_native_output_string_port.js';
import * as UString from './unicode_string.js';

const PRINT_PREFIX_USTRING = UString.makeInternedImmutable("'");

export class PrintablePrimitive extends Primitive /* implements Printable */ {
    /**
     * Writes a String representation similar to Racket's `display` to the given port.
     *
     * @param {!Ports.NativeStringOutputPort} out
     */
    /* abstract displayNativeString(out); */

    /**
     * Writes a UString representation similar to Racket's `display` to the given port.
     *
     * @param {!Ports.UStringOutputPort} out
     */
    displayUString(out) {
        const stringOut = new MiniNativeOutputStringPort();
        this.displayNativeString(stringOut);
        out.consume(UString.makeMutable(stringOut.getOutputString()));
    }

    /**
     * Writes a string representation that can be read by Racket's `read` to the given port.
     *
     * @param {!Ports.NativeStringOutputPort} out
     */
    writeNativeString(out) {
        this.displayNativeString(out);
    }

    /**
     * Writes a UString representation that can be read by Racket's `read` to the given port.
     *
     * @param {!Ports.UStringOutputPort} out
     */
    writeUString(out) {
        const stringOut = new MiniNativeOutputStringPort();
        this.writeNativeString(stringOut);
        out.consume(UString.makeMutable(stringOut.getOutputString()));
    }

    /**
     * Writes a string representation similar to Racket's `print` to the given port.
     *
     * @param {!Ports.NativeStringOutputPort} out
     */
    printNativeString(out) {
        out.consume("'");
        this.writeNativeString(out);
    }

    /**
     * Writes a UString representation similar to Racket's `print` to the given port.
     *
     * @param {!Ports.UStringOutputPort} out
     */
    printUString(out) {
        out.consume(PRINT_PREFIX_USTRING);
        this.writeUString(out);
    }

    /**
     * @return {!String} a string representation similar to Racket's `display`.
     */
    toString() {
        const out = new MiniNativeOutputStringPort();
        this.displayNativeString(out);
        return out.getOutputString();
    }
}
