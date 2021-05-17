import { displayNativeString, writeNativeString, printNativeString } from './print_native_string.js';
import { displayUString, writeUString, printUString } from './print_ustring.js';

// TODO: All of these functions can be migrated to kernel.rkt after fprintf is.

/**
 * Writes a string representation similar to Racket's `display` to the given port.
 *
 * @param {(!Ports.NativeStringOutputPort|!Ports.UStringOutputPort)} out
 * @param {!(Primitive.Primitive|Uint8Array|Function|String|number|boolean|undefined|null)} v
 */
export function display(out, v) {
    if (out.isUStringPort()) {
        displayUString(out, v);
    } else {
        displayNativeString(out, v);
    }
}

/**
 * Writes a string representation that can be read by Racket's `read` to the given port.
 *
 * @param {!Ports.NativeStringOutputPort} out
 * @param {!(Primitive.Primitive|Uint8Array|Function|String|number|boolean|undefined|null)} v
 */
export function write(out, v) {
    if (out.isUStringPort()) {
        writeUString(out, v);
    } else {
        writeNativeString(out, v);
    }
}

/**
 * Writes a string representation similar to Racket's `print` to the given port.
 *
 * @param {!Ports.NativeStringOutputPort} out
 * @param {!(Primitive.Primitive|Uint8Array|Function|String|number|boolean|undefined|null)} v
 * @param {!boolean} printAsExpression
 * @param {!(0|1)} quoteDepth
 */
export function print(out, v, printAsExpression, quoteDepth) {
    if (out.isUStringPort()) {
        printUString(out, v, printAsExpression, quoteDepth);
    } else {
        printNativeString(out, v, printAsExpression, quoteDepth);
    }
}
