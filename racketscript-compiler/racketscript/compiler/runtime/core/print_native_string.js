import * as Primitive from './primitive.js';
import * as Bytes from './bytes.js';
import * as Procedure from './procedure.js';

/**
 * @param {!Ports.NativeStringOutputPort} out
 * @param {!(Primitive.Primitive|Uint8Array|Function|String|number|boolean|undefined|null)} v
 */
export function displayNativeString(out, v) {
    if (v === true) {
        out.consume('#t');
    } else if (v === false) {
        out.consume('#f');
    } else if (v === undefined || v === null) {
        out.consume('#<void>');
    } else if (Primitive.check(v)) {
        v.displayNativeString(out);
    } else if (Bytes.check(v)) {
        Bytes.displayNativeString(out, v);
    } else if (Procedure.check(v)) {
        if (v.__rjs_struct_object) {
            v.__rjs_struct_object.displayNativeString(out);
        } else {
            Procedure.displayNativeString(out, v);
        }
    } else /* if (typeof v === 'number' || typeof v === 'string') */ {
        out.consume(v.toString());
    }
}

/**
 * @param {!Ports.NativeStringOutputPort} out
 * @param {!(Primitive.Primitive|Uint8Array|Function|String|number|boolean|undefined|null)} v
 */
export function writeNativeString(out, v) {
    if (Primitive.check(v)) {
        // Assume `v` implements Printable, as only Values does not,
        // and it cannot be passed here.
        v.writeNativeString(out);
    } else {
        displayNativeString(out, v);
    }
}


/**
 * @param {!Ports.NativeStringOutputPort} out
 * @param {!(Primitive.Primitive|Uint8Array|Function|String|number|boolean|undefined|null)} v
 * @param {!boolean} printAsExpression
 * @param {!(0|1)} quoteDepth
 */
export function printNativeString(out, v, printAsExpression, quoteDepth) {
    if (printAsExpression && quoteDepth !== 1 && Primitive.check(v)) {
        v.printNativeString(out);
    } else if (Bytes.check(v)) {
        Bytes.printNativeString(out, v);
    } else {
        writeNativeString(out, v);
    }
}
