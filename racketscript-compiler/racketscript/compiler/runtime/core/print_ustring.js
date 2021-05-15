import * as Primitive from './primitive.js';
import * as Bytes from './bytes.js';
import * as Procedure from './procedure.js';
import * as UString from './unicode_string.js';

const TRUE_USTRING = UString.makeInternedImmutable('#t');
const FALSE_USTRING = UString.makeInternedImmutable('#f');
const VOID_USTRING = UString.makeInternedImmutable('#<void>');

/**
 * @param {!Ports.UStringOutputPort} out
 * @param {!(Primitive.Primitive|Uint8Array|Function|String|number|boolean|undefined|null)} v
 */
export function displayUString(out, v) {
    if (v === true) {
        out.consume(TRUE_USTRING);
    } else if (v === false) {
        out.consume(FALSE_USTRING);
    } else if (v === undefined || v === null) {
        out.consume(VOID_USTRING);
    } else if (typeof v === 'number' || typeof v === 'string') {
        out.consume(UString.makeMutable(v.toString()));
    } else if (Primitive.check(v)) {
        v.displayUString(out);
    } else if (Bytes.check(v)) {
        out.consume(UString.makeMutable(Bytes.toString(v)));
    } else if (Procedure.check(v)) {
        if (v.__rjs_struct_object) {
            v.__rjs_struct_object.displayUString(out);
        } else {
            out.consume(UString.makeMutable(Procedure.toString(v)));
        }
    } else /* if (typeof v === 'number' || typeof v === 'string') */ {
        out.consume(UString.makeMutable(v.toString()));
    }
}

/**
 * @param {!Ports.UStringOutputPort} out
 * @param {!(Primitive.Primitive|Uint8Array|Function|String|number|boolean|undefined|null)} v
 */
export function writeUString(out, v) {
    if (Primitive.check(v)) {
        // Assume `v` implements Printable, as only Values does not,
        // and it cannot be passed here.
        v.writeUString(out);
    } else {
        displayUString(out, v);
    }
}

/**
 * @param {!Ports.UStringOutputPort} out
 * @param {!(Primitive.Primitive|Uint8Array|Function|String|number|boolean|undefined|null)} v
 * @param {!boolean} printAsExpression
 * @param {!(0|1)} quoteDepth
 */
export function printUString(out, v, printAsExpression, quoteDepth) {
    if (printAsExpression && quoteDepth !== 1 && Primitive.check(v)) {
        v.printUString(out);
    } else {
        writeUString(out, v);
    }
}
