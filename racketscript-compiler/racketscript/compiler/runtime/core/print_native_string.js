import * as Primitive from './primitive.js';
import * as Bytes from './bytes.js';
import * as Procedure from './procedure.js';
import * as Ports from './ports.js';

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
        Procedure.displayNativeString(out, v);
    } else /* if (typeof v === 'number' || typeof v === 'string') */ {
        out.consume(v.toString());
    }
}

/**
 * @param {!Ports.NativeStringOutputPort} out
 * @param {!(Primitive.Primitive|Uint8Array|Function|String|number|boolean|undefined|null)} v
 */
export function writeNativeString(out, v) {
    if (v === true) {
        out.consume('#t');
    } else if (v === false) {
        out.consume('#f');
    } else if (v === undefined || v === null) {
        out.consume('#<void>');
    } else if (typeof v === 'number' || typeof v === 'string') {
        out.consume(v.toString());
    } else if (Primitive.check(v)) {
        v.writeNativeString(out);
    } else if (Bytes.check(v)) {
        Bytes.displayNativeString(out, v);
    } else if (Procedure.check(v)) {
        Procedure.displayNativeString(out, v);
    } else /* if (typeof v === 'number' || typeof v === 'string') */ {
        out.consume(v.toString());
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
  } else {
      writeNativeString(out, v);
  }
}
