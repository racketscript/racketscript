import { PrintablePrimitive } from './printable_primitive.js';
import { isEqual } from './equality.js';
import { hashForEqual } from './hashing.js';
import { displayNativeString, writeNativeString } from './print_native_string.js';
import { displayUString, writeUString } from './print_ustring.js';
import * as UString from './unicode_string.js';

const BOX_PREFIX_USTRING = UString.makeInternedImmutable('#&');

class Box extends PrintablePrimitive {
    constructor(v) {
        super();
        this.value = v;
    }

    set(v) {
        this.value = v;
    }

    get() {
        return this.value;
    }

    /**
     * @param {*} v
     * @return {!boolean}
     */
    equals(v) {
        return isEqual(v.value, this.value);
    }

    /**
     * @return {!number} a 32-bit integer
     */
    hashForEqual() {
        return hashForEqual(this.value);
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    displayNativeString(out) {
        out.consume('#&');
        displayNativeString(out, this.value);
    }

    /**
     * @param {!Ports.UStringOutputPort} out
     */
    displayUString(out) {
        out.consume(BOX_PREFIX_USTRING);
        displayUString(out, this.value);
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    writeNativeString(out) {
        out.consume('#&');
        writeNativeString(out, this.value);
    }

    /**
     * @param {!Ports.UStringOutputPort} out
     */
    writeUString(out) {
        out.consume(BOX_PREFIX_USTRING);
        writeUString(out, this.value);
    }
}


export function make(v) {
    return new Box(v);
}

export function check(v) {
    return (v instanceof Box);
}
