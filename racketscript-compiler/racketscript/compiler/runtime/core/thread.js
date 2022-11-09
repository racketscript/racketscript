import { PrintablePrimitive } from './printable_primitive.js';
import { isEqual } from './equality.js';
import { hashForEqual } from './hashing.js';

const THREAD_REPR = '#<thread>';

class Thread extends PrintablePrimitive {
    /**
     * @param {*} v
     * @return {!boolean}
     */
    equals(other) {
        return isEqual(this, other);
    }

    /**
     * @return {!number} a 32-bit integer
     */
    hashForEqual() {
        return hashForEqual(this);
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    displayNativeString(out) {
        out.consume(THREAD_REPR);
    }

    /**
     * @param {!Ports.UStringOutputPort} out
     */
    displayUString(out) {
        out.consume(THREAD_REPR);
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    writeNativeString(out) {
        out.consume(THREAD_REPR);
    }

    /**
     * @param {!Ports.UStringOutputPort} out
     */
    writeUString(out) {
        out.consume(THREAD_REPR);
    }
}

export function make() {
    return new Thread();
}

export function check(v) {
    return (v instanceof Thread);
}

export const currentThread = make();
