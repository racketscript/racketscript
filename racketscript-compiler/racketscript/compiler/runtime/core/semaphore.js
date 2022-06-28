import { PrintablePrimitive } from './printable_primitive.js';
import { isEqual } from './equality.js';
import { hashForEqual } from './hashing.js';
import { racketCoreError } from './errors.js';

const SEMAPHORE_REPR = '#<semaphore>';

class Semaphore extends PrintablePrimitive {
    constructor(n) {
        super();
        this.n = n;
    }

    post() {
        this.n += 1;
    }

    wait() {
        if (this.n >= 1) {
            return;
        }
        throw racketCoreError('Waiting for a semaphore will never finish');
    }
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
        out.consume(SEMAPHORE_REPR);
    }

    /**
     * @param {!Ports.UStringOutputPort} out
     */
    displayUString(out) {
        out.consume(SEMAPHORE_REPR);
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    writeNativeString(out) {
        out.consume(SEMAPHORE_REPR);
    }

    /**
     * @param {!Ports.UStringOutputPort} out
     */
    writeUString(out) {
        out.consume(SEMAPHORE_REPR);
    }
}

export function make(n) {
    return new Semaphore(n);
}

export function check(v) {
    return (v instanceof Semaphore);
}
