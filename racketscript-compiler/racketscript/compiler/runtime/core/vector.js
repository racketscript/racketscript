import { PrintablePrimitive } from './printable_primitive.js';
import { displayNativeString, writeNativeString } from './print_native_string.js';
import { isEqual } from './equality.js';
import { hashArray } from './raw_hashing.js';
import { hashForEqual } from './hashing.js';
import { racketCoreError } from './errors.js';

class Vector extends PrintablePrimitive {
    constructor(items, mutable) {
        super();
        this.mutable = mutable;
        this.items = items;
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     * @param {function(Ports.NativeStringOutputPort, *)} itemFn
     */
    writeToPort(out, itemFn) {
        const n = this.items.length;
        out.consume('#(');
        for (let i = 0; i < n; i++) {
            itemFn(out, this.items[i]);
            if (i !== n - 1) out.consume(' ');
        }
        out.consume(')');
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    displayNativeString(out) {
        this.writeToPort(out, displayNativeString);
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    writeNativeString(out) {
        this.writeToPort(out, writeNativeString);
    }

    isImmutable() {
        return !this.mutable;
    }

    ref(n) {
        if (n < 0 || n > this.items.length) {
            throw racketCoreError(`vector-ref: index out of range\n  index: ${n}`);
        }

        return this.items[n];
    }

    set(n, v) {
        if (n < 0 || n > this.items.length) {
            throw racketCoreError(`vector-set: index out of range\n  index: ${n}`);
        } else if (!this.mutable) {
            throw racketCoreError('vector-set: immutable vector\n ', this);
        }
        this.items[n] = v;
    }

    copy(destStart, src, srcStart, srcEnd) {
        for (let i = srcStart, j = destStart;
            i < srcEnd && i < src.items.length && j < this.items.length;
            i++, j++) {
            this.items[j] = src.items[i];
        }
    }

    length() {
        return this.items.length;
    }

    /**
     * @param {*} v
     * @return {!boolean}
     */
    equals(v) {
        if (!check(v)) {
            return false;
        }

        const items1 = this.items;
        const items2 = v.items;

        if (items1.length !== items2.length) {
            return false;
        }

        for (let i = 0; i < items1.length; i++) {
            if (!isEqual(items1[i], items2[i])) {
                return false;
            }
        }

        return true;
    }

    /**
     * @return {!number} a 32-bit integer
     */
    hashForEqual() {
        return hashArray(this.items, hashForEqual);
    }
}

export function make(items, mutable) {
    return new Vector(items, mutable);
}

export function copy(vec, mutable) {
    return new Vector(vec.items, mutable);
}

export function makeInit(size, init) {
    const r = new Array(size);
    r.fill(init);
    return new Vector(r, true);
}

export function check(v1) {
    return (v1 instanceof Vector);
}
