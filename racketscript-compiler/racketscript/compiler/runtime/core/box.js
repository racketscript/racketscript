import { Primitive } from './primitive.js';
import { isEqual } from './equality.js';

class Box extends Primitive {
    constructor(v) {
        super();
        this.value = v;
    }

    toString() {
        return this.value;
    }

    toRawString() {
        return this.toString();
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
}


export function make(v) {
    return new Box(v);
}

export function check(v) {
    return (v instanceof Box);
}
