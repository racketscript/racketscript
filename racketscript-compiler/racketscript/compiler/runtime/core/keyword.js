import { PrintablePrimitive } from './printable_primitive.js';
import { internedMake } from './lib.js';

class Keyword extends PrintablePrimitive {
    constructor(v) {
        super();
        this.v = v;
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    displayNativeString(out) {
        out.consume('#:');
        out.consume(this.v);
    }

    equals(v) {
        return check(v) && this.v === v.v;
    }

    lt(v) {
        if (v === this) {
            return false;
        }
        return this.v < v.v;
    }
}


export const make = internedMake(v => new Keyword(v));

export function check(v) {
    return (v instanceof Keyword);
}
