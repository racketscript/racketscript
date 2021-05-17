import { PrintablePrimitive } from './printable_primitive.js';
import { internedMake } from './lib.js';

class Symbol extends PrintablePrimitive {
    constructor(v) {
        super();
        this.v = v;
        this._cachedHashCode = null;
    }

    /**
     * @param {!Ports.NativeStringOutputPort} out
     */
    displayNativeString(out) {
        out.consume(this.v);
    }

    equals(v) {
        // Symbols are interned by default, and two symbols
        // with same name can't be unequal.
        // Eg. (define x (gensym)) ;;=> 'g60
        //     (equal? x 'g60)     ;;=> #f
        // TODO: does this handle uninterned symbols?
        return v === this;
    }

    lt(v) {
        if (v === this) {
            return false;
        }
        return this.v < v.v;
    }

    /**
     * @return {!number}
     */
    hashForEqual() {
        if (this._cachedHashCode === null) {
            this._cachedHashCode = super.hashForEqual();
        }
        return this._cachedHashCode;
    }
}


export const make = internedMake(v => new Symbol(v.toString()));

// TODO: is it correct to convert toString()?
export const makeUninterned = v => new Symbol(v.toString());

export function check(v) {
    return (v instanceof Symbol);
}
