import { PrintablePrimitive } from './printable_primitive.js';
import { hashString } from './raw_hashing.js';

let counter = 0;

class PrimitiveSymbol extends PrintablePrimitive {
    constructor(name) {
        super();

        if (name) {
            // interned
            this.name = name;
            this.sym = Symbol.for(name);
        } else {
            // uninterned
            this.sym = Symbol(`_${counter++}`);
        }
    }

    get isInterned() {
        return Boolean(this.name);
    }

    get value() {
        return this.sym;
    }

    equals(s) {
        if (s.sym) {
            return s.value === this.value;
        }
        return s === this.value;
    }

    lt(s) {
        if (s === this) {
            return false;
        }
        return this.toString() < s.toString();
    }

    hashForEqual() {
        return hashString(this.toString());
    }

    /* String printing */

    [Symbol.toPrimitive](hint) {
        if (hint === 'number') {
            return 0;
        }
        return this.toString();
    }

    displayNativeString(out) {
        if (this.isInterned) {
            out.consume(Symbol.keyFor(this.sym));
        } else {
            out.consume(this.sym.toString());
        }
    }
}

export function make(v) {
    return new PrimitiveSymbol(v ? v.toString() : '');
}

export function makeUninterned() {
    return new PrimitiveSymbol();
}

export function check(v) {
    return v instanceof PrimitiveSymbol;
}

export function isInterned(v) {
    if (check(v)) {
        return v.isInterned;
    }
    return false;
}
