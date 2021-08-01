import { PrintablePrimitive } from './printable_primitive.js';

let counter = 0;

class PrimitiveSymbol extends PrintablePrimitive {
    constructor(name) {
        super(name);

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

    le(s) {
        if (s === this) {
            return false;
        }
        return this.value < s.value;
    }

    /* String printing */

    [Symbol.toPrimitive](hint) {
        if (hint === 'number') {
            return 0;
        }
        return this.toString();
    }

    toString() {
        return Symbol.keyFor(this.sym);
    }

    displayNativeString(out) {
        out.consume(this.toString());
    }

    // Adds the quote character before the value, ex: 'sym
    printNativeString(out) {
        out.consume("'");
        this.writeNativeString(out);
    }
}

export function make(v) {
    return new PrimitiveSymbol(v);
}

export function makeUninterned() {
    return new PrimitiveSymbol();
}

export function check(v) {
    return v instanceof PrimitiveSymbol;
}
