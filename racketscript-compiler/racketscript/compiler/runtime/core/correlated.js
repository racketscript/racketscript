import { PrintablePrimitive } from './printable_primitive.js';
import { isEqual } from './equality.js';
import { hashForEqual } from './hashing.js';

class Correlated extends PrintablePrimitive {
    constructor(v) {
        super();
        this.value = v;
    }

    equals(v) {
        return isEqual(v.value, this.value);
    }

    get() { return this.value; }

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
        out.consume('#<syntax>');
    }
}

export function datumToSyntax(v) { return new Correlated(v); }

export function syntaxP(v) { return (v instanceof Correlated); }

// TODO: implement these stubs
/* eslint no-unused-vars: ["error", { "args": "none" }] */
export function syntaxSource(v) { return false; }

/* eslint no-unused-vars: ["error", { "args": "none" }] */
export function syntaxLine(v) { return false; }

/* eslint no-unused-vars: ["error", { "args": "none" }] */
export function syntaxColumn(v) { return false; }

/* eslint no-unused-vars: ["error", { "args": "none" }] */
export function syntaxPosition(v) { return false; }

/* eslint no-unused-vars: ["error", { "args": "none" }] */
export function syntaxSpan(v) { return false; }

