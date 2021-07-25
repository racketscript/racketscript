// Create interned Symbol (Symbol.for)
export const make = v => Symbol.for(v ? v.toString() : '');

// Create uninterned Symbol
export const makeUninterned = v => Symbol(v ? v.toString() : '');

export function check(v) {
    return typeof v === 'symbol';
}

// Only interned Symbols (Symbol.for) will have a keyFor
export function isInterned(v) {
    return Boolean(Symbol.keyFor(v));
}

// Compares that two Symbols are equal. Only interned symbols
// will pass.
export function equals(s, v) {
    if (check(s) && check(v)) {
        return s === v;
    }
    return false;
}

// Can only get the value of interned Symbols (Symbol.for)
// Ex: Symbol.keyFor(Symbol.for("foo")) === "foo"
export function getValue(s) {
    return Symbol.keyFor(s);
}

export function lt(s, v) {
    if (check(s) && check(v)) {
        return getValue(s) < getValue(v);
    }
    return false;
}
