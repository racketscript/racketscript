/**
 * Returns a non-cryptographic hash code for any object.
 * Suitable for use in hash tables.
 *
 * Primitives are hashed based on their value.
 * Everything else is hashed based on its identity (a basically unique ID).
 *
 * If two values are equal with `===` (ECMAScript spec: SameValueZero),
 * their hash codes are guaranteed to be the same,
 * with the exception of Symbol values.
 *
 * @param {*} o
 * @return {!number} a 32-bit integer
 */
export function hash(o) {
    if (o === null) return 0;
    if (o && o.sym) return hashString(`sym_${o.toString()}`);
    switch (typeof o) {
    case 'number':
        return hashNumber(o);
    case 'string':
        return hashString(o);
    case 'boolean':
        return o ? 1 : -1;
    case 'undefined':
        return 0;
    case 'object':
    case 'function':
        return hashObjectIdentity(o);
    default:
        return hashString(o.toString());
    }
}

/**
 * Calculates the given string's hash.
 *
 * Uses the same algorithm as the JVM string hash.
 *
 * @param {!String} s
 * @return {!number} a 32-bit integer
 */
export function hashString(s) {
    let h = 0;
    const n = s.length;
    for (let i = 0; i < n; ++i) {
    // Benchmarks of various ways to do this:
    // https://run.perf.zone/view/String-Hashing-Performance-1504040177726
        h = ~~((h << 5) - h + s.charCodeAt(i));
    }
    return h;
}

// Buffers and arrays for {hashNumber}.
const kBuf = new ArrayBuffer(8);
const kBufAsF64 = new Float64Array(kBuf);
const kBufAsI32 = new Int32Array(kBuf);

/**
 * Calculates the given number's hash.
 *
 * If two numbers are equal with `===` (ECMAScript spec: SameValueZero),
 * the returned hash codes will be the same for them.
 *
 * @param {!number} n any number
 * @return {!number} a 32-bit integer
 */
export function hashNumber(n) {
    // Benchmarks of various ways to do this:
    // https://run.perf.zone/view/Number-Hashing-Performance-v9-1504054910628

    // For small numbers, return the number directly.
    // This slightly increases the potential number of collisions
    // with large numbers and floats, but increases the performance by 20%.
    if (~~n === n) {
    // If `n` is -0, the above check will pass.
    // `~~` is here only to convert the potential -0 to 0.
        return ~~n;
    }
    kBufAsF64[0] = n;
    return kBufAsI32[0] ^ kBufAsI32[1];
}

/**
 * A weak map to the object's ID for {hashObjectIdentity}.
 *
 * @type {!WeakMap<Object, number>}
 */
const objectIds = new WeakMap();
let currentId = 0;

/**
 * Returns an object ID.
 *
 * @param {!(Object|null)} o
 * @return {!number} a 32-bit integer
 */
export function hashObjectIdentity(o) {
    const result = objectIds.get(o);
    if (result === undefined) {
        currentId = ~~(currentId + 1);
        objectIds.set(o, currentId);
        return currentId;
    }
    return result;
}

//= The functions below are not called by `hash`.

/**
 * Returns a hash of the array.
 *
 * The elements of the array must either be integers or objects
 * with `valueOf` that returns an integer.
 *
 * @template T
 * @param {T[]} a
 * @return {!number} a 32-bit integer
 */
export function hashIntArray(a) {
    let h = 0;
    const n = a.length;
    for (let i = 0; i < n; ++i) {
        h = ~~((h << 5) - h + a[i]);
    }
    return h;
}

/**
 * Returns a hash of the array.
 *
 * The given function is called with every element of the array
 * and must return an integer.
 *
 * @template T
 * @param {T[]} a
 * @param {function(T): !number} valueToIntFn
 * @return a 32-bit integer
 */
export function hashArray(a, valueToIntFn) {
    let h = 0;
    const n = a.length;
    for (let i = 0; i < n; ++i) {
        h = ~~((h << 5) - h + valueToIntFn(a[i]));
    }
    return h;
}
