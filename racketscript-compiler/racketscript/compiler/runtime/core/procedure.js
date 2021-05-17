/**
 * @param {!Function} fn
 * @param {*} arity
 */
// arity is either:
// - array of exact arities,
// - a number, meaning "arity of at least"
export function attachProcedureArity(fn, arity) {
    fn.__rjs_arityValue = arity || fn.length;
    return fn;
}

/**
 * @param {!Function} fn
 * @param {!String} name
 */
// need this to get some fns to print correct Racket name,
// eg "cons" instead of "makePair"
export function attachProcedureName(fn, name) {
    fn.__rjs_name = name;
    return fn;
}

/**
 * @param {*} v
 * @return {!boolean}
 */
export function check(v) {
    return typeof v === 'function';
}

/**
 * @param {!Function} f
 * @return {!String} A string representation similar to Racket's `display`.
 */
export function toString(f) {
    const notRjsName = f.name
        ? `#<procedure:${f.name}>`
        : '#<procedure>';

    return f.__rjs_name
        ? `#<procedure:${f.__rjs_name}>`
        : notRjsName;
}

/**
 * Writes a string representation similar to Racket's `display` to the given port.
 *
 * @param {!Ports.NativeStringOutputPort} out
 * @param {!Function} f
 */
export function displayNativeString(out, f) {
    out.consume(toString(f));
}
