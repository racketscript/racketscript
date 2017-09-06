/**
 * @param {!Function} fn
 * @param {*} arity
 */
export function attachProcedureArity(fn, arity) {
    fn.__rjs_arityValue = arity || fn.length;
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
    return f.name ? `#<procedure:${f.name}>` : '#<procedure>';
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
