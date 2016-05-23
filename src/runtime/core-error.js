/**
 * RacketCoreError: String String -> Void
 * A custom error object for Racket runtime
 */
export default
function RacketCoreError(lab, message) {
    this.name = 'RacketCoreError';
    this.message = (lab + ": ") + message;
    this.stack = (new Error()).stack;
}
RacketCoreError.prototype = Object.create(Error.prototype);
RacketCoreError.prototype.constructor = RacketCoreError;
