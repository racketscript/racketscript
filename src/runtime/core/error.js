
function makeError(name) {
    let e = function(message) {
	this.name = name;
	this.message = message;
	this.stack = (new Error()).stack;
	if (Error.captureStackTrace) {
            Error.captureStackTrace(this, this.constructor);
	} else {
            this.stack = (new Error()).stack;
	}
    }
    e.prototype = Object.create(Error.prototype);
    e.prototype.constructor = e;
    return e;
}

export let RacketCoreError = makeError("RacketCoreError");
export let RacketContractError = makeError("RacketContractError");
