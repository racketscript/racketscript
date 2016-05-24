import RacketCoreError from "./core-error.js";

/**
   Base class for various compound data types
   such as structs, pairs, values, ...
*/
export default
class Primitive {
    constructor() {
	//
    }

    toString() {
	throw new RacketCoreError("Not Implemented");
    }

    toRawString() {
	return this.toString();
    }

    equals(v) {
	throw new RacketCoreError("Not Implemented");
    }
}

export
function check(v) {
    return (v instanceof Primitive);
}


