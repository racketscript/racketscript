import { Primitive } from './primitive.js';

class Values extends Primitive {
    constructor(vals) {
        super();
        this.v = vals;
    }

    getAt(i) {
        return this.v[i];
    }

    getAll() {
        return this.v;
    }
}


export function make(vals) {
    return new Values(vals);
}

export function check(v) {
    return (v instanceof Values);
}
