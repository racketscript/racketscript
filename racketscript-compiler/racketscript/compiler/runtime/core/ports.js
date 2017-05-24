import {Primitive} from "./primitive.js";
import * as $ from "./lib.js";

//TODO: Quick hacked up implementation around console.log
class Port extends Primitive {
    constructor(read, write) {
	super();
	this.__state = {
	    buffer: []
	} ;
	this.__read = read;
	this.__write = write;
    }

    read(n_char) {
	if (this.__read === undefined) {
	    throw $.racketCoreError("Not Implemented");
	} else {
	    return this.__read(this.__state, n_char);
	}
    }

    write(chars) {
	if (this.__write === undefined) {
	    throw $.racketCoreError("Not Implemented");
	} else {
	    return this.__write(this.__state, chars);
	}
    }

    isOutputPort() {
	return this.__write && true;
    }

    isInputPort() {
	return this.__read && true;
    }
}

export function makeOutputPort(write) {
    return new Port(undefined, write);
}

export function makeInputPort(read) {
    return new Port(read);
}

export function check(v) {
    return (v instanceof Port);
}

export function checkInputPort(v) {
    return check(v) && v.isInputPort();
}

export function checkOutputPort(v) {
    return check(v) && v.isOutputPort();
}

export let standardOutputPort = makeOutputPort((state, chars) => {
    let nl_index = chars.lastIndexOf("\n");
    if (nl_index >= 0) {
	let flushchars = state.buffer.join("") + chars.slice(0, nl_index);
	let rest_chars = chars.slice(nl_index + 1);
	state.buffer = [];
	if (rest_chars !== "") {
	    state.buffer.push(rest_chars);
	}
	console.log(flushchars);
    } else {
	state.buffer.push(chars);
    }
});
