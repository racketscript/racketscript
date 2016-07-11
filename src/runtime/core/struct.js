import {default as Primitive} from "./primitive.js";
import RacketCoreError from "./error.js";
import * as rutils from "./utils.js";
import * as Values from "./values.js";

class Struct extends Primitive {
    constructor(name, fields) {
	super();
	this.name = name;
	this.fields = [];
	for (var i = 0; i < fields.length; i++) {
	    this.fields.push(fields[i]);
	}
    }

    toString() {
	var fields = "";
	for (var i = 0; i < this.fields.length; i++) {
	    fields += this.fields[i].toString();
	    if (i !== this.fields.length - 1) {
		fields += " ";
	    }
	}
	return "#(struct:" + this.name + " " + fields + ")";
    }

    toRawString() {
	return this.toString();
    }

    getName() {
	return this.name;
    }

    getField(n) {
	if (n >= this.fields.length) {
	    throw new Error("TypeError: invalid field at position " + n);
	}
	return this.fields[n];
    }

    equals(v) {
	if (this.name !== v.name ||
	    this.fields.length !== v.fields.length) {
	    return false;
	}

	for (var i = 0; i < this.fields.length; i++) {
	    if (!rutils.isEqual(this.fields[i], v.fields[i])) {
		return false;
	    }
	}

	return true;
    }
}

//TODO: two different struct types with same name?
export
function makeStructType(options) {
    var constructor = function() {
	if (arguments.length != options.init_field_cnt) {
	    throw new RacketCoreError("Error: invalid number of arguments");
	}
	return new Struct(options.name, arguments);
    }

    var predicate = function(v) {
	return v.getName() == options.name;
    }

    var accessor = function(s, pos) {
	return s.getField(pos);
    }

    var mutator = function(s, pos) {
	throw new RacketCoreError("Error: Not implemented error");
    }

    var desc = "struct:" + options.name;

    return Values.make([desc, constructor, predicate, accessor, mutator])
}
