import { PrintablePrimitive } from './printable_primitive.js';
import { isEqual } from './equality.js';
import { hashForEqual } from './hashing.js';
import { displayNativeString, writeNativeString } from './print_native_string.js';
import { displayUString, writeUString } from './print_ustring.js';
import * as UString from './unicode_string.js';

class Linklet extends PrintablePrimitive {
    constructor() {
	super()
    }
}


class LinkletInstance extends PrintablePrimitive {
    constructor(name, data, mode, m) {
	super()
	this.name = name;
	this.data = data;
	this.mode = mode;
	this.m = m;
    }
}

export function make_instance(name, _data, _mode, ...args) {
    let m = new Map();
    let data = _data || false;
    let mode = _mode || false;
    for (let i = 0; i < args.length; i=i+2) {
	m.set(args[i],args[i+1]);
    }
    return new LinkletInstance(name, data, mode, m);
}


export function instance_name(i) { return i.name; }
export function instance_data(i) { return i.data; }
export function instance_variable_value(i,s) { return i.m.get(s); }
export function instance_variable_names(i) { return i.keys(); }
export function instance_set_variable_value(i,s,v) {
    return i.m.set(s,v);
}
export function instance_unset_variable(i,s) { return i.m.remove(s); }
export function instance_describe_variable(...args) { return; }



