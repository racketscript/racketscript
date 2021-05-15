import { PrintablePrimitive } from './printable_primitive.js';

// eslint-disable-next-line no-unused-vars
class Linklet extends PrintablePrimitive {}

class LinkletInstance extends PrintablePrimitive {
    constructor(name, data, mode, m) {
        super();
        this.name = name;
        this.data = data;
        this.mode = mode;
        this.m = m;
    }
}

export function make_instance(name, _data, _mode, ...args) {
    const m = new Map();
    const data = _data || false;
    const mode = _mode || false;
    for (let i = 0; i < args.length; i += 2) {
        m.set(args[i], args[i + 1]);
    }
    return new LinkletInstance(name, data, mode, m);
}


export function instance_name(i) { return i.name; }
export function instance_data(i) { return i.data; }
export function instance_variable_value(i, s) { return i.m.get(s); }
export function instance_variable_names(i) { return i.keys(); }
export function instance_set_variable_value(i, s, v) {
    return i.m.set(s, v);
}
export function instance_unset_variable(i, s) { return i.m.remove(s); }
export function instance_describe_variable() { }

