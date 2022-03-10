import { PrintablePrimitive } from './printable_primitive.js';

// eslint-disable-next-line no-unused-vars
class Linklet extends PrintablePrimitive {
    // constructor corresponds to compile-linklet
    constructor(form, name, importKeys, getImports, options) {
        super();
        this.form = form;
        this.name = name;
        this.importKeys = importKeys;
        this.getImports = getImports;
        this.options = options;
        this.payload = this._compileLinklet();
    }

    _compileLinklet() {
        // (cons displayln (cons 1 '()))


        // Q: what is the data definition for 'form'?
        //    it should produce whatever the runtime has decided
        //    the definition of 'cons cells' are, and then
        //    however symbols, numbers, etc. are represented for the
        //    relevant things.
        
    }
}

class LinkletInstance extends PrintablePrimitive {
    constructor(name, data, mode, m) {
        super();
        this.name = name;
        this.data = data;
        this.mode = mode;
        this.m = m;
    }
}

export function makeInstance(name, _data, _mode, ...args) {
    const m = new Map();
    const data = _data || false;
    const mode = _mode || false;
    for (let i = 0; i < args.length; i += 2) {
        m.set(args[i], args[i + 1]);
    }
    return new LinkletInstance(name, data, mode, m);
}


export function instanceName(i) { return i.name; }
export function instanceData(i) { return i.data; }
export function instanceVariableValue(i, s) { return i.m.get(s); }
export function instanceVariableNames(i) { return i.keys(); }
export function instanceSetVariableValue(i, s, v) {
    return i.m.set(s, v);
}
export function instanceUnsetVariable(i, s) { return i.m.remove(s); }
export function instanceDescribeVariable() { }

