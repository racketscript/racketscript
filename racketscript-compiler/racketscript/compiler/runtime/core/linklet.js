import { isEmpty, check as isCons } from './pair.js';
import { check as isSym } from './primitive_symbol.js';
import { PrintablePrimitive } from './printable_primitive.js';

// eslint-disable-next-line no-unused-vars
class Linklet extends PrintablePrimitive {
    // constructor corresponds to compile-linklet
    constructor(form, name, importKeys, getImport, options) {
        super();
        this.form = form;
        this._setProps(name, importKeys, getImport, options);
        this.payload = this._compileLinklet();
    }

    _setProps(name, importKeys, getImport, options) {
        this.name = name;
        this.importKeys = importKeys;
        this.getImport = getImport;
        this.options = options;
    }

    _compileLinklet() {
        if (isCons(this.form)) {
            const func = this.form.car();
            const args = this.form.cdr();

            if (isSym(func) && func.value() === 'display' && isCons(args)) {
                const arg = args.car();
                const rst = args.cdr();

                if (arg === 1 && isEmpty(rst)) {
                    return 'console.log("1")';
                }
            }
        }

        return 'throw new Error("Sad!")';
    }

    eval() {
        return eval(this.payload);
    }
}

export function check(l) { return l.constructor === Linklet; }

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

