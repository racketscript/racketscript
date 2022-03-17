import { check as isCons, isEmpty } from './pair.js';
import { check as isSym } from './primitive_symbol.js';
import { PrintablePrimitive } from './printable_primitive.js';


// TODO I should determine which of the 'props' (arguments to compile-linklet and recompile-linklet actually need to be saved
// eslint-disable-next-line no-unused-vars
class Linklet extends PrintablePrimitive {
    // constructor corresponds to compile-linklet
    constructor(form, name, importKeys, getImport, options) {
        super();
        this.compileLinklet(form, name, importKeys, getImport, options);
    }

    compile(form, name, importKeys, getImport, options) {
        this.form = form;
        this._setProps(name, importKeys, getImport, options);
        this.linklet = this._compile();
    }

    _compile() {
        if (this._isRightSexp()) {
            return 'console.log("1")';
        }
        throw new Error('cannot handle this s-expression yet');
    }

    // in reality, we should be able to throw most of these away
    _setProps(name, importKeys, getImport, options) {
        this.name = name;
        this.importKeys = importKeys;
        this.getImport = getImport;
        this.options = options;
    }


    _recompile(name, importKeys, getImport, options) {
        this._setProps(name, importKeys, getImport, options);
    }

    // actual purpose is to optimize linklet
    recompile(name, importKeys, getImport, options) {
        this._recompile(name, importKeys, getImport, options);
    }

    _validateSexp() {
        if (isCons(this.form)) {
            const cmd = this.form.car();
            const args = this.form.cdr();

            if (isSym(cmd) && cmd.value() === 'displayln' && isCons(args)) {
                const arg = args.car();
                const rst = args.cdr();

                return arg === 1 && isEmpty(rst);
            }
        }

        return false;
    }

    eval() {
        return eval(this.linklet);
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

