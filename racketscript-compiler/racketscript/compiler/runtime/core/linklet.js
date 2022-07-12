import { check as isCons, isEmpty, EMPTY } from './pair.js';
import { make as makeSym, check as isSym } from './primitive_symbol.js';
import { PrintablePrimitive } from './printable_primitive.js';
import { makeEq } from './hash.js';
import { toBytesUtf8 } from './unicode_string.js';
import { make as makeVector } from './vector.js';
import { make as makeValues } from './values.js';

// TODO I should determine which of the 'props' (arguments to compile-linklet and recompile-linklet actually need to be saved
// eslint-disable-next-line no-unused-vars
class Linklet extends PrintablePrimitive {
    // constructor corresponds to compile-linklet
    // Missing import-keys, get-import, options
    constructor(form, name) {
        super();
        this.compile(form, name);
    }

    // Missing import-keys, get-import, options
    compile(form, name) {
        this.form = form;
        this.name = name;
        this.payload = this._compileLinklet();
    }

    // true purpose is further optimization
    // Missing import-keys, get-import, options
    recompile(name) {
        this.name = name;
        this.payload = this._compileLinklet();
    }

    _compileLinklet() {
        if (isCons(this.form)) {
            const func = this.form.car();
            const args = this.form.cdr();

            if (isSym(func) && func.value === Symbol.for('displayln') && isCons(args)) {
                const arg = args.car();
                const rst = args.cdr();

                if (typeof arg === 'number' && isEmpty(rst)) {
                    return `console.log("${arg}")`;
                }
            }
        }

        return 'throw new Error("This s-expression isn\'t supported yet")';
    }

    importVariables() {
        return EMPTY;
    }

    exportVariables() {
        return EMPTY;
    }

    eval() {
        return this;
    }

    // - intentionally excluding `prompt`, don't know what to do with it
    // - I have no use for instances right now either, since I'm not producing linklets
    //   that import stuff
    instantiate(_instances, target) {
        const res = eval(this.payload);
        if (target === undefined || target === false) {
            return makeInstance(this.name);
        }

        return res;
    }
}

export function makeLinklet(form, name, importKeys) {
    const newLinklet = new Linklet(form, name);
    if (importKeys) {
        return makeValues([newLinklet, makeVector([], true)]);
    } else {
        return newLinklet;
    }
}

export function checkLinklet(l) {
    return l === 'object' && l !== null && l.constructor === Linklet;
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

export function checkInstance(i) {
    return i === 'object' && i !== null && i.constructor === LinkletInstance;
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

// LINKLET PRIMITIVE TABLE
const exportedPrimitives = {
    'linklet?': checkLinklet,
    'compile-linklet': makeLinklet,
    'recompile-linklet': (lnk, n, importKeys, getImport, opts) => lnk.recompile(n, importKeys, getImport, opts),
    'eval-linklet': lnk => lnk.eval(),
    'instantiate-linklet': (lnk, imports, target, usePrompt) => lnk.instantiate(imports, target, usePrompt),
    // HACK: there are never any imports or exports
    'linklet-import-variables': () => EMPTY,
    'linklet-export-variables': () => EMPTY,
    'linklet-virtual-machine-bytes': () => toBytesUtf8('racketscript'),
    'instance?': checkInstance,
    'make-instance': makeInstance,
    'instance-name': instanceName,
    'instance-data': instanceData,
    'instance-variable-names': instanceVariableNames,
    'instance-variable-value': instanceVariableValue,
    'instance-set-variable-value!': instanceSetVariableValue,
    'instance-unset-variable!': instanceUnsetVariable,
    'instance-describe-variable!': instanceDescribeVariable,
    'variable-reference-from-unsafe?': () => false,
    'variable-reference-constant?': () => false
};

export const primitiveTable = Object.entries(exportedPrimitives).reduce(
    (table, [name, impl]) => table.set(makeSym(name), impl),
    makeEq([], false),
);
