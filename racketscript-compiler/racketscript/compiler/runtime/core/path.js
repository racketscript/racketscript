import { PrintablePrimitive } from './printable_primitive.js';
import { make as values } from './values.js';
import { make as sym } from './primitive_symbol.js';

const UNIX_PATH_SEP = '/';

// ASSUMES that the path does not end with double separator, e.g. '//'
const splitPathString = (str) => {
    const lastDirSep = str.substring(0, str.length - 1).lastIndexOf(UNIX_PATH_SEP);
    let dirPathStr = '';
    if (lastDirSep !== -1) {
        dirPathStr = str.substring(0, lastDirSep + 1); // include the last separator
    }

    const basePathStr = str.substring(lastDirSep + 1);
    return { dirPathStr, basePathStr };
};

// TODO going to assume this is Unix for now, will fix later
class Path extends PrintablePrimitive {
    constructor(s) {
        super();
        this.s = s;
    }

    isRelative() {
        return this.s.substring(0, 1) !== UNIX_PATH_SEP;
    }

    isAbsolute() {
        return this.s.substring(0, 1) === UNIX_PATH_SEP;
    }

    // TODO isComplete is only different than isAbsolute if on a Windows machine
    isComplete() {
        return this.isAbsolute();
    }

    isDir() {
        return this.s.slice(-2, -1) === UNIX_PATH_SEP;
    }

    appendPath(p) {
        const isDirPath = this.isDir();
        const isCompPath = p.isComplete();
        if (isDirPath ^ isCompPath) {
            return fromString(this.s + p.s);
        } else if (isDirPath) {
            return fromString(this.s + p.s.substring(1));
        } else {
            return fromString(this.s + UNIX_PATH_SEP + p.s);
        }
    }

    splitPath() {
        const { dirPathStr, basePathStr } = splitPathString(this.s);

        let dirRes = '';
        let baseRes = '';

        if (dirPathStr === '' && basePathStr === UNIX_PATH_SEP) {
            dirRes = false;
        } else if (dirPathStr === '') {
            dirRes = sym('relative');
        } else {
            dirRes = fromString(dirPathStr);
        }

        if (basePathStr === '.') {
            baseRes = sym('same');
        } else if (basePathStr === '..') {
            baseRes = sym('up');
        } else {
            baseRes = fromString(basePathStr);
        }

        return values([dirRes, baseRes, this.isDir()]);
    }
}

export function fromString(s) { return new Path(s); }
export function check(s) { return (s instanceof Path); }

