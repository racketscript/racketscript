import { PrintablePrimitive } from './printable_primitive.js';

const UNIX_PATH_SEP = '/';

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
}

export function fromString(s) { return new Path(s); }
export function check(s) { return (s instanceof Path); }

