import { PrintablePrimitive } from './printable_primitive.js';
import { isEqual } from './equality.js';
import { hashForEqual } from './hashing.js';
import { displayNativeString, writeNativeString } from './print_native_string.js';
import { displayUString, writeUString } from './print_ustring.js';
import * as UString from './unicode_string.js';

class Path extends PrintablePrimitive {
    constructor(s) {
	super();
	this.s = s;
    }
}

export function from_string(s) { return new Path(s); }
export function check(s) { return (s instanceof Path); }




