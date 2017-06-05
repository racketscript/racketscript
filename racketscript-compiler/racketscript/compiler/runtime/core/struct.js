import * as C from "./check.js";
import * as $ from "./lib.js";
import * as Pair from "./pair.js";
import {Primitive} from "./primitive.js";
import * as Values from "./values.js";

// This module implements Racket structs via three classes which
// directly corresponds to their Racket counterparts. Structure
// instances are either Struct or if its applicable structures, i.e.
// with a proc-spec parametre, it is a function with Struct wrapped
// inside.
//
// - Struct: This class represents a struct instance. This will
//   keep a reference to its struct-type-description, to determine
//   properties, guards etc...
// - StructTypeDescriptor: Corresponds to a struct-type-descriptor
//   which is returned by make-struct-type. This class contains
//   all data needed to create constructor, accessors, mutators to
//   structures. Properties are attached when an object is created
//   using make-struct-type. All super properties of properties that
//   are directly passed to make-struct-type are added, however,
//   properties of super struct-type-descriptors are not added, and
//   searched on-demand. In future we may cache all this for
//   performance
// - StructTypeProperty: Class representing a struct-type-property.
//   Also contains methods to attach itself to a struct instance.
//   By specification, guards are applied when being attached to
//   structs.
//
// TODO:
// - Structure Inspectors
// - Prefab

class Struct extends Primitive {
    constructor(desc, fields, callerName=false) {
	super();
	this._desc = desc; /* struct-type-descriptor */

	C.eq(fields.length,
	     this._desc._totalInitFields,
	     $.racketCoreError,
	     "arity mismatch");

	// Guard's are applied starting from subtype to supertype
	// Later when we instantiate the subtype, its guard will be
	// called in its constructor, hence maintaining the required
	// order
	let guardLambda = this._desc._options.guard;
	if (guardLambda) {
	    let guardFields = fields.concat(callerName ||
					    this._desc._options.constructorName ||
					    this._desc._options.name);
	    fields = guardLambda.apply(null, guardFields).getAll()
	}

	// Initialize current and super instance
	this._superStructInstance = false /* Struct instance of super-type */
	let superType = this._desc.getSuperType();
	if (superType !== false) {
	    let superInitFields = fields.slice(0, superType._totalInitFields);
	    this._fields = fields.slice(superType._totalInitFields);
	    this._superStructInstance = superType.getStructConstructor()
		.apply(null, superInitFields);
	} else {
	    this._fields = fields;
	}

	// Auto fields
	let autoV = this._desc._options.autoV; /* Initial value for auto fields */
	for (let i = 0; i < this._desc._options.autoFieldCount; i++) {
	    this._fields.push(autoV);
	}
    }

    toString() {
	let fields = "";
	for (let i = 0; i < this._fields.length; i++) {
	    fields += this._fields[i].toString();
	    if (i !== this._fields.length - 1) {
		fields += " ";
	    }
	}
	return "#(struct:" + this._desc.getName() + " " + fields + ")";
    }

    toRawString() {
	return this.toString();
    }

    equals(v) {
	if (!check(v, this._desc)) {
	    return false;
	}

	// TODO: Support equal+hash property

	if (this._desc._options.inspector) {
	    // Not a transparent inspector
	    return this === v;
	}

	for (let i = 0; i < this._fields.length; i++) {
	    if (!$.isEqual(this._fields[i], v._fields[i])) {
		return false;
	    }
	}

	return true;
    }

    getField(n) {
	if (n >= this._fields.length) {
	    throw new Error("TypeError: invalid field at position " + n);
	}
	return this._fields[n];
    }

    setField(n, v) {
	C.truthy(n < this._fields.length, $.racketCoreError,
		 "invalid field at position");
	C.falsy(this._desc.isFieldImmutable(n), $.racketCoreError,
		 "field is immutable");
	this._fields[n] = v;
    }

    // Return false if targetDesc is not a superType of current struct
    // or return struct instance of struct-type-descriptor targetDesc
    _maybeFindSuperInstance(targetDesc) {
	for (let s = this; s !== false; s = s._superStructInstance) {
	    if (s._desc === targetDesc) {
		return s;
	    }
	}

	return false;
    }
}

/*****************************************************************************/

class StructTypeDescriptor extends Primitive {
    constructor(options) {
	super();
	// Visit makeStructType (or make-struct-type in Racket docs)
	// to see the structure of options
	this._options = options;

	// Initialize properties
	// supers in struct-type-property are also added when
	// attached. However propeties attached to super types of this
	// struct are not added here and will have to be followed.
	let props = options.props && Pair.listToArray(options.props);
	this._options.props = new Map();
	if (props) {
	    // TODO: If prop is already added, then check associated
	    // values with eq?, else raise contract-errorx
	    for (let prop of props) {
		prop.hd.attachToStructTypeDescriptor(this, prop.tl)
	    }
	}
	this._propProcedure = this._findProperty(propProcedure);

	// Value for auto fields
	this._options.autoV = this._options.autoV || false;

	// Number of intializing fields needed, that is including
	// those of super types
	this._totalInitFields = options.initFieldCount;
	if (options.superType) {
	    this._totalInitFields += options.superType._totalInitFields;
	}

	// Immutables
	let immutables = options.immutables || [];
	this._options.immutables = new Set(Pair.listToArray(immutables));
	this._options.immutables.forEach((e) => {
	    if (e < 0 || e >= options.initFieldCount) {
		C.raise("invalid index in immutables provided");
	    }
	});
    }

    static make(options) {
	return Object.freeze(new StructTypeDescriptor(options));
    }

    toString() {
	return "#<struct-type:" + this._options.name + ">";
    }

    toRawString() {
	return this.toString();
    }

    getName() {
	return this._options.name;
    }

    getSuperType() {
	return this._options.superType;
    }

    getApplicableStructObject(structObject, procSpec) {
	let structfn = function (...args) {
	    // Struct object is also a procedure
	    let proc;
	    if (typeof(procSpec) === 'function') {
		proc = procSpec;
		// Structure object is sent only when procSpec is
		// function, not when we get procedure from field.
		args.unshift(structObject);
	    } else if (Number.isInteger(procSpec)) {
		proc = structObject.getField(procSpec);
	    } else {
		throw new Error("ValueError: invalid field at position "
				+ procSpec);
	    }
	    return proc.apply(null, args);
	}
	structfn.__rjs_struct_object = structObject;
	return structfn;
    }

    maybeStructObject(s) {
	let structObject;
	if (s instanceof Struct) {
	    return s;
	} else if (s instanceof Function &&
		   (s.__rjs_struct_object instanceof Struct)) {
	    return s.__rjs_struct_object;
	} else {
	    return false;
	}
    }

    getStructConstructor() {
	return $.attachReadOnlyProperty((...args) => {
	    let structObject = new Struct(this, args);
	    let hasPropProc = this._propProcedure !== undefined &&
		this._propProcedure !== false;
	    let hasProcSpec = this._options.procSpec !== undefined &&
		this._options.procSpec !== false;

	    if (!hasPropProc && !hasProcSpec) {
		return structObject;
	    } else if (hasPropProc) {
		return this.getApplicableStructObject(
		    structObject,
		    this._propProcedure);
	    } else {
		return this.getApplicableStructObject(
		    structObject,
		    this._options.procSpec);
	    }
	}, "racketProcedureType", "struct-constructor");
    }

    getStructPredicate() {
	return $.attachReadOnlyProperty((s) => {
	    let structObject = this.maybeStructObject(s);
	    return structObject &&
		structObject._maybeFindSuperInstance(this) && true;
	}, "racketProcedureType", "struct-predicate");
    }

    getStructAccessor() {
	return $.attachReadOnlyProperty((s, pos) => {
	    let structObject = this.maybeStructObject(s);
	    if (!structObject) {
		C.raise(TypeError,
			"(" + s + " : " + typeof(s) + " != " +
			this._options.name + " object)");
	    }

	    let sobj = structObject._maybeFindSuperInstance(this);
	    if (sobj === false) {
		C.raise($.racketCoreError, "accessor applied to invalid type")
	    }

	    return sobj.getField(pos);
	}, "racketProcedureType", "struct-accessor");
    }

    getStructMutator() {
	return $.attachReadOnlyProperty((s, pos, v) => {
	    let structObject = this.maybeStructObject(s);
	    if (!structObject) {
		C.raise(TypeError,
			"(" + s + " : " + typeof(s) + " != " +
			this._options.name + " object)");
	    }

	    let sobj = structObject._maybeFindSuperInstance(this);
	    if (sobj === false) {
		C.raise($.racketCoreError, "mutator applied to invalid type")
	    }

	    return sobj.setField(pos, v);

	}, "racketProcedureType", "struct-mutator");
    }

    // Find value associated with property `prop` or return `undefined`
    // We return undefined, as `false` could be possible Racket value
    // attached
    //
    // The property can be in -
    // - Current struct-type-descriptor
    // - Super of current struct-type-descriptor
    // - Supers of any struct-type-property attached to this
    // The first and third case can be handled together. See property
    // initialization in constructor.
    _findProperty(prop) {
	for (let desc = this; desc; desc = desc.getSuperType()) {
	    let val = desc._options.props.get(prop);
	    if (val !== undefined) {
		return val;
	    }
	}

	return undefined;
    }

    isFieldImmutable(n) {
	return this._options.immutables.has(n);
    }
}

/*****************************************************************************/

class StructTypeProperty extends Primitive {
    constructor(args) {
	super();

	this._name =  args.name.toString();
	this._guard = args.guard || false; /* applied when attaching */
	this._canImpersonate = args.canImpersonate || false; /* TODO */
	this._supers = (args.supers && Pair.listToArray(args.supers)) || [];
    }

    static make(args) {
	return Object.freeze(new StructTypeProperty(args));
    }

    toString() {
	return "#<struct-type-property:" + this._name + ">";
    }

    toRawString() {
	return this.toString();
    }

    getPropertyPredicate() {
	return (v) => {
	    if (v instanceof StructTypeDescriptor) {
		var desc = v;
	    } else if (v instanceof Struct) {
		var desc = v._desc;
	    } else {
		return false;
	    }

	    return desc._findProperty(this) !== undefined;
	}
    }

    getPropertyAccessor() {
	return (v) => { /* property acccessor */
	    if (v instanceof StructTypeDescriptor) {
		var desc = v;
	    } else if (v instanceof Struct) {
		var desc = v._desc;
	    } else {
		C.raise($.racketCoreError, "invalid argument to accessor");
	    }

	    return desc._findProperty(this) ||
		C.raise($.racketCoreError, "property not in struct");
	}
    }

    // Attaches current property with given struct descriptor
    // For simplicity all supers of this property will also be
    // attached directly
    //
    // Lambda associated with each item in supers of this
    // property is called with the result of guard application
    attachToStructTypeDescriptor(desc, v) {
	let newV = v;
	if (this._guard) {
	    newV = this._guard(v, Pair.listFromArray(structTypeInfo(desc)));
	}
	desc._options.props.set(this, newV);
	this._supers.forEach((superEntry) => {
	    let prop = superEntry.hd;
	    let proc = superEntry.tl;
	    prop.attachToStructTypeDescriptor(desc, proc(newV));
	});
    }
}

/*****************************************************************************/

export function makeStructTypeProperty(options) {
    let stProp = StructTypeProperty.make(options);

    return Values.make([
	stProp,
	stProp.getPropertyPredicate(),
	stProp.getPropertyAccessor()
    ]);
}

/*****************************************************************************/

export function makeStructType(options) {
    let descriptor = new StructTypeDescriptor(options);
    return Values.make([
	descriptor,
	descriptor.getStructConstructor(),
	descriptor.getStructPredicate(),
	descriptor.getStructAccessor(),
	descriptor.getStructMutator()
    ])
}

export function isStructType(v) {
    return v instanceof StructTypeDescriptor;
}

export function structTypeInfo(desc) {
    return [
	desc._options.name,
	desc._options.initFieldCount,
	desc._options.autoFieldCount,
	desc.getStructAccessor(),
	desc.getStructMutator(),
	desc._options.immutables, //TODO: What about supers?
	desc._options.superType || false,
	false //TODO: Not sure what this field means?
    ];
}

export function isStructInstance(v) {
    return (v instanceof Struct) ||
	(v instanceof Function) && (v.__rjs_struct_object instanceof Struct);
}

export function check(v, desc) {
    return isStructInstance(v) && v._desc == desc;
}

/*****************************************************************************/
// Properties

export let propProcedure =  makeStructTypeProperty({
    name: "prop:procedure"
}).getAt(0);
