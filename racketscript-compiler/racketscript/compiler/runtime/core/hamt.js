// Copyright (C) 2016-2021 Matt Bierner, RacketScript Authors
//
// Original source and copyright clause: https://github.com/mattbierner/hamt_plus

const _typeof = typeof Symbol === 'function' && typeof Symbol.iterator === 'symbol'
    ? function (obj) { return typeof obj; }
    : function (obj) {
        return obj && typeof Symbol === 'function' && obj.constructor === Symbol && obj !== Symbol.prototype
            ? 'symbol'
            : typeof obj;
    };

/**
    @fileOverview Hash Array Mapped Trie.

    Code based on: https://github.com/exclipy/pdata
*/
const hamt = {}; // export

/* Configuration
 ***************************************************************************** */
const SIZE = 5;

const BUCKET_SIZE = 2 ** SIZE;

const MASK = BUCKET_SIZE - 1;

const MAX_INDEX_NODE = BUCKET_SIZE / 2;

const MIN_ARRAY_NODE = BUCKET_SIZE / 4;

/*
 ***************************************************************************** */
const nothing = {};

const constant = function constant(x) {
    return function () {
        return x;
    };
};

/**
    Get 32 bit hash of string.

    Based on:
    http://stackoverflow.com/questions/7616461/generate-a-hash-from-string-in-javascript-jquery
*/
hamt.hash = function (str) {
    const type = typeof str === 'undefined' ? 'undefined' : _typeof(str);
    if (type === 'number') return str;
    if (type !== 'string') str += '';

    let hash = 0;
    for (let i = 0, len = str.length; i < len; ++i) {
        const c = str.charCodeAt(i);
        hash = (hash << 5) - hash + c | 0;
    }
    return hash;
};

/* Bit Ops
 ***************************************************************************** */
/**
    Hamming weight.

    Taken from: http://jsperf.com/hamming-weight
*/
const popcount = function popcount(x) {
    x -= x >> 1 & 0x55555555;
    x = (x & 0x33333333) + (x >> 2 & 0x33333333);
    x = x + (x >> 4) & 0x0f0f0f0f;
    x += x >> 8;
    x += x >> 16;
    return x & 0x7f;
};

const hashFragment = function hashFragment(shift, h) {
    return h >>> shift & MASK;
};

const toBitmap = function toBitmap(x) {
    return 1 << x;
};

const fromBitmap = function fromBitmap(bitmap, bit) {
    return popcount(bitmap & bit - 1);
};

/* Array Ops
 ***************************************************************************** */
/**
    Set a value in an array.

    @param mutate Should the input array be mutated?
    @param at Index to change.
    @param v New value
    @param arr Array.
*/
const arrayUpdate = function arrayUpdate(mutate, at, v, arr) {
    let out = arr;
    if (!mutate) {
        const len = arr.length;
        out = new Array(len);
        for (let i = 0; i < len; ++i) {
            out[i] = arr[i];
        }
    }
    out[at] = v;
    return out;
};

/**
    Remove a value from an array.

    @param mutate Should the input array be mutated?
    @param at Index to remove.
    @param arr Array.
*/
const arraySpliceOut = function arraySpliceOut(mutate, at, arr) {
    const newLen = arr.length - 1;
    let i = 0;
    let g = 0;
    let out = arr;
    if (mutate) {
        i = at;
        g = at;
    } else {
        out = new Array(newLen);
        while (i < at) {
            out[g++] = arr[i++];
        }
    }
    ++i;
    while (i <= newLen) {
        out[g++] = arr[i++];
    } if (mutate) {
        out.length = newLen;
    }
    return out;
};

/**
    Insert a value into an array.

    @param mutate Should the input array be mutated?
    @param at Index to insert at.
    @param v Value to insert,
    @param arr Array.
*/
const arraySpliceIn = function arraySpliceIn(mutate, at, v, arr) {
    const len = arr.length;
    if (mutate) {
        let _i = len;
        while (_i >= at) {
            arr[_i--] = arr[_i];
        }arr[at] = v;
        return arr;
    }
    let i = 0;
    let g = 0;
    const out = new Array(len + 1);
    while (i < at) {
        out[g++] = arr[i++];
    }out[at] = v;
    while (i < len) {
        out[++g] = arr[i++];
    } return out;
};

/* Node Structures
 ***************************************************************************** */
const LEAF = 1;
const COLLISION = 2;
const INDEX = 3;
const ARRAY = 4;

/**
    Empty node.
*/
const empty = {
    __hamt_isEmpty: true
};

const isEmptyNode = function isEmptyNode(x) {
    return x === empty || x && x.__hamt_isEmpty;
};

/**
    Leaf holding a value.

    @member edit Edit of the node.
    @member hash Hash of key.
    @member key Key.
    @member value Value stored.
*/
const Leaf = function Leaf(edit, hash, key, value) {
    return {
        type: LEAF,
        edit,
        hash,
        key,
        value,
        // eslint-disable-next-line no-use-before-define
        _modify: LeafModify
    };
};

/**
    Leaf holding multiple values with the same hash but different keys.

    @member edit Edit of the node.
    @member hash Hash of key.
    @member children Array of collision children node.
*/
const Collision = function Collision(edit, hash, children) {
    return {
        type: COLLISION,
        edit,
        hash,
        children,
        // eslint-disable-next-line no-use-before-define
        _modify: CollisionModify
    };
};

/**
    Internal node with a sparse set of children.

    Uses a bitmap and array to pack children.

  @member edit Edit of the node.
    @member mask Bitmap that encode the positions of children in the array.
    @member children Array of child nodes.
*/
const IndexedNode = function IndexedNode(edit, mask, children) {
    return {
        type: INDEX,
        edit,
        mask,
        children,
        // eslint-disable-next-line no-use-before-define
        _modify: IndexedNodeModify
    };
};

/**
    Internal node with many children.

    @member edit Edit of the node.
    @member size Number of children.
    @member children Array of child nodes.
*/
const ArrayNode = function ArrayNode(edit, size, children) {
    return {
        type: ARRAY,
        edit,
        size,
        children,
        // eslint-disable-next-line no-use-before-define
        _modify: ArrayNodeModify
    };
};

/**
    Is `node` a leaf node?
*/
const isLeaf = function isLeaf(node) {
    return node === empty || node.type === LEAF || node.type === COLLISION;
};

/* Internal node operations.
 ***************************************************************************** */
/**
    Expand an indexed node into an array node.

  @param edit Current edit.
    @param frag Index of added child.
    @param child Added child.
    @param mask Index node mask before child added.
    @param subNodes Index node children before child added.
*/
const expand = function expand(edit, frag, child, bitmap, subNodes) {
    const arr = [];
    let bit = bitmap;
    let count = 0;
    for (let i = 0; bit; ++i) {
        if (bit & 1) arr[i] = subNodes[count++];
        bit >>>= 1;
    }
    arr[frag] = child;
    return ArrayNode(edit, count + 1, arr);
};

/**
    Collapse an array node into a indexed node.

  @param edit Current edit.
    @param count Number of elements in new array.
    @param removed Index of removed element.
    @param elements Array node children before remove.
*/
const pack = function pack(edit, count, removed, elements) {
    const children = new Array(count - 1);
    let g = 0;
    let bitmap = 0;
    for (let i = 0, len = elements.length; i < len; ++i) {
        if (i !== removed) {
            const elem = elements[i];
            if (elem && !isEmptyNode(elem)) {
                children[g++] = elem;
                bitmap |= 1 << i;
            }
        }
    }
    return IndexedNode(edit, bitmap, children);
};

/**
    Merge two leaf nodes.

    @param shift Current shift.
    @param h1 Node 1 hash.
    @param n1 Node 1.
    @param h2 Node 2 hash.
    @param n2 Node 2.
*/
const mergeLeaves = function mergeLeaves(edit, shift, h1, n1, h2, n2) {
    if (h1 === h2) return Collision(edit, h1, [n2, n1]);

    const subH1 = hashFragment(shift, h1);
    const subH2 = hashFragment(shift, h2);
    // eslint-disable-next-line no-nested-ternary,max-len
    return IndexedNode(edit, toBitmap(subH1) | toBitmap(subH2), subH1 === subH2 ? [mergeLeaves(edit, shift + SIZE, h1, n1, h2, n2)] : subH1 < subH2 ? [n1, n2] : [n2, n1]);
};

/**
    Update an entry in a collision list.

    @param mutate Should mutation be used?
    @param edit Current edit.
    @param keyEq Key compare function.
    @param hash Hash of collision.
    @param list Collision list.
    @param f Update function.
    @param k Key to update.
    @param size Size ref.
*/
const updateCollisionList = function updateCollisionList(mutate, edit, keyEq, h, list, f, k, size) {
    const len = list.length;
    for (let i = 0; i < len; ++i) {
        const child = list[i];
        if (keyEq(k, child.key)) {
            const { value } = child;
            const _newValue = f(value);
            if (_newValue === value) return list;

            if (_newValue === nothing) {
                --size.value;
                return arraySpliceOut(mutate, i, list);
            }
            return arrayUpdate(mutate, i, Leaf(edit, h, k, _newValue), list);
        }
    }

    const newValue = f();
    if (newValue === nothing) return list;
    ++size.value;
    return arrayUpdate(mutate, len, Leaf(edit, h, k, newValue), list);
};

const canEditNode = function canEditNode(edit, node) {
    return edit === node.edit;
};

/* Editing
 ***************************************************************************** */
let LeafModify = function LeafModify(edit, keyEq, shift, f, h, k, size) {
    if (keyEq(k, this.key)) {
        const _v = f(this.value);
        if (_v === this.value) return this; else if (_v === nothing) {
            --size.value;
            return empty;
        }
        if (canEditNode(edit, this)) {
            this.value = _v;
            return this;
        }
        return Leaf(edit, h, k, _v);
    }
    const v = f();
    if (v === nothing) return this;
    ++size.value;
    return mergeLeaves(edit, shift, this.hash, this, h, Leaf(edit, h, k, v));
};

let CollisionModify = function CollisionModify(edit, keyEq, shift, f, h, k, size) {
    if (h === this.hash) {
        const canEdit = canEditNode(edit, this);
        const list = updateCollisionList(canEdit, edit, keyEq, this.hash, this.children, f, k, size);
        if (list === this.children) return this;

        return list.length > 1 ? Collision(edit, this.hash, list) : list[0]; // collapse single element collision list
    }
    const v = f();
    if (v === nothing) return this;
    ++size.value;
    return mergeLeaves(edit, shift, this.hash, this, h, Leaf(edit, h, k, v));
};

let IndexedNodeModify = function IndexedNodeModify(edit, keyEq, shift, f, h, k, size) {
    const { children, mask } = this;
    const frag = hashFragment(shift, h);
    const bit = toBitmap(frag);
    const indx = fromBitmap(mask, bit);
    const exists = mask & bit;
    const current = exists ? children[indx] : empty;
    const child = current._modify(edit, keyEq, shift + SIZE, f, h, k, size);

    if (current === child) return this;

    const canEdit = canEditNode(edit, this);
    let bitmap = mask;
    let newChildren;
    if (exists && isEmptyNode(child)) {
        // remove
        bitmap &= ~bit;
        if (!bitmap) return empty;
        if (children.length <= 2 && isLeaf(children[indx ^ 1])) return children[indx ^ 1]; // collapse

        newChildren = arraySpliceOut(canEdit, indx, children);
    } else if (!exists && !isEmptyNode(child)) {
        // add
        if (children.length >= MAX_INDEX_NODE) return expand(edit, frag, child, mask, children);

        bitmap |= bit;
        newChildren = arraySpliceIn(canEdit, indx, child, children);
    } else {
        // modify
        newChildren = arrayUpdate(canEdit, indx, child, children);
    }

    if (canEdit) {
        this.mask = bitmap;
        this.children = newChildren;
        return this;
    }
    return IndexedNode(edit, bitmap, newChildren);
};

let ArrayNodeModify = function ArrayNodeModify(edit, keyEq, shift, f, h, k, size) {
    let count = this.size;
    const { children } = this;
    const frag = hashFragment(shift, h);
    const child = children[frag];
    const newChild = (child || empty)._modify(edit, keyEq, shift + SIZE, f, h, k, size);

    if (child === newChild) return this;

    const canEdit = canEditNode(edit, this);
    let newChildren;
    if (isEmptyNode(child) && !isEmptyNode(newChild)) {
        // add
        ++count;
        newChildren = arrayUpdate(canEdit, frag, newChild, children);
    } else if (!isEmptyNode(child) && isEmptyNode(newChild)) {
        // remove
        --count;
        if (count <= MIN_ARRAY_NODE) return pack(edit, count, frag, children);
        newChildren = arrayUpdate(canEdit, frag, empty, children);
    } else {
        // modify
        newChildren = arrayUpdate(canEdit, frag, newChild, children);
    }

    if (canEdit) {
        this.size = count;
        this.children = newChildren;
        return this;
    }
    return ArrayNode(edit, count, newChildren);
};

empty._modify = function (edit, keyEq, shift, f, h, k, size) {
    const v = f();
    if (v === nothing) return empty;
    ++size.value;
    return Leaf(edit, h, k, v);
};

/*
 ***************************************************************************** */
function Map(editable, edit, config, root, size) {
    this._editable = editable;
    this._edit = edit;
    this._config = config;
    this._root = root;
    this._size = size;
}

Map.prototype.setTree = function (newRoot, newSize) {
    if (this._editable) {
        this._root = newRoot;
        this._size = newSize;
        return this;
    }
    return newRoot === this._root ? this : new Map(this._editable, this._edit, this._config, newRoot, newSize);
};

/* Queries
 ***************************************************************************** */
/**
    Lookup the value for `key` in `map` using a custom `hash`.

    Returns the value or `alt` if none.
*/
hamt.tryGetHash = (alt, hash, key, map) => {
    let node = map._root;
    let shift = 0;
    const { keyEq } = map._config;
    while (true) {
        switch (node.type) {
        case LEAF:
        {
            return keyEq(key, node.key) ? node.value : alt;
        }
        case COLLISION:
        {
            if (hash === node.hash) {
                for (let i = 0, len = node.children.length; i < len; ++i) {
                    const child = node.children[i];
                    if (keyEq(key, child.key)) return child.value;
                }
            }
            return alt;
        }
        case INDEX:
        {
            const frag = hashFragment(shift, hash);
            const bit = toBitmap(frag);
            if (node.mask & bit) {
                node = node.children[fromBitmap(node.mask, bit)];
                shift += SIZE;
                break;
            }
            return alt;
        }
        case ARRAY:
        {
            node = node.children[hashFragment(shift, hash)];
            if (node) {
                shift += SIZE;
                break;
            }
            return alt;
        }
        default:
            return alt;
        }
    }
};

Map.prototype.tryGetHash = function (alt, hash, key) {
    return hamt.tryGetHash(alt, hash, key, this);
};

/**
    Lookup the value for `key` in `map` using internal hash function.

    @see `tryGetHash`
*/
hamt.tryGet = (alt, key, map) =>
    hamt.tryGetHash(alt, map._config.hash(key), key, map);

Map.prototype.tryGet = function (alt, key) {
    return hamt.tryGet(alt, key, this);
};

/**
    Lookup the value for `key` in `map` using a custom `hash`.

    Returns the value or `undefined` if none.
*/
hamt.getHash = (hash, key, map) =>
    hamt.tryGetHash(undefined, hash, key, map);

Map.prototype.getHash = function (hash, key) {
    return hamt.getHash(hash, key, this);
};

/**
    Lookup the value for `key` in `map` using internal hash function.

    @see `get`
*/
hamt.get = (key, map) =>
    hamt.tryGetHash(undefined, map._config.hash(key), key, map);

Map.prototype.get = function (key, alt) {
    return hamt.tryGet(alt, key, this);
};

/**
    Does an entry exist for `key` in `map`? Uses custom `hash`.
*/
hamt.hasHash = (hash, key, map) => hamt.tryGetHash(nothing, hash, key, map) !== nothing;


Map.prototype.hasHash = function (hash, key) {
    return hamt.hasHash(hash, key, this);
};

/**
    Does an entry exist for `key` in `map`? Uses internal hash function.
*/
const has = (key, map) => hamt.hasHash(map._config.hash(key), key, map);


Map.prototype.has = function (key) {
    return has(key, this);
};

const defKeyCompare = function defKeyCompare(x, y) {
    return x === y;
};

/**
    Create an empty map.

    @param config Configuration.
*/
hamt.make = function (config) {
    return new Map(0, 0, {
        keyEq: config && config.keyEq || defKeyCompare,
        hash: config && config.hash || hamt.hash
    }, empty, 0);
};

/**
    Empty map.
*/
hamt.empty = hamt.make();

/**
    Does `map` contain any elements?
*/
hamt.isEmpty = function (map) {
    return map && !!isEmptyNode(map._root);
};

Map.prototype.isEmpty = function () {
    return hamt.isEmpty(this);
};

/* Updates
 ***************************************************************************** */
/**
    Alter the value stored for `key` in `map` using function `f` using
    custom hash.

    `f` is invoked with the current value for `k` if it exists,
    or no arguments if no such value exists. `modify` will always either
    update or insert a value into the map.

    Returns a map with the modified value. Does not alter `map`.
*/
hamt.modifyHash = function (f, hash, key, map) {
    const size = { value: map._size };
    const newRoot = map._root._modify(map._editable ? map._edit : NaN, map._config.keyEq, 0, f, hash, key, size);

    return map.setTree(newRoot, size.value);
};

Map.prototype.modifyHash = function (hash, key, f) {
    return hamt.modifyHash(f, hash, key, this);
};

/**
    Alter the value stored for `key` in `map` using function `f` using
    internal hash function.

    @see `modifyHash`
*/
hamt.modify = (f, key, map) => hamt.modifyHash(f, map._config.hash(key), key, map);

Map.prototype.modify = function (key, f) {
    return hamt.modify(f, key, this);
};

/**
    Store `value` for `key` in `map` using custom `hash`.

    Returns a map with the modified value. Does not alter `map`.
*/
hamt.setHash = (hash, key, value, map) => hamt.modifyHash(constant(value), hash, key, map);

Map.prototype.setHash = function (hash, key, value) {
    return hamt.setHash(hash, key, value, this);
};

/**
    Store `value` for `key` in `map` using internal hash function.

    @see `setHash`
*/
hamt.set = (key, value, map) => hamt.setHash(map._config.hash(key), key, value, map);

Map.prototype.set = function (key, value) {
    return hamt.set(key, value, this);
};

/**
    Remove the entry for `key` in `map`.

    Returns a map with the value removed. Does not alter `map`.
*/
const del = constant(nothing);
hamt.removeHash = (hash, key, map) => hamt.modifyHash(del, hash, key, map);

Map.prototype.removeHash = function (hash, key) {
    return hamt.removeHash(hash, key, this);
};

Map.prototype.deleteHash = Map.prototype.removeHash;

/**
    Remove the entry for `key` in `map` using internal hash function.

    @see `removeHash`
*/
hamt.remove = (key, map) => hamt.removeHash(map._config.hash(key), key, map);

Map.prototype.remove = function (key) {
    return hamt.remove(key, this);
};

Map.prototype.delete = Map.prototype.remove;

/* Mutation
 ***************************************************************************** */
/**
    Mark `map` as mutable.
 */
hamt.beginMutation = map => new Map(map._editable + 1, map._edit + 1, map._config, map._root, map._size);

Map.prototype.beginMutation = function () {
    return hamt.beginMutation(this);
};

/**
    Mark `map` as immutable.
 */
hamt.endMutation = function (map) {
    map._editable = map._editable && map._editable - 1;
    return map;
};

Map.prototype.endMutation = function () {
    return hamt.endMutation(this);
};

/**
    Mutate `map` within the context of `f`.
    @param f
    @param map HAMT
*/
hamt.mutate = function (f, map) {
    const transient = hamt.beginMutation(map);
    f(transient);
    return hamt.endMutation(transient);
};

Map.prototype.mutate = function (f) {
    return hamt.mutate(f, this);
};

/* Traversal
 ***************************************************************************** */
/**
    Apply a continuation.
*/
const appk = function appk(k) {
    // eslint-disable-next-line no-use-before-define
    return k && lazyVisitChildren(k[0], k[1], k[2], k[3], k[4]);
};

/**
    Recursively visit all values stored in an array of nodes lazily.
*/
const lazyVisitChildren = function lazyVisitChildren(len, children, i, f, k) {
    while (i < len) {
        const child = children[i++];
        // eslint-disable-next-line no-use-before-define
        if (child && !isEmptyNode(child)) return lazyVisit(child, f, [len, children, i, f, k]);
    }
    return appk(k);
};

/**
    Recursively visit all values stored in `node` lazily.
*/
const lazyVisit = function lazyVisit(node, f, k) {
    switch (node.type) {
    case LEAF:
        return {
            value: f(node),
            rest: k
        };

    case COLLISION:
    case ARRAY:
    case INDEX:
        return lazyVisitChildren(node.children.length, node.children, 0, f, k);

    default:
        return appk(k);
    }
};

const DONE = {
    done: true
};

/**
    Javascript iterator over a map.
*/
function MapIterator(v) {
    this.v = v;
}

MapIterator.prototype.next = function () {
    if (!this.v) return DONE;
    const v0 = this.v;
    this.v = appk(v0.rest);
    return v0;
};

MapIterator.prototype[Symbol.iterator] = function () {
    return this;
};

/**
    Lazily visit each value in map with function `f`.
*/
const visit = function visit(map, f) {
    return new MapIterator(lazyVisit(map._root, f));
};

/**
    Get a Javascsript iterator of `map`.

    Iterates over `[key, value]` arrays.
*/
hamt.entries = map => visit(map, x => [x.key, x.value]);

Map.prototype.entries = function () {
    return hamt.entries(this);
};

Map.prototype[Symbol.iterator] = Map.prototype.entries;

/**
    Get array of all keys in `map`.

    Order is not guaranteed.
*/
hamt.keys = map => visit(map, x => x.key);

Map.prototype.keys = function () {
    return hamt.keys(this);
};

/**
    Get array of all values in `map`.

    Order is not guaranteed, duplicates are preserved.
*/
hamt.values = map => visit(map, x => x.value);

Map.prototype.values = function () {
    return hamt.values(this);
};

/* Fold
 ***************************************************************************** */
/**
    Visit every entry in the map, aggregating data.

    Order of nodes is not guaranteed.

    @param f Function mapping accumulated value, value, and key to new value.
    @param z Starting value.
    @param m HAMT
*/
hamt.fold = function (f, z, m) {
    const root = m._root;
    if (root.type === LEAF) return f(z, root.value, root.key);

    const toVisit = [root.children];
    let children = toVisit.pop();
    while (children) {
        for (let i = 0, len = children.length; i < len;) {
            const child = children[i++];
            if (child && child.type) {
                if (child.type === LEAF) z = f(z, child.value, child.key); else toVisit.push(child.children);
            }
        }

        children = toVisit.pop();
    }
    return z;
};

Map.prototype.fold = function (f, z) {
    return hamt.fold(f, z, this);
};

/**
    Visit every entry in the map, aggregating data.

    Order of nodes is not guaranteed.

    @param f Function invoked with value and key
    @param map HAMT
*/
hamt.forEach = (f, map) => hamt.fold((_, value, key) => f(value, key, map), null, map);

Map.prototype.forEach = function (f) {
    return hamt.forEach(f, this);
};

/* Aggregate
 ***************************************************************************** */
/**
    Get the number of entries in `map`.
*/
hamt.count = map => map._size;

Map.prototype.count = function () {
    return hamt.count(this);
};

Object.defineProperty(Map.prototype, 'size', {
    get: Map.prototype.count
});

export { hamt };
// # sourceMappingURL=hamt.js.map
