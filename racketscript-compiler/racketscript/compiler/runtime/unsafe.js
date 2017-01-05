import * as Core from "./core.js";

/* --------------------------------------------------------------------------*/
// All exports go in exports

export const exports = {};

/* --------------------------------------------------------------------------*/
// Pairs

exports["unsafe-car"] = (v) => v.hd;
exports["unsafe-cdr"] = (v) => v.tl;

/* --------------------------------------------------------------------------*/
// Strutures

exports["unsafe-struct-ref"] = (v, k) => v._fields[k];

/* --------------------------------------------------------------------------*/
// Numbers

exports["unsafe-fx<"] = Core.Number.lt;
exports["unsafe-fx+"] = Core.Number.add;
