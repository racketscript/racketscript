/* eslint-disable */
import { makeEqual } from './hash.js';

export const kernelTable = makeEqual([], true);
export const unsafeSet   = (k, v) => { kernelTable.doset(k, v) };
