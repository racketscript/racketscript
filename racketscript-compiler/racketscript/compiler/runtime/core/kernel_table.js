/* eslint-disable */
// import * as Kernel from '../core.js';

export const kernelTable = {
    writeToPort:         () => { throw new Error('writeToPort not supported'); },
    displayNativeString: () => { throw new Error('displayNativeString not supported'); },
    writeNativeString:   () => { throw new Error('writeNativeString not supported'); },
    toRawString:         () => { throw new Error('toRawString not supported'); },
    isImmutable:         () => { return true; },
    ref:                 (k, fail) => {
                             throw new Error(`primitive ${k} not currently supported`);
                         },
    toRawString:         () => { throw new Error('hasKey not supported'); },
    refKey:              () => { return kernelTable.ref(); },
    set:                 () => { throw new Error('set not supported'); },
    remove:              () => { throw new Error('remove not supported'); },
    doset:               () => { throw new Error('doset not supported'); },
    doremove:            () => { throw new Error('doremove not supported'); },
    size:                () => { throw new Error('size not supported'); },
    iterateFirst:        () => { throw new Error('iterateFirst not supported'); },
    iterateNext:         () => { throw new Error('iterateNext not supported'); },
    iterateKey:          () => { throw new Error('iterateKey not supported'); },
    iterateValue:        () => { throw new Error('iterateValue not supported'); },
    iteratePair:         () => { throw new Error('iteratePair not supported'); },
    iterateKeyValue:     () => { throw new Error('iterateKeyValue not supported'); },
    union:               () => { throw new Error('union not supported'); },
    isSameType:          () => { throw new Error('isSameType not supported'); },
    isKeysSubset:        () => { throw new Error('isKeysSubset not supported'); },
    equals:              () => { throw new Error('equals not supported'); },
};
