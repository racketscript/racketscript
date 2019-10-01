import { racketCoreError } from './errors.js';

/* Arithmetic */

export function add(...operands) {
    return [].reduce.call(operands, (a, b) => a + b, 0);
}

export function sub(...operands) {
    if (operands.length === 1) {
        return -operands[0];
    }
    let result = operands[0];
    for (let i = 1; i < operands.length; ++i) {
        result -= operands[i];
    }
    return result;
}

export function mul(...operands) {
    return [].reduce.call(operands, (a, b) => a * b, 1);
}

export function div(...operands) {
    if (operands.length === 1) {
        return 1 / operands[0];
    }
    let result = operands[0];
    for (let i = 1; i < operands.length; ++i) {
        result /= operands[i];
    }
    return result;
}

/* Comparison */

export function compare(cmp, operands) {
    if (operands.length < 2) {
        throw racketCoreError('compare: at least 2 arguments required, given', ...operands);
    }
    for (let i = 1; i < operands.length; i++) {
        if (!cmp(operands[i - 1], operands[i])) {
            return false;
        }
    }
    return true;
}

export function lt(...operands) {
    return compare((a, b) => a < b, operands);
}

export function lte(...operands) {
    return compare((a, b) => a <= b, operands);
}

export function gt(...operands) {
    return compare((a, b) => a > b, operands);
}

export function gte(...operands) {
    return compare((a, b) => a >= b, operands);
}

export function equals(...operands) {
    return compare((a, b) => a === b, operands);
}

export function check(v) {
    return typeof v === 'number';
}

// copied from internet:
// https://gist.github.com/jlbruno/1535691/db35b4f3af3dcbb42babc01541410f291a8e8fac
export function toOrdinal(i) {
    var j = i % 10,
        k = i % 100;
    if (j == 1 && k != 11) {
        return i + "st";
    }
    if (j == 2 && k != 12) {
        return i + "nd";
    }
    if (j == 3 && k != 13) {
        return i + "rd";
    }
    return i + "th";
}
