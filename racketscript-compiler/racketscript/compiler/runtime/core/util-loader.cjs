// This file exists, because webpack complains about using `require` when using ES6
// modules.

const { TextEncoder, TextDecoder } = typeof window === 'undefined' ? require('util') : window;

module.exports = { TextEncoder, TextDecoder };
