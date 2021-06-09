module.exports = {
    TextEncoder: (typeof window === 'undefined')
        ? require('util').TextEncoder
        : window.TextEncoder,
    TextDecoder: (typeof window === 'undefined')
        ? require('util').TextDecoder
        : window.TextDecoder,
}
