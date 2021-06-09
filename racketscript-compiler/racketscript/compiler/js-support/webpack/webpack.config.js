import webpack from 'webpack'
const target = "./modules/~a" + ".rkt.js";

export default {
    entry: target,
    output: {
        filename: 'main.js',
    },
    mode: "production",
    target: "web",
    experiments: {
        topLevelAwait: true
    }
};
