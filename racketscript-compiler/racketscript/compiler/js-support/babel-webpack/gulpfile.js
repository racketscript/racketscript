const gulp = require('gulp');
const babel = require('gulp-babel');
const replace = require('gulp-replace');
const webpackStream = require('webpack-stream');
const webpack = require('webpack');
const UglifyJsPlugin = require("uglifyjs-webpack-plugin");

const target = "~a" + ".rkt.js";

gulp.task('transform', function() {
    return gulp.src(['./**/*.js',
            '!./node_modules/**',
            '!./dist/**',
            '!./*.js'
        ])
        .pipe(webpackStream({
            watch: false,
            plugins: [new UglifyJsPlugin()],
            output: {
                filename: 'compiled.js'
            }
        }))
        .pipe(gulp.dest('dist'));
});

gulp.task('build', gulp.series('transform', function() {}));

gulp.task('default', gulp.series('build', function() {}));
