const gulp = require('gulp');
const babel = require('gulp-babel');
const replace = require('gulp-replace');
const babelDeps = require("babel-deps");
const strip = require('gulp-strip-comments');

const target = "~a" + ".rkt.js";

gulp.task('copy-hamt', function() {
    return gulp.src('node_modules/hamt_plus/hamt.js')
        .pipe(replace(/\/\* Export(.*\n)*/m, "\nexport {hamt}"))
	.pipe(gulp.dest("runtime/third-party/"));
});

gulp.task('build', ['copy-hamt'], function() {
    return gulp.src(['./**/*.js',
		     '!./node_modules/**',
		     '!./dist/**',
		     '!./*.js'])
	.pipe(babel({
	    presets: ["es2015", "babel-polyfill"],
	    plugins:["babel-plugin-transform-helper"]
	}))
        .pipe(strip())
	.pipe(gulp.dest('dist'));
});

gulp.task('default', ['build']);
