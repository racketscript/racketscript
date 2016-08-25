const gulp = require('gulp');
const babel = require('gulp-babel');
const concat = require('gulp-concat');
const insert = require('gulp-insert');
const babelDeps = require("babel-deps");

const target = "~a" + ".rkt.js";

gulp.task('copy-hamt', function() {
    return gulp.src('node_modules/hamt_plus/hamt.js')
	.pipe(insert.append("\nexport {hamt}"))
	.pipe(gulp.dest("runtime/third-party/"));
});

gulp.task('build', ['copy-hamt'], function() {
    return gulp.src(['./**/*.js',
		     '!./node_modules/**',
		     '!./dist/**',
		     '!./*.js'])
	.pipe(babel({
	    presets: ["es2015", "babel-polyfill"]
	}))
	.pipe(gulp.dest('dist'));
});

gulp.task('default', ['build']);
