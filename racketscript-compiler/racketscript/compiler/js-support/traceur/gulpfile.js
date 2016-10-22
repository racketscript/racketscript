const gulp = require('gulp');
const traceur = require('gulp-traceur-cmdline');
const concat = require('gulp-concat');
const insert = require('gulp-insert');
const replace = require('gulp-replace');

const target = "~a" + ".rkt.js";

gulp.task('copy-hamt', function() {
    return gulp.src('node_modules/hamt_plus/hamt.js')
        .pipe(replace("undefined.hamt = hamt;", ""))
	.pipe(insert.append("\nexport {hamt}"))
	.pipe(gulp.dest("runtime/third-party/"));
});

gulp.task('build', ['copy-hamt'], function() {
    return gulp.src('modules/' + target)
	.pipe(traceur({modules: 'inline'}))
        .pipe(concat('compiled.js'))
	.pipe(gulp.dest('dist'));
});

gulp.task('default', ['build']);
