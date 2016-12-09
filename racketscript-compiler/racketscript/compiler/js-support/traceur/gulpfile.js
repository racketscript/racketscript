const gulp = require('gulp');
const traceur = require('gulp-traceur-cmdline');
const concat = require('gulp-concat');
const replace = require('gulp-replace');
const strip = require('gulp-strip-comments');

const target = "~a" + ".rkt.js";

gulp.task('copy-hamt', function() {
    return gulp.src('node_modules/hamt_plus/hamt.js')
        .pipe(replace(/\/\* Export(.*\n)*/m, "\nexport {hamt}"))
	.pipe(gulp.dest("runtime/third-party/"));
});

gulp.task('build', ['copy-hamt'], function() {
    return gulp.src('modules/' + target)
	.pipe(traceur({modules: 'inline'}))
        .pipe(concat('compiled.js'))
        .pipe(strip())
	.pipe(gulp.dest('dist'));
});

gulp.task('default', ['build']);
