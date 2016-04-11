const gulp = require('gulp');
const traceur = require('gulp-traceur-cmdline');
const concat = require('gulp-concat');
const insert = require('gulp-insert');

const target = "~a" + ".rkt.js";

gulp.task('default', function() {
    return gulp.src('modules/' + target)
	.pipe(traceur({modules: 'inline'}))
        .pipe(concat('compiled.js'))
	.pipe(gulp.dest('dist'));
});
