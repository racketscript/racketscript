const gulp = require('gulp');
const traceur = require('gulp-traceur');
const concat = require('gulp-concat');
const insert = require('gulp-insert');

gulp.task('default', function() {
    return gulp.src('modules/*.js')
	.pipe(traceur({modules: 'bootstrap'}))
        .pipe(concat('compiled.js'))
	.pipe(insert.append('\n\n$traceurRuntime.getModule("~a.js");'))
	.pipe(gulp.dest('dist'));
});
