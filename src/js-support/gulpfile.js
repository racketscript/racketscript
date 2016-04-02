const gulp = require('gulp');
const traceur = require('gulp-traceur');
const concat = require('gulp-concat');

gulp.task('default', function() {
    return gulp.src('modules/*.js')
	.pipe(traceur({modules: 'inline'}))
        .pipe(concat('compiled.js'))
	.pipe(gulp.dest('dist'));
});
