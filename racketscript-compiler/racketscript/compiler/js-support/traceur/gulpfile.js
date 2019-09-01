const gulp = require('gulp');
const traceur = require('gulp-traceur-cmdline');
const concat = require('gulp-concat');
const replace = require('gulp-replace');
const uglify = require('gulp-uglify');

const target = "~a" + ".rkt.js";

gulp.task('copy-hamt', function() {
    return gulp.src('node_modules/hamt_plus/hamt.js')
        .pipe(replace(/\/\* Export(.*\n)*/m, "\nexport {hamt}"))
	.pipe(gulp.dest("runtime/third-party/"));
});

gulp.task('build', gulp.series('copy-hamt', function() {
    return gulp.src('modules/' + target)
	.pipe(traceur({modules: 'inline'}))
        .pipe(concat('compiled.js'))
        //.pipe(uglify())
	.pipe(gulp.dest('dist'));
}));

gulp.task('default', gulp.series('build', function () {}));
