const gulp = require('gulp');
const babel = require('gulp-babel');
const replace = require('gulp-replace');
const uglify = require('gulp-uglify');

const target = "~a" + ".rkt.js";

gulp.task('copy-hamt', function() {
    return gulp.src('node_modules/hamt_plus/hamt.js')
        .pipe(replace(/\/\* Export(.*\n)*/m, "\nexport {hamt}"))
	.pipe(gulp.dest("runtime/third-party/"));
});

gulp.task('transform', gulp.series('copy-hamt', function() {
    return gulp.src(['./**/*.js',
		     '!./node_modules/**',
		     '!./dist/**',
		     '!./*.js'])
	.pipe(babel({
	    presets: ["@babel/preset-env"],
	    plugins: ["@babel/plugin-transform-runtime"]
	}))
    .pipe(uglify())
    .pipe(gulp.dest('dist'));
}));

gulp.task('build', gulp.series('transform', function (done) {
    done();
}));

gulp.task('default', gulp.series('build', function (done) {
    done();
}));
