const gulp = require('gulp');
const babel = require('gulp-babel');
const uglify = require('gulp-uglify');

const target = "~a" + ".rkt.js";

gulp.task('transform', function() {
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
});

gulp.task('build', gulp.series('transform', function (done) {
    done();
}));

gulp.task('default', gulp.series('build', function (done) {
    done();
}));
