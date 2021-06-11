const gulp = require('gulp');
var closureCompiler = require('google-closure-compiler').gulp();

const target = "~a" + ".rkt.js";

gulp.task('build',  function() {
    return gulp.src(['./**/*.js',
                     '!./node_modules/**',
                     '!./dist/**',
                     '!./*.js'])
        .pipe(closureCompiler({
            compilation_level: 'SIMPLE',
            warning_level: 'VERBOSE',
            language_in: 'ECMASCRIPT6_STRICT',
            language_out: 'ECMASCRIPT5_STRICT',
            output_wrapper: '(function(){\n%output%\n}).call(this)',
            js_output_file: 'compiled.js'
        }))
        .pipe(gulp.dest('dist'));
});

gulp.task('default', gulp.series('build', function () {}));
