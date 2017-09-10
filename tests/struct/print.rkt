#lang racket/base

(struct point (x y))
(displayln (point 1 '(#\a #\b)))
(writeln (point 1 '(#\a #\b)))
(println (point 1 '(#\a #\b)))
(parameterize ([print-as-expression #f])
  (println (point 1 '(#\a #\b))))

(struct tpoint (x y) #:transparent)
(displayln (tpoint 1 '(#\a #\b)))
(writeln (tpoint 1 '(#\a #\b)))
(println (tpoint 1 '(#\a #\b)))
(parameterize ([print-as-expression #f])
  (println (tpoint 1 '(#\a #\b))))
