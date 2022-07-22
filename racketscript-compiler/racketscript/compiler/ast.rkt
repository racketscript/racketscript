#lang racket/base

(struct Linklet (imports exports forms) #:transparent)

(struct DefineValues (ids expr))


