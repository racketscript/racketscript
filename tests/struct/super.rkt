#lang racket

(struct document (author title content))
(struct book document (publisher))
(struct paper (journal) #:super struct:document)

(define d1 (document "John Doe" "Life of John Doe" "Is unknown"))
(document? d1)
(book? d1)
(paper? d1)
(document-author d1)
(document-title d1)
(document-content d1)

(define b1 (book "John Doe" "Life of John Doe" "Is unknown" "Publisher 1"))
(document? b1)
(book? b1)
(paper? b1)
(document-author b1)
(document-title b1)
(document-content b1)
(book-publisher b1)


(define p1 (paper "John Doe" "Life of Joh Doe" "Is unknown" "Journal 1"))
(document? p1)
(book? p1)
(paper? p1)
(document-author p1)
(document-title p1)
(document-content p1)
(paper-journal p1)
