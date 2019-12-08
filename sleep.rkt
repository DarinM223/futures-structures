#lang racket/base

(define (fsleep sec)
  (define saved (+ (current-inexact-milliseconds) (* sec 1000)))
  (let loop ()
    (when (< (current-inexact-milliseconds) saved)
      (loop))))
