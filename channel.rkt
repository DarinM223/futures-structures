#lang racket/base

(require racket/future)

(provide make-fchannel
         fchannel-put!
         fchannel-take!)
         
(struct channel
  (buffer
   buffer-size
   [buffer-start #:mutable]
   [buffer-end #:mutable]
   buffer-mutex
   fill-count
   empty-count))

(define (make-fchannel size)
  (channel (make-vector size)
           size
           0
           0
           (make-fsemaphore 1)
           (make-fsemaphore 0)
           (make-fsemaphore size)))

(define (fchannel-put! chan elem)
  (define (put! chan elem)
    (vector-set! (channel-buffer chan) (channel-buffer-end chan) elem)
    (set-channel-buffer-end! chan (modulo (+ (channel-buffer-end chan) 1)
                                          (channel-buffer-size chan))))
  (fsemaphore-wait (channel-empty-count chan))
  (fsemaphore-wait (channel-buffer-mutex chan))
  (put! chan elem)
  (fsemaphore-post (channel-buffer-mutex chan))
  (fsemaphore-post (channel-fill-count chan)))

(define (fchannel-take! chan)
  (define (get! chan)
    (define value (vector-ref (channel-buffer chan)
                              (channel-buffer-start chan)))
    (set-channel-buffer-start! chan (modulo (+ (channel-buffer-start chan) 1)
                                            (channel-buffer-size chan)))
    value)
  (fsemaphore-wait (channel-fill-count chan))
  (fsemaphore-wait (channel-buffer-mutex chan))
  (define result (get! chan))
  (fsemaphore-post (channel-buffer-mutex chan))
  (fsemaphore-post (channel-empty-count chan))
  result)

(module+ test
  (require rackunit)
  (require racket/match)

  (define chan (make-fchannel 3))
  (fchannel-put! chan 1)
  (fchannel-put! chan 2)
  (fchannel-put! chan 3)
  (check-equal? (fchannel-take! chan) 1)
  (check-equal? (fchannel-take! chan) 2)
  (check-equal? (fchannel-take! chan) 3)

  (fchannel-put! chan 4)
  (fchannel-put! chan 5)
  (fchannel-put! chan 6)
  (check-equal? (fchannel-take! chan) 4)
  (check-equal? (fchannel-take! chan) 5)
  (check-equal? (fchannel-take! chan) 6)

  (let* ([fchan (make-fchannel 3)]
         [future1 (future
                   (λ ()
                     (fchannel-put! fchan 1)
                     (fchannel-put! fchan 2)
                     (fchannel-put! fchan 3)))]
         [future2 (future
                   (λ ()
                     (fchannel-put! fchan 4)
                     (fchannel-put! fchan 5)
                     (fchannel-put! fchan 6)))]
         [future3 (future
                   (λ () (for/list ([_ 3]) (fchannel-take! fchan))))])
    (check-equal?
     (length (begin
               (touch future1)
               (touch future2)
               (touch future3)))
     3)
    (check-equal?
     (length (for/list ([_ 3]) (fchannel-take! fchan)))
     3)))
