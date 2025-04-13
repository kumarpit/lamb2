#lang racket
(require racket/cmdline)

(define amb-helpers
  "// --- amb helpers ---
const AmbError = Symbol('AmbError');

function amb(iterable, closure) {
  for (const n of iterable) {
    try {
      return closure(n);
    } catch (e) {
      if (e == AmbError) {
        continue;
      }
    }
  }
  throw AmbError;
}

function fail() {
  throw AmbError;
}

function assert(pred) {
  if (!(pred())) {
    fail();
  }
}
// --- end helpers ---\n\n")

;; TODO: this does not handle nested brackets inside the closure
(define (transform-amb js-code)
  (define amb-re #px"amb\\(([^,]+),\\s*([^\\)]+)\\)\\s*\\{([^}]*)\\}")
  (define match (regexp-match amb-re js-code))
  (if (null? match)
      js-code
      (let* ((full (list-ref match 0))
             (var  (list-ref match 1))
             (iter (list-ref match 2))
             (body (list-ref match 3))
             (replacement
              (format "amb(~a, (~a) => {\n~a\n})"
                      iter
                      var
                      body)))
        (string-append amb-helpers
                       (string-replace js-code full replacement)))))

(define (preprocess-js input-file output-file)
  (define input (file->string input-file))
  (define output (transform-amb input))
  (call-with-output-file output-file
    (λ (out) (fprintf out "~a" output))
    #:exists 'replace))

(define input-file #f)
(define output-file #f)

(command-line
 #:program "preprocess"
 #:args (in out)
 [(set! input-file in)
  (set! output-file out)])
