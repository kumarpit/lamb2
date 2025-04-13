#lang racket
(require racket/cmdline)

(define amb-helpers
  "// --- amb helpers ---
const AmbError = Symbol('AmbError');

function amb(iterable, closure) {
  let it = iterable[Symbol.iterator]();
  for (const n of it) {
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

(define (transform-amb js-code)
  (define amb-re #px"amb\\(([^,]+),\\s*([^\\)]+)\\)\\s*\\{([^}]*)\\}")
  (define matches (regexp-match amb-re js-code))
  (displayln matches)
  ;; Only prepend helper code if we actually matched
  (if (null? matches)
      js-code
      (let loop ((result js-code) (ms matches))
        (if (null? ms)
            (string-append amb-helpers result)
            (let* ((match ms)
                   (full (list-ref match 0))
                   (var  (list-ref match 1))
                   (iter (list-ref match 2))
                   (body (list-ref match 3))
                   (replacement (format "amb(~a, (~a) => {\n~a\n})" iter var body)))
             (loop (string-replace result full replacement) '()))))))

(define (preprocess-js input-file output-file)
  (define input (file->string input-file))
  (define output (transform-amb input))
  (call-with-output-file output-file
    (λ (out) (fprintf out "~a" output))
    #:exists 'replace))

;; Example usage:
;; (preprocess-js "input.js" "output.js")

(define input-file #f)
(define output-file #f)

;; Define CLI flags
#;
(command-line
 #:program "preprocess"
 #:args (in out)
 [(set! input-file in)
  (set! output-file out)])

;; (preprocess-js input-file output-file)
