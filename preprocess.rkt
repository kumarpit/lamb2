#lang racket

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

(define (rewrite-amb input-file)
  (define
    amb-regex
    #px"amb\\s*\\(\\s*([a-zA-Z_$][a-zA-Z0-9_$]*)\\s*,\\s*([^\\)]+)\\);?")

  ;; Find the matching closing brace for a block starting at `{` position
  (define (find-first-unmatched str)
  (define len (string-length str))

  (define (find-block/rec i stack)
    (cond
      [(>= i len) len]
      [(char=? (string-ref str i) #\{)
       (find-block/rec (add1 i) (cons #\{ stack))]
      [(char=? (string-ref str i) #\})
       (if (empty? stack)
           (add1 i) ; matching } found
           (find-block/rec (add1 i) (cdr stack)))]
      [else
       (find-block/rec (add1 i) stack)]))

  (find-block/rec 0 '())) ; start with empty stack

  (define (process str)
    (define match (regexp-match-positions amb-regex str))
    (if (not match) ;; there is no occurrence of amb(...)
        str
        (let* ([start (caar match)] ;; start of the match
               [end (cdar match)]   ;; end of the match
               [match-text (substring str start end)]
               [match-data (regexp-match amb-regex match-text)]
               [var (list-ref match-data 1)]
               [iter (list-ref match-data 2)]
               [body-end (find-first-unmatched (substring str end))]
               [body (substring str end (+ end body-end))]
               [transformed-body (process body)]
               [new-code
                (format "amb(~a, (~a) => { ~a })"
                        iter
                        var
                        transformed-body)]
               [before (substring str 0 start)]
               [after (substring str (+ end body-end))])
          (process (string-append before new-code after)))))

  (begin
    (define output (process (file->string input-file)))
    (call-with-output-file "output.js"
    (λ (out) (fprintf out "~a" (string-append amb-helpers output)))
    #:exists 'replace)))
