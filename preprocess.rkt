#lang racket

(define amb-helpers
  "///////////////////////////////////////////////////////////////////////////
//
// A simple implementation of the `ambiguous` operator for Javascript
//
//////////////////////////////////////////////////////////////////////////////

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
  if (!pred) {
    fail();
  }
}
/////////////////////////////////////////////////////////////////////////////
//
// End of implementation
//
/////////////////////////////////////////////////////////////////////////////
\n\n")

;; String -> String
;; Given a `.js` file name, transforms all occurences of `amb(...)` to valid
;; `amb` function calls
(define (rewrite-amb input-file)
  ;; String -> Number
  ;; Finds the index of the first unmatched `}` in the string. If there is no
  ;; such index, returns the length of the string.
  (define (find-first-unmatched str)
    (define len (string-length str))

    (define (find-block/rec i stack)
      (cond
        [(>= i len) len]
        [(char=? (string-ref str i) #\{)
         (find-block/rec (add1 i) (cons #\{ stack))]
        [(char=? (string-ref str i) #\})
         (if (empty? stack)
             i ; matching `}` found
             (find-block/rec (add1 i) (cdr stack)))]
        [else
         (find-block/rec (add1 i) stack)]))

    (find-block/rec 0 '()))

  ;; String Number -> String
  ;; Recursively applies the `amb` transformations to the given string
  ;; representing JS code (correctly deals with nested `amb` calls)
  ;; The offset represents the index before which transformations have
  ;; already been applied -- this is needed to prevent applying
  ;; transformations twice on the same code, resulting in weird and most
  ;; likely broken code
  (define (process str offset)
    (define whitespace "\\s*")
    (define var-name-grp "([a-zA-Z_$][a-zA-Z0-9_]*)")
    (define iter-grp "([^\\;]+)")
    (define
      amb-rgx
      (pregexp
       (string-append "amb" whitespace
                      var-name-grp whitespace "="
                      whitespace iter-grp ";?")))
    (define match (regexp-match-positions amb-rgx (substring str offset)))
    (if (not match)
        str
        (let* ([start (+ offset (caar match))] ; start index of `amb(...)`
               [end (+ offset (cdar match))]   ; end index of `amb(...)`
               [match-text (substring str start end)]
               [match-groups (regexp-match amb-rgx match-text)]
               [var (list-ref match-groups 1)]
               [iter (list-ref match-groups 2)]
               ; index of the end of scope of the current `amb` expression
               [amb-end (+ end (find-first-unmatched (substring str end)))]
               [body (substring str end amb-end)] ; code within the current
               [transformed-body (process body 0)]
               ; amb scope
               [amb-call
                (format "return amb(~a, (~a) => { ~a })"
                        iter
                        var
                        transformed-body)]
               [before (substring str 0 start)]
               [after (substring str amb-end)])
          (process (string-append before amb-call after) amb-end))))

  ; Write transformed code to "output.js"
  (begin
    (define output (process (file->string input-file) 0))
    (call-with-output-file "output.js"
      (λ (out) (fprintf out "~a" (string-append amb-helpers output)))
      #:exists 'replace)))