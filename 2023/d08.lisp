; run this file with `sbcl --script ./d08.lisp`
; ' -> don't evaluate the following ident/whatever
; (import 'ppcre:split-sequence)
; packages need to be downloaded first and the stdlib is basically non-existant
; define a function
; (parameter*) -> ((param-name default) ..)
; &key -> parameters after &key will be keyword parameters -> (split s :delimiterp d)
; #'f -> treat as (function f) which returns function bound to f
(defun split (string &key (delimiterp #'delimiterp))
  ; look up position which __does not__ satisfy predicate `delimiterp` in string
  ; (i think that's what the -if-not means, but it's not documented anywhere)
  (loop :for beg = (position-if-not delimiterp string) ; beg = initial-value-form
    ; then step-form -> result of step-form  will become the new value of `beg`
    ; (w/o then var = initial-value-form is reevaluated)
    ; :start -> keyword param
    ; get pos of next char
    :then (position-if-not delimiterp string :start (1+ end))
    ; 2nd for evaluated after 1st one (nested loop)
    ; `and` macro returns nil if one of the subforms is nil (CL uses prefix notation)
    :for end = (and beg (position-if delimiterp string :start beg))
    ; collect -> accumulation clause of the loop macro; collect builds up a list
    :when beg :collect (subseq string beg end)
    ; termination clause -> stop when no end delim is found
    :while end))

(defun filter-string (string predicate)
  ; opens (and later closes) output stream resulting in a string
  (with-output-to-string (out)
    (loop :for c :across string do
          ; call predicate with `funcall`
          (when (funcall predicate c)
            (write-char c out)))))

; create a record type, alternative would be using `defclass`, which
; is more complicated and more OO (also auto-creates initializers and accessors)
; `(defstruct foo field ...)` generates `make-foo` -> `(make-foo :field 1 ...)`
; auto-creates accessors `foo-field` ...
(defstruct element
  name
  left
  right)

(defun steps-required (starting-element-name
                       instructions
                       name-to-element
                       target-predicate)
  (loop
    with current-element-name = starting-element-name
    with steps-required = 0
    ; get current element
    for el = (gethash current-element-name name-to-element)
    ; increase direction-idx by 1 when stepping
    ; staying in the bounds [0, len instructions)
    for direction-idx = 0 then (mod (1+ direction-idx) (length instructions))
    for direction = (char instructions direction-idx)
    ; continue until we're at the ZZZ element
    while (not (funcall target-predicate el))
    ; can't use regular statements here, they always have to be part of some
    ; loop macro keyword, e.g. `do` here
    do (incf steps-required)
    ; step left 
    if (char= direction #\L) do (setq current-element-name (element-left el))
    ; step right
    else do (setq current-element-name (element-right el))
    ; could also use `count` keyword instead
    finally (return steps-required)
  )
)

; &aux -> auxilliary/helper variables
(defun string-ends-with (string suffix
                         &aux (string-len (length string))
                              (suffix-len (length suffix)))
  (if (< string-len suffix-len)
    nil
    (string= string suffix :start1 (- string-len suffix-len)))
)

; SBCL actually provides gcd/lcm functions, but these also work
(defun mgcd (a b)
  (if (= b 0)
    a
    (mgcd b (mod a b))
  )
)

(defun mlcm (a b)
  (if (> a b)
    ; (a / gcd(a, b)) * b
    (* (/ a (mgcd a b)) b)
    ; (b / gcd(a, b)) * a
    (* (/ b (mgcd a b)) a)
  )
)

; (let (variable*) body-form*) bind variable to name, here variable is
; a name + init (w/o init it would bind nil): e.g. (let ((x 10)) ...)
; but binding is only available in an enclosed scope, there's also `let*` but
; it does not make it available in the top-level
; with-open-file handles closing the files on errors etc.
(let*
  ((input-file "d08_input.txt")
   ; input-file is not available here if using `let` only if using `let*`
   ; since then previous let bindings are available in the same scope
   ; (alternative would be nesting)
   (input (with-open-file (in-file input-file)
    ; read-line takes a 2nd arg that defaults to true, if not nil is returned
    ; if called at the end of the file
    (loop for line = (read-line in-file nil)
        ; ~a consume one arg and output it in human readable form
        ; ~% emit newline
        ; while line do (format t "~a~%" line)))
        while line
        ; collect into a list
        collect (string-trim "\n\r " line))))
   ; car -> get first item of a list; cdr -> get next item (another list/cons cell)
   (instructions (car input))
   ; no way to collect so we init to nil list and then push in the body
   ; alternative would be to use loop with collect and iter over the list
   ; using car/cdr
   (elements nil)
   ; creat hash table, since we use strings as keys we need to use `equal`
   ; instead of the default `eql`
   ; NOTE: use `'` to quote the name, otherwise it looks for a var of that name
   (name-to-element (make-hash-table :test 'equal))
   (starting-nodes nil)
   (ending-nodes nil))

  ; (dolist (var list-form)
  ;  body-form*)
  ; iterate over list-form where var takes the value of the current item
  ; NOTE: parse input into a list of elements
  (dolist (line (cdr input))
    (let ((split-cleaned nil)) 
      ; #\Space -> treat Space as character literal
      ; we push to the front -> words in reverse order -> reverse iteration
      (dolist (word (nreverse (split line :delimiterp #'(lambda (c) (char= c #\Space)))))
          ; only keep A-Z and 0-9
          ; (format t "b4 cleaned ~{~a, ~}~%" split-cleaned)
          (let ((word-cleaned
                  (filter-string word
                                 #'(lambda (c) (or 
                                                 (and
                                                   (char>= c #\0)
                                                   (char<= c #\9))
                                                 (and
                                                   (char>= c #\A)
                                                   (char<= c #\Z)))))))
            (when (string/= "" word-cleaned)
              (push word-cleaned split-cleaned))))
      ; make sure we have items after cleaning
      (when (not (null split-cleaned))
        ; NOTE: use `let`, `defvar`, `defparameter` over setq, since behaviour
        ;       is technically undefined if the var which setq assigns to doesn't
        ;       exist yet
        (let ((element
                (make-element :name (first split-cleaned)
                              :left (second split-cleaned)
                              :right (third split-cleaned))))
          ; can't collect (loop is special and most of the keywords are not CL)
          ; -> have to push (uses cons, so elements are added to the front)
          (push element elements)
          (when (string-ends-with (first split-cleaned) "A")
            (push element starting-nodes))
          (when (string-ends-with (first split-cleaned) "Z")
            (push element ending-nodes))
        ))))
  ; populate hashmap
  (dolist (el elements)
    ; use gethash to access elements in the table and setf to set it
      (setf (gethash (element-name el) name-to-element) el))
  ; print hashmap
  ; (maphash #'(lambda (key value) (format t "k ~a v ~a~%" key value)) name-to-element)
  
  (format t "Part1: ~a~%"
    (steps-required "AAA" instructions name-to-element
                    #'(lambda (el) (string= "ZZZ" (element-name el)))))
  ; get the amount of steps required to end up on a ending node for all the
  ; starting nodes
  ; then find the minimum amount of steps required to end up on all starting
  ; nodes by computing the LCM (least/smallest common multiple) of the steps
  ; required to reach the first ending node from each starting node
  (let* ((steps-per-starting-node
           (loop
             for start in starting-nodes
             collect (steps-required (element-name start) instructions name-to-element
                                     #'(lambda (el)
                                         (string-ends-with (element-name el) "Z")))
           ))
         ; NOTE: if we could assume that __all__ the steps required were prime
         ;       numbers the LCM (least/smallest common multiple) would just
         ;       be the product of those primes
         ; (steps-total
         ;   (loop
         ;     with prod = 1
         ;     for steps in steps-per-starting-node
         ;     do (setf prod (* prod steps))
         ;     finally (return prod))
         ; )
         ; NOTE: since this is not true in this case we actuall have to compute
         ;       the LCM for all the steps required
         (steps-total
           (loop
             with result = 1
             for steps in steps-per-starting-node
             do (setf result (lcm result steps))
             finally (return result))
         )
        )
    ; print all items in a list
    (format t "~{~a, ~}~%" steps-per-starting-node)
    (format t "Part2: ~a~%" steps-total)
  )
)
