; run this file with `sbcl --script ./d09.lisp`
(defun split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string) ; beg = initial-value-form
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun all-zeroes-p (history)
  (loop
    for item in history
    when (/= item 0) do (return nil)
    finally (return t)
  )
)

; build a difference from history, the new lenght will be (length history) - 1
(defun difference (history)
  (loop
    for x in history
    ; not nested!
    for y in (cdr history)
    collect (- y x)
  )
)

(defun extrapolate (history)
  (if (all-zeroes-p history)
    ; zero diff -> extrapolation is just 0
    0
    ; recurse
    ; last returns a list, so we have to get the item
    (+ (car (last history)) (extrapolate (difference history)))
  )
)

(defun extrapolate-backwards (history)
  (if (all-zeroes-p history)
    ; zero diff -> extrapolation is just 0
    0
    ; recurse
    ; since extrapolating backwards -> use first value and subtract from it
    (- (first history) (extrapolate-backwards (difference history)))
  )
)

(let*
  ((input-file "d09_input.txt")
   (input (with-open-file (in-file input-file)
      (loop for line = (read-line in-file nil)
          while line
          collect (string-trim "\n\r " line)
      ))
   )
   (histories
     ; `dolist` is imperative, so it needs helper etc to return sth.
     ; prefer `loop` here
     (loop
       for line in input
       collect (mapcar
                 #'parse-integer
                 (split line :delimiterp #'(lambda (c) (char= c #\Space))))
     )
   )
   (extrapolations
     (mapcar #'extrapolate histories))
   (part1
     (loop
       for x in extrapolations
       sum x))
   (part2
     (loop
       for x in (mapcar #'extrapolate-backwards histories)
       sum x
     ))
  )
  ; (format t "~{~a, ~}~%" histories)
  ; (format t "~{~a, ~}~%" extrapolations)
  (format t "Part1: ~a~%" part1)
  ; (format t "~{~a, ~}~%" extrapolations-pt2)
  (format t "Part2: ~a~%" part2)
)
