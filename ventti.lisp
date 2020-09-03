;
; Ohjelmointikielet ja -paradigmat 2014
;
; Ventti in ANSI Common Lisp
; Markus Viljanen
;

(defun make-deck ()
	(apply #'append
		(loop for x from 1 to 13 collect
			(loop for y from 1 to 4 collect
				(cons x y)
			)
		)
	)
)

(defun ace-remainder (k c)
	(let ((i (min (floor (/ (- c k ) 9)) k) ))
		(+ (* 10 i) (- k i) )
	)
)

(defun normalize (hand)
	(mapcar (lambda (n) (if (> n 10) 10 n)) hand)
)

(defun nums (hand)
	(mapcar #'car hand)
)

(defun sum (hand)
	(let ((x (reduce #'+ (set-difference (normalize (nums hand)) '(1))) )
		 (k (count 1 (nums hand)) ))
		(+ x (ace-remainder k (+ k (max (- 21 (+ x k)) 0)) ) )
	)
)

; Knuth's algorithm
(defun shuffle (seq)
	(let ((n (length seq)))
		(dotimes (i n seq)
			(rotatef
				(elt seq i)
				(elt seq (+ i (random (- n i))))
			)
		)
	)
)

(defun check-state ()
	(let ((over NIL) (win NIL))
		(if (> (sum *hand*) 21)
			(progn (setf over T) (setf win 'house))
		)
		(if (= (sum *hand*) 21)
			(progn (setf over T) (setf win 'player))
		)
		(if (> (sum *house*) 21)
			(progn (setf over T) (setf win 'player))
		)
		(if (= (sum *house*) 21)
			(progn (setf over T) (setf win 'house))
		)
		(if (and *player-out* *house-out*)
			(progn (setf over T) 
				(if (> (sum *hand*) (sum *house*))
					 (setf win 'player)
					 (setf win 'house)
				)
			)
		)
		(values over win)
	)
)

(defun deal ()
	(push (pop *deck*) *hand*)
	(print-state)
	(if (check-state)
		(print-over)
		(if (not *house-out*) (house-move))
	)
)

(defun out ()
	(setf *player-out* T)
	;(print-state)
	(if (check-state)
		(print-over)
		(if (not *house-out*) (house-move))
	)
)

(defun house-move ()
	(if (< (sum *house*) (sum *hand*))
		(house-deal)
		(if (and (<= (sum *house*) 16) (not *player-out*))
			(house-deal)
			(house-out)
		)
	)
)

(defun house-out ()
	(setf *house-out* T)
	;(print-state)
	(if (check-state)
		(print-over)
		(if *player-out* (house-move))
	)
)

(defun house-deal ()
	(push (pop *deck*) *house*)
	(print-state)
	(if (check-state)
		(print-over)
		(if *player-out* (house-move))
	)
)

(defun print-state ()
	(progn 
		(print '(game state))
		(print (append '(house has) *house*))
		(print (append '(=) (sum *house*)))
		(print (append '(player has) *hand*))
		(print (append '(=) (sum *hand*)))
		(princ #\newline)
	)
)

(defun print-over ()
	(multiple-value-bind (over win) (check-state)
		(print (append (list win) '(wins !)))
	)
)

(defun print-help ()
	(progn 
		(princ "Commands: (new-game) (deal) (out) (quit)")
		(princ #\newline)
	)
)

(defun new-game ()
	(defparameter *deck* (shuffle (make-deck)))
	(defparameter *hand* '())
	(defparameter *house* '())
	(defparameter *player-out* nil)
	(defparameter *house-out* nil)
	(dotimes (i 2) (progn
		(push (pop *deck*) *hand*)
		(push (pop *deck*) *house*)
	))
	(print-state)
)

(defun main-loop ()
	(loop (eval (read)))
)

(progn (print-help) (main-loop))