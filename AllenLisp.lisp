;; AllenLisp.lisp 
;;
;; AllenLisp is a program that contains functions for a tic-tac-toe 
;; game. There are functions for a sample board with a winning game and 
;; an empty board, 
;;
;; Marissa Allen
;;

;; Example board - represented as a list of nine markers on the 
;; tic-tac-toe board
(defparameter *sampleboard* (list 'x 'x 'x '- 'o '- '- '- '-))


;; Actual board - the actual empty board that it will start out as.
(defparameter *emptyboard* (list '- '- '- '- '- '- '- '- '-))


;; Takes in a board and prints the board that is a list 
;; of nine symbols in row-major order.
(defun print-board (board)
  (format t "=============")
  (do ( (i 0 (+ i 1)))
      ( (= i 9) 'done)
      ;; all these expressions are evaled in order every iteration
      (if ( = (mod i 3) 0)  (format t "~%|") nil)
      (format t " ~A |" (nth i board))
  )
  (format t "~%=============")
)


;; Takes in three elements and determines if the three elements in a list ;; are all equal to each other. The three in a row checker. If the       ;; elements are all equal to each other it will return true, otherwise it ;; will return false.
(defun threequal (a b c)
	(and (equal a b) (equal b c)
	)
 )


;; Takes in a list and determines if a list representing a row or column ;; or diagonal of a tic-tac-toe board is a victory or not. It will 
;; return true if it is a victory and the first, second, and third       ;; elements in the list are equal to each other and return false for three ;; '- elements in the list or if the three elements in the list are not  ;; equal to each other.
(defun victory (alist) 

	(and
		;; Checks if first three elements are equal.
		(and (equal (first alist) (second alist))
		(equal (second alist) (third alist))
		)
		;; Checks if the first the elements are equal to an 
		;; empty space '-. If they are, then the value is negated 
		;; since three empty spaces are not a game-winning victory.
  		(not 
  			(and
  				(and(equal (first alist) '-)
  					(equal (second alist) '-)
  			    )
  			    	(equal (third alist) '-)
  			)  
  		)
    )
)


;; Takes in a board and a row value and returns a list consisting of the ;; nth row (zero-based) of a tic-tac-toe board.
(defun grab-row (board row)
  (let ((x (* 3 row)))
    (list (nth x board)
          (nth (+ x 1) board)
          (nth (+ x 2) board)
    )
  )
)


;; Takes in a board and a column value and returns a list consisting of  ;; the nth column (zero-based) of a tic-tac-toe board.
(defun grab-col (board col)
  (list (nth col board)
        (nth (+ col 3) board)
        (nth (+ col 6) board)
  )
)
