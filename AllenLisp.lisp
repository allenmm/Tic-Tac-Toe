;; AllenLisp.lisp 
;;
;; AllenLisp is a program that contains functions for a tic-tac-toe 
;; game. There are functions for a sample board with a winning game and 
;; the empty board that the players start out with. Along with functions ;; that check if three elements in a list are equal to each other 
;; for comparison and a function that determines if the row or column of ;; the board passed in currently has a game-winning move or not. And     ;; functions that display the current value of the board row or column.
;; In the future there will be functions that allow the user to 
;; place a move on the board, and play a game against the computer based ;; on user-input.
;;
;; Marissa Allen
;;

;; Example board - represented as a list of nine markers on the 
;; tic-tac-toe board. A global variable that can be called anywhere.
(defparameter *sampleboard* (list 'x 'x 'x '- 'o '- '- '- '-))


;; Actual board - the actual empty board that it will start out as.
;; A global variable that can be called anywhere.
(defparameter *emptyboard* (list '- '- '- '- '- '- '- '- '-))

;; The default board marker that the first player will start out with.
;; A global variable that can be called anywhere.
(defparameter *mark* 'X)

;; Takes in a board and prints the board that is a list 
;; of nine symbols in row-major order. Defines a variable i
;; that starts out with an inital value of 0, then increments 
;; the variable by 1 each time it iterates over the function.
;; When i is nine, then we stop evaluating the statement.
;;
;; Then the current ith board value is printed as a space, the ith value
;;
;; Grabs each of the elements in turn and prints them off,
;; before every third element a newline character is printed.
;; 
(defun print-board (board)
  ;;Prints a bar that goes along the top of the tic-tac-toe board.
  (format t "=============")
  ;;Iterates over the function, increments one each time.
  (do ( (i 0 (+ i 1)))
  	  ;;Tests if i is nine. If it is nine, done is evaluated.
      ( (= i 9) 'done)
      ;; All these expressions are evaled in order every iteration
      ;; and printed
      (if ( = (mod i 3) 0)  (format t "~%|") nil)
      ;; Standard outprint. ~A gets replaced by the 
      ;; current ith board value is printed.
      (format t " ~A |" (nth i board))
  )
  ;; Bottom bar that goes along the bottom of the tic-tac-toe board.
  ;; is printed when the do statement is exited.
  (format t "~%=============")
)


;; Takes in three elements and determines if the three elements in a list ;; are all equal to each other. The three in a row checker. If the       ;; elements are all equal to each other it will return true, otherwise it ;; will return false.
(defun threequal (a b c)
	;; Evaluates if both a and b are equal to b and c to check
	;; if the elements are all equal to each other.
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



;; Takes in an index and checks to see if that index is 
;; equal to an empty spot on the board and print out to show what moves  ;; can be made. If the spot on the board is empty, the move is allowed. 
;; If the spot on the board already has a position marked, then it will 
;; say that spot is already marked.
(defun mark-board (index)
	(if (equal (nth index *emptyboard*) '-)
		(setf (nth index *emptyboard*) *mark*)
		(format t "~%This spot ~A is already marked, try 
		again.~%~%" index)
	)
	;; Print out the current board.
	(print-board *emptyboard*)
)


;; If it is the other player's turn, it checks if the mark is equal
;; to X. If it's equal to X, then X is changed to the value O. 
;; If the value of the mark is not X, then it is set to X to be used 
;; by the other player.
(defun other-player ()
	;; Checks what mark is equal to.
	(if (equal *mark* 'X)
	;; Sets as O if X.
	(setf *mark* 'O)
	;; Sets as X if O.
	(setf *mark* 'X)
	)
	;; Prints out the current mark for the next player.
	(format t "~%~%Next player is ~A ~%" *mark*)
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
