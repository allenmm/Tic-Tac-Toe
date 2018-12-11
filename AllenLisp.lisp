;; AllenLisp.lisp 
;;
;; AllenLisp is a program that contains functions for a tic-tac-toe 
;; game. There are functions for a sample board with a winning game and 
;; the empty board that the players start out with. Along with functions
;; that check if three elements in a list are equal to each other 
;; for comparison and a function that determines if the row or column of
;; the board passed in currently has a game-winning move or not. 
;; Functions that place a move on the board, switch the mark that's 
;; currently being used, and functions that let the user play a game 
;; against themselves. And functions that display the current value of the
;; board row or column. There is also a function that allows a computer
;; player to place a move in the middle of the board if there is nothing ;; there. In the future there will be functions that allow
;; the player to play a game against the computer based on user-input.
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
(defparameter *mark* 'x)


;; Takes in a board and prints the board that is a list 
;; of nine symbols in row-major order. Defines a variable i
;; that starts out with an inital value of 0, then increments 
;; the variable by one each time it iterates over the function.
;; Every time we iterate over the function the current ith board value 
;; is formatted and printed until i is nine. Then we stop evaluating the
;; statement.
;; Param: board - a list containing elements of a tic-tac-toe board in 
;; row-major order.
(defun print-board (board)
  ;;Prints a bar that goes along the top of the tic-tac-toe board.
  (format t "=============")
  ;;Iterates over the function, increments one each time.
  (do ( (i 0 (+ i 1)))
  	  ;;Tests if i is nine. If it is nine, done is evaluated.
      ( (= i 9) 'done)
      ;; All these expressions are evaled in order every iteration
      ;; and printed out. If the current ith board value is i modulo 3 
      ;; and the remainder is 0, it's printed.
      (if ( = (mod i 3) 0)  (format t "~%|") nil)
      ;; Standard outprint. ~A gets replaced by the 
      ;; current ith board value is printed.
      (format t " ~A |" (nth i board))
  )
  ;; Bottom bar that goes along the bottom of the tic-tac-toe board.
  ;; is printed when the do statement is exited.
  (format t "~%=============")
)


;; Takes in three elements and determines if the three elements in a list
;; are all equal to each other. The three in a row checker. If the      
;; elements are all equal to each other it will return true, otherwise it
;; will return false.
;; Param: a b c - A list containing three elements of a board to check 
;; against each other.
(defun threequal (a b c)
	;; Evaluates if both a and b are equal to b and c to check
	;; if the elements are all equal to each other.
	(and (equal a b) (equal b c)
	)
)


;; Takes in a list and determines if a list representing a row or column
;; or diagonal of a tic-tac-toe board is a victory or not. It will 
;; return true if it is a victory and the first, second, and third      
;; elements in the list are equal to each other and return false for three
;; '- elements in the list or if the three elements in the list are not 
;; equal to each other.
;; Param: alist - a list containing a row, column, or diagonal of a 
;; tic-tac-toe board.
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
  			(and(equal (first alist) '-)
  				(equal (second alist) '-)
  		    	(equal (third alist) '-)
  			)  
  		)
    )
)


;; Checks to see if the player won by checking to see if any of the rows,
;; columns, or diagonals on the board have three equal elements in a row.
;; Because the current mark will always be the opposite of the current 
;; player, the current mark is changed to its opposite value before
;; comparing all the rows, columns, and diagonals. If a player has won, 
;; a function is called. Otherwise, if the player hasn't won, the current ;; mark is changed back to the opposite player.
(defun player-win ()
	;; Changes mark to the opposite mark to reflect the correct
	;; player winner.
	(other-player)
	;; Checks to see if there is a victory in a row, column, or diagonal.
	(if (or
			(victory (grab-row *emptyboard* 0))
			(victory (grab-row *emptyboard* 1))
			(victory (grab-row *emptyboard* 2))
			(victory (grab-col *emptyboard* 0))
			(victory (grab-col *emptyboard* 1))
			(victory (grab-col *emptyboard* 2))
			(victory (grab-diagnol-left *emptyboard*))
			(victory (grab-diagnol-right *emptyboard*))
		)
		;; If there is a victory calls function to display win.
		(player-win-quit)
		;; If the player has not won changes the mark back to normal.
		(other-player)
	)
)

;; If a player has won, the mark of the player that won is printed out.
;; And says that the game is over. Then the program exits.
(defun player-win-quit ()
(format t "~%~%Player ~A wins!~%" *mark*)
(format t "Game over, hit enter. Exit and reload to play again.")
(read)
;; Exits program.
(quit)
)

;; If no moves have been placed on the middle of the board, the board 
;; is marked with the computer-player's mark.
(defun computer-player-middle ()
	;; Checks if the middle spot on board is an empty spot.
	(if (equal (nth 4 *emptyboard*) '-)
	(setf (nth 4 *emptyboard*) *mark*)	
	nil
	)
)


;; Takes in an index and checks to see if that index is greater than 
;; or equal to 0 and less than or equal to 8. If it is, a move is 
;; placed on the board and the mark is switched to its opposite value. 
;; If it's not, the player is prompted to enter an index from 0 to 8.
;; Then the board is checked to see if the player won and the mark for 
;; the next player is printed out.
;; Param: index - a list that represents an index spot on the ttt board.
(defun play-self (index)
	(if (and (<= index 8) (>= index 0) )
		;; If index is from 0 to 8 then mark the board and 
		;; switch the board mark to its opposite value.
		(if (mark-board index) nil (other-player))
		(format t "Enter a move from 0 to 8")		
	)
	;;Checks to see if the player won or not.
	(player-win)
	
	;; Prints out the current mark for the next player.
	(format t "~%~%Next player is ~A ~%" *mark*)

)

;; Takes in an index and checks to see if that index is 
;; equal to an empty spot on the board and print out to show what moves 
;; can be made. If the spot on the board is empty, the move is allowed. 
;; If the spot on the board already has a position marked, then it will 
;; say that spot is already marked.
;; Param: index - a list that represents an index spot on the ttt board. 
(defun mark-board (index)
	;; Checks if spot on board is an empty spot.
	(if (equal (nth index *emptyboard*) '-)
		;; If the spot on the board is empty it marks it with 
		;; whichever mark is currently available.
		(setf (nth index *emptyboard*) *mark*)
		(format t "~%This spot ~A is already marked, try 
		again.~%~%" index)
	)
	;; Print out the current board.
	(print-board *emptyboard*)
)


;; If it is the other player's turn, it checks if the mark is equal
;; to x. If it's equal to x, then x is changed to the value o. 
;; If the value of the mark is not x, then it is set to x to be used 
;; by the other player.
;;
(defun other-player ()
	;; Checks what mark is equal to.
	(if (equal *mark* 'x)
	;; Sets as o if x.
	(setf *mark* 'o)
	;; Sets as x if o.
	(setf *mark* 'x)
	)
	;; Stops nil from being printed, for formatting.
	(format t "")
)


;; Takes in a board and a row value and returns a list consisting of the
;; nth row (zero-based) of a tic-tac-toe board. The nth row
;; can be either 0, 1, or 2. Returns the current row by multiplying the 
;; row number entered by three to get the first row value and 
;; adding a one and two to the row number entered to get the next two 
;; values to the right of it.
;; Param: board - a list containing elements of a tic-tac-toe board in 
;; row-major order.
;; Param: row - a list representing the desired row to be grabbed.
(defun grab-row (board row)
  ;; Inside let is a list of variable definitions for local variables.
  ;; The local variable x is now three times the row number.
  (let ((x (* 3 row)))
  	;; Uses the local variable x.
  	;; The first nth value is three times the column number entered.
    (list (nth x board)
    	  ;; The second nth value is the value of x plus one.
          (nth (+ x 1) board)
          ;; The third nth value is the  value of x plus two.
          (nth (+ x 2) board)
    )
  )
)


;; Takes in a board and a column value and returns a list consisting of 
;; the nth column (zero-based) of a tic-tac-toe board. The nth column
;; can be either 0, 1, or 2. Returns the current column by adding three 
;; to the enter column index each time. 
;; Param: board - a list containing elements of a tic-tac-toe board in 
;; row-major order.
;; Param: col - a list representing the desired column to be grabbed. 
(defun grab-col (board col)
  ;; Locates the nth elements of the list.
  (list (nth col board)
        (nth (+ col 3) board)
        (nth (+ col 6) board)
  )
)

;; Takes in a board and returns a list consisting of the nth numbers 
;; 0, 4, and 8 of a tic-tac-toe board going from left to right.
;; Param: board - a list containing elements of a tic-tac-toe board in 
;; row-major order.
(defun grab-diagnol-left (board)
	;; Locates the nth elements of the board list.
	(list 
		(nth 0 board)
		(nth 4 board)
		(nth 8 board)
	)	
)

;; Takes in a board and returns a list consisting of the nth numbers 
;; 2, 4, and 6 of a tic-tac-toe board going from right to left.
;; Param: board - a list containing elements of a tic-tac-toe board in 
;; row-major order.
(defun grab-diagnol-right (board)
	;; Locates the nth elements of the board list.
	(list 
		(nth 2 board)
		(nth 4 board)
		(nth 6 board)
	)
)