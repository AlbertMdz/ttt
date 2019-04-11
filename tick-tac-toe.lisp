;;; (play #'random-strategy #'random-strategy) should represent this.
;;;
;;; For playing human against computer use, e.g.
;;; (play #'human #'alpha-beta-strategy)
;;; (play #'alpha-beta-strategy #'human)
;;;
;;; Beware: (play ... :size 4) and more needs a lot of time!

(defconstant empty  0 "Empty field")
(defconstant cross  1 "Player X")
(defconstant circle 2 "Player O")

(defun initial-board (n)
  "Return a board with empty fields of size N x N."
  (make-array (list n n) :element-type `(integer ,empty ,circle) :initial-element empty))

;;; Output
(defun print-board (board)
  "Print a board."
  (let ((size (first (array-dimensions board))))
    (format t "~2&~3T")
    (loop for i below size do
	 (format t "~A " (code-char (+ (char-code #\A) i))))
    (loop for i below size do
	 (format t "~&~2D" (1+ i))
	 (loop for j below size do
	      (format t " ~A" (player-name-of (aref board i j)))))))

(defun player-name-of (player)
  "Return character of current player or of empty field."
  (char ".XO" player))

;;; Access to board
(defun place-piece (move player board)
  "Place a piece to a board location."
  (setf (row-major-aref board move) player))

(defun legal-p (move board)
  "A legal move must be into an empty field."
  (eql empty (row-major-aref board move)))

(defun legal-moves (board)
  "Return list of legal moves."
  (loop for move below (array-total-size board)
     when (legal-p move board) collect move))


(defun human (player board)
  "A human player for the game of Tic-Tac-Toe."
  (format t "~&~C to move: " (player-name-of player))
  (symbol-move (read) board))


(defun symbol-move (symbol board)
  "Convert SYMBOL \(got from READ\) to a board move."
  (let ((s (string symbol)))
    (array-row-major-index board
         (- (char-code (char s 1)) (char-code #\1)) 
         (- (char-code (char s 0)) (char-code #\A)))))

(defun alpha-beta (player board alpha beta)
  "Make perfect move with alpha-beta strategy."
  (let ((moves (legal-moves board)))
    (if (or (null moves) (has-won-p player board) (has-won-p (opponent player) board))
  (evaluate player board)
  (let (best-move)
    (loop for move in moves
       with val do
         (place-piece move player board)
         (setf val (- (alpha-beta (opponent player) board (- beta) (- alpha))))
         (place-piece move empty board)
         (when (> val alpha)
     (setf alpha val
                       best-move move))
       until (>= alpha beta))
    (values alpha best-move)))))

(defun alpha-beta-strategy (player board)
  "Wrapper for ALPHA-BETA."
  (multiple-value-bind (value move)
      (alpha-beta player board -1 1)
    (declare (ignore value))
    move))



(defun minimax (player board)
  "Make perfect move with minimax strategy."
  (let ((moves (legal-moves board)))
    (if (or (null moves) (has-won-p player board) (has-won-p (opponent player) board))
  (evaluate player board)
  (let ((best-val most-negative-fixnum)
              best-move)
    (loop for move in moves
       with val do
         (place-piece move player board)
         (setf val (- (minimax (opponent player) board)))
         (place-piece move empty board)
         (when (> val best-val)
     (setf best-val val
                       best-move move)))
    (values best-val best-move)))))

(defun minimax-strategy (player board)
  "Wrapper for MINIMAX."
  (multiple-value-bind (value move)
      (minimax player board)
    (declare (ignore value))
    move))

(defun random-strategy (player board)
  "Make any legal move."
  (declare (ignore player))
  (let ((moves (legal-moves board)))
    (and moves (nth (random (length moves)) moves))))






















;;; Play it
(defun play (strategy-1 strategy-2 &key (size 3) print-p)
  "Play the game of Tic-Tac-Toe.
STRATEGY is a member of the functions:
  HUMAN               Human player
  RANDOM-STRATEGY     Computer plays legal random move
  ALPHA-BETA-STRATEGY Computer plays with Alpha-Beta strategy
  MINIMAX-STRATEGY    Computer plays with Minimax strategy"
  (let ((board (initial-board size)))
    (when print-p (print-board board))
    (loop
       repeat (array-total-size board)           ; needed for draw
       for player = cross then (opponent player) ; X makes always the first move
       for strategy = (if (eql player cross) strategy-1 strategy-2)
       for move = (funcall strategy player board)
       do (place-piece move player board)
	 (when print-p (print-board board))
       until (or (has-won-p cross board) (has-won-p circle board)))
    (evaluate cross board)))

(defun opponent (player)
  "Return the opponent of PLAYER."
  (if (eql player cross) circle cross))

(defun has-won-p (player board)
  "A player has won if either a complete row, a complete column or a complete diagonal is finished."
  (let ((size (first (array-dimensions board))))
    (or
     (loop for r below size thereis (loop for c below size always (eql player (aref board r c))))   ; complete row?
     (loop for c below size thereis (loop for r below size always (eql player (aref board r c))))   ; complete column?
     (loop for r below size for c below size               always (eql player (aref board r c)))    ; complete 1st diagonal?
     (loop for r below size for c from (1- size) downto 0  always (eql player (aref board r c)))))) ; complete 2nd diagonal?

(defun evaluate (player board)
  "Evaluation function for BOARD."
  (cond
    ((has-won-p player            board)  1)
    ((has-won-p (opponent player) board) -1)
    (t                                    0)))

(defun tournament (strategy-1 strategy-2 &optional (matches 1))
  "Comparing strategies between player and opponent."
  (loop repeat matches
     for score = (play strategy-1 strategy-2)
     when (= score 1) sum 1 into player-score
     when (= score -1) sum 1 into opponent-score
     when (= score 0) sum 1 into draws
     finally (return (values player-score opponent-score draws))))