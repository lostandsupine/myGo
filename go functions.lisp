;;So far I've just begun representing the board and the rules of the game and a little bit of the AI's representation.
;;The board is a 19x19 grid.  A space can either be empty or occupied by a black(value 1) or white(value -1) peice.
;;Pieces of the same color that are adjacent combine to form a single shape.
;;A piece is represented by a binomial stored in its postion in *board*.  The first element is the color, the second element is the number of the shape which it belongs to. For example: the piece on space 1,1 (aref *board* 1 1) -> (-1 5) is a white piece part of shape #5.
;;Individual pieces count as shapes and get their own shape number.
;; Each shape has a unique number, non-repeating, so that if shape #1 gets captured and removed from the board the #1 is never used again for any future shape.
;;*shape-board* and *simple-board* and *pressure-board* and *influence-board* are just for displaying for humans.
;;Current shapes are stored in *shape-list* as: (shape# color (list of binomial spaces)), ex: (21 1 ((1 1) (1 2) (2 2))) shape #21 is black and three pieces occupying (1,1), (1,2) and (2,2).
;;*black-shape-list* and *white-shape-list* are the black and white subsets of *shape-list*
;;*playable-list* and *occupied-list* are the complimentary subsets of the board, made of empty spaces and occupied spaces, respectively.



(defun create-new-board ()
  "Initializes the representation of the board, the shape list and other variables."
           (defparameter *board* (make-array '(19 19 2) :initial-element 0))
           (defparameter *simple-board* (coerce-array-to-list (make-array (list 19 19) :initial-element 0)))
           (defparameter *shape-board* (make-list 19 :initial-element (make-list 19 :initial-element 0)))
           (defparameter *shape-list* (list ))
           (defparameter *black-shape-list* (list ))
           (defparameter *white-shape-list* (list ))
           (defparameter *playable-list* (list ))
           (defparameter *occupied-list* (list ))
           (defparameter *empty-shapes* (list ))
           (defparameter *shape-count* 0)
           (defparameter *last-play-black* nil)
           (defparameter *last-play-white* nil)
           (dotimes (x 19)
             (dotimes (y 19)
               (setf *playable-list* (append *playable-list* (list (list x y))))))
           t);

(defun init-comp ()
"Initializes computer AI variables"
  (defparameter *comp-target* nil)
  (defparameter *pressure-shape-list* (list ))
  (defparameter *pressure-board* (make-array (list 19 19) :initial-element 0))
  (defparameter *influence-board* (coerce-array-to-list (make-array (list 19 19) :initial-element 0))));

(defun display-pressure-board ()
"Displays the pressure-board for human consumption."
  (pressure-board-map)
  (defparameter display *pressure-board*)
  (dotimes (i 19)
    (dotimes (j 19)
      (if (> (aref display i j) 0)
          (setf (aref display i j) 2)
          (if (< (aref display i j) 0)
              (setf (aref display i j) 0)
              (setf (aref display i j) 1)))))
  (setf display (coerce-array-to-list display))
  (display-board display));

(defun update-bw-shape-lists ()
"Refreshes the black and white shape lists"
  (setf *black-shape-list* (list ))
  (setf *white-shape-list* (list ))
  (dotimes (i (length *shape-list*))
    (if (= (elt (elt *shape-list* i) 1) 1)
        (setf *black-shape-list* (append *black-shape-list* (list (elt *shape-list* i))))
        (setf *white-shape-list* (append *white-shape-list* (list (elt *shape-list* i)))))));
  
(defun color-check (row column)
"Checks the color of a point on the board"
           (aref *board* row column 0));

(defun simple-board-map ()
"Maps the peice location from the main board onto the simple board"
           (dotimes (x 19)
             (dotimes (y 19)
               (setf (elt (elt *simple-board* x) y) (+ (aref *board* x y 0) 1)))));

(defun shape-board-map ()
"Maps the shape values from the main board onto the shape board"
           (dotimes (x 19)
             (dotimes (y 19)
               (setf (elt (elt *shape-board* x) y) (aref *board* x y 1)))));

(defun display-board (board)
"Displays the board as a grid of + for blank vertices, X and O for black and white pieces."
  (format t "~%~{~{~[0~;+~;X~] ~}~%~}" board));
  
(defun add-piece (row column color)
"Adds a peice to the board, makes a new shape with the peice.  Updates last play value, playable list and occupied list."
           (setf (aref *board* row column 0) color)
           (make-new-shape row column)
           (setf *playable-list* (remove-if #'(lambda (x) (equal x (list row column))) *playable-list*))
           (setf *occupied-list* (append *occupied-list* (list (list row column color))))
           (if (= color 1)
               (setf *last-play-black* (list row column))
               (setf *last-play-white* (list row column))));

(defun neighbors (row column)
"Checks the 4 adjacencies of a piece on the board"
           (if (not (= (aref *board* row column 0) 0))
               (progn
                 (let ((piece-neighbors (make-array 5)))
                 (setf (aref piece-neighbors 0) (list row column))
                 (if (not (= row 0))
                     (if (= (aref *board* row column 0) (aref *board* (- row 1) column 0))
                         (setf (aref piece-neighbors 1) (list (- row 1) column))))
                 (if (not (= column 18))
                     (if (= (aref *board* row column 0) (aref *board* row (+ column 1) 0))
                         (setf (aref piece-neighbors 2) (list row (+ column 1)))))
                 (if (not (= row 18))
                     (if (= (aref *board* row column 0) (aref *board* (+ row 1) column 0))
                         (setf (aref piece-neighbors 3) (list (+ row 1) column))))
                 (if (not (= column 0))
                     (if (= (aref *board* row column 0) (aref *board* row (- column 1) 0))
                         (setf (aref piece-neighbors 4) (list row (- column 1)))))
                 piece-neighbors))));

(defun is-shape-p (shape-number)
  (dotimes (i (length *shape-list*))
    (if (=  shape-number (elt (elt *shape-list* i) 0))
	(return t)))
  nil)
    

(defun find-shape (search-list n output)
"Finds and outputs the position in the list, or the pieces of shape n in the inputted search-list"
  (dotimes (i (length search-list) )
    (if (= (elt (elt search-list i) 0) n)
        (if output
            (return (elt (elt search-list i) 2))
            (return i)))));

(defun find-shape-from-element (search-list x y)
"Returns the shape number of the shape containing ordered pair (x y) from the searched list."
  (dotimes (i (length search-list))
    (dotimes (j (length (elt (elt search-list i) 2)))
      (if (equal (list x y) (elt (elt (elt search-list i) 2) j))
          (return-from find-shape-from-element (elt (elt search-list i) 0))))));

(defun combine-shapes (search-list shape-list)
"Returns a list containing the ordered pair elements of the shapes in search-list matching the shape numbers in the list shape-list."
  (let ((new-shape (list )))
    (dotimes (i (length shape-list))
      (setf new-shape (append new-shape (find-shape search-list (elt shape-list i) t))))
    (return-from combine-shapes new-shape)));    

(defun remove-shape (n and-pieces)
"Removes shape n from the shape-list and zeros shape value of peices on the board and the actual pieces too if and-pieces = t. Updates playable list and occupied list."
  (let ((shape-to-remove (find-shape *shape-list* n t)))
    (dotimes (i (length shape-to-remove))
      (setf (aref *board* (elt (elt shape-to-remove i) 0) (elt (elt shape-to-remove i) 1) 1) 0)
      (if and-pieces
          (setf (aref *board* (elt (elt shape-to-remove i) 0) (elt (elt shape-to-remove i) 1) 0) 0)))
    (setf *playable-list* (append *playable-list* shape-to-remove))
    (setf *occupied-list* (remove-if #'(lambda (y) (find (list (elt y 0) (elt y 1)) shape-to-remove :test #'equal)) *occupied-list*)))
  (setf *shape-list* (remove-if #'(lambda (x) (= (elt x 0) n)) *shape-list*)));

(defun remove-shape-from-list (search-list n)
"Returns searched list with shape number N removed"
  (remove-if #'(lambda (x) (= (elt x 0) n)) search-list));

(defun make-new-shape (row column)
"Adds a piece on the board to shape list, as part of a bigger shape if that's the case"
  (let ((my-color (color-check row column)) (my-neighbors (neighbors row column)) (my-shape (list (list row column))))
    (if (not (= my-color 0))
        (progn
          (if (= (count NIL my-neighbors :key #'first) 4)
              (progn
                (setf *shape-count* (+ *shape-count* 1))
                (setf (aref *board* row column 1) *shape-count*)
                (setf *shape-list* (append *shape-list* (list (list *shape-count* my-color my-shape)))))
              (progn
                ;(setf *shape-count* (+ *shape-count* 1));;I had this initially change the shape# if the new peice is added to an old shape.  But that seems silly.
                (if (not (= row 0))
                    (if (= (color-check (- row 1) column) my-color)
                        (progn
                          (setf my-shape (concatenate 'list my-shape (find-shape *shape-list* (aref *board* (- row 1) column 1) t)))
                          (remove-shape (aref *board* (- row 1) column 1) nil))))
                (if (not (= column 18))
                    (if (= (color-check row (+ column 1)) my-color)
                        (progn
                          (setf my-shape (concatenate 'list my-shape (find-shape *shape-list* (aref *board* row (+ column 1) 1) t)))
                          (remove-shape (aref *board* row (+ column 1) 1) nil))))
                (if (not (= row 18))
                    (if (= (color-check (+ row 1) column) my-color)
                        (progn
                          (setf my-shape (concatenate 'list my-shape (find-shape *shape-list* (aref *board* (+ row 1) column 1) t)))
                          (remove-shape (aref *board* (+ row 1) column 1) nil))))
                (if (not (= column 0))
                    (if (= (color-check row (- column 1)) my-color)
                        (progn
                          (setf my-shape (concatenate 'list my-shape (find-shape *shape-list* (aref *board* row (- column 1) 1) t)))
                          (remove-shape (aref *board* row (- column 1) 1) nil))))
                (setf *shape-list* (append *shape-list* (list (list *shape-count* my-color my-shape))))
                (dotimes (i (length my-shape))
                  (setf (aref *board* (elt (elt my-shape i) 0) (elt (elt my-shape i) 1) 1) *shape-count*))))))));

(defun adjasencies (n output)
"Returns the number of adjacent spaces or the list of adjasent spaces of a shape n"
  (let ((adjasent-list (list )) (my-shape (find-shape *shape-list* n t)))
    (dotimes (i (length my-shape))
      (let ((row (elt (elt my-shape i) 0)) (column (elt (elt my-shape i) 1)))
        (if (not (= row 0))
            (if (= (color-check (- row 1) column) 0)
                (setf adjasent-list (append adjasent-list (list (list (- row 1) column))))))
        (if (not (= column 18))
            (if (= (color-check row (+ column 1)) 0)
                (setf adjasent-list (append adjasent-list (list (list row (+ column 1)))))))
        (if (not (= row 18))
            (if (= (color-check (+ row 1) column) 0)
                (setf adjasent-list (append adjasent-list (list (list (+ row 1) column))))))
        (if (not (= column 0))
            (if (= (color-check row (- column 1)) 0)
                (setf adjasent-list (append adjasent-list (list (list row (- column 1)))))))))
    (setf adjasent-list (remove-duplicates adjasent-list :test #'equal))
    (if output
        adjasent-list
        (length adjasent-list))));
          
(defun run-rules-for-color (color)
"Checks every shape of one color in the shape-list for adjasencies and removes them if they have none"
  (dotimes (i (length *shape-list*))
    (if (= i (length *shape-list*))
        (return))
    (if (and (= (elt (elt *shape-list* i) 1) color) (= (adjasencies (elt (elt *shape-list* i) 0) nil) 0))
        (remove-shape (elt (elt *shape-list* i) 0) t))));

(defun make-play (row column color)
"Checks to make sure inputted coordinate is empty, then adds the peice and runs the rules for the opposite color then for played color"
  (if (= (aref *board* row column 0) 0)
      (progn
        (add-piece row column color)
        (run-rules-for-color (* color -1))
        (run-rules-for-color color)
        t)
      NIL));

(defun random-move (color)
"Chooses randomly a coordinate pair from the playable-list and makes that play according the inputted color"
  (let ((the-play (elt *playable-list* (random (length *playable-list*)))))
    (make-play (elt the-play 0) (elt the-play 1) color)));

(defun prompt-move (move)
"Prompts User for input, checks if it's an integer between 1 and 19"
 (let ((x))
    (format t "~% ~a" move)
    (setf x (read))
    (loop
     (if (and (integerp x) (and (> x 0) (< x 20)))
         (return x)
         (progn 
           (format t "~% It must be an integer between 1 and 19. ~% ~a" move)
           (setf x (read)))))));

(defun whole-move ()
  (let (x y)
    (loop
     (setf x (- (prompt-move "X:") 1))
     (setf y (- (prompt-move "Y:") 1))
     (if (find (list y x) *playable-list* :test #'equal)
         (return (list y x))
         (format t "~% That is an occupied space, try again.")))));
    
(defun game-loop (n)
  (let (player-move)
    (create-new-board)
    (init-comp)
    (format t "You are black, and you go first.")
    (dotimes (i n)
      (simple-board-map)
      (display-board *simple-board*)
      (setf player-move (whole-move))
      (make-play (elt player-move 0) (elt player-move 1) 1)
      (comp-ai))));

(defun coerce-array-to-list (array-in)
"Outputs a 19x19 list from a 19x19 array"
  (loop for i below 19
        collect (loop for j below 19
                      collect (aref array-in i j))));

(defun acquire-target ()
"If there is already a target shape, checks to make sure it's still on the shape list.  If it's been renamed, reselects the newly named shape(shapes no longer get renumbered).  Otherwise randomly picks a shape to target"
  (let ((new-target t))
    (update-bw-shape-lists)
    (if *comp-target*
	(dotimes (i (length *black-shape-list*))
	  (if (= (elt *comp-target* 0) (elt (elt *black-shape-list* i) 0))
	      (progn
		(setf new-target nil)
		(return)))
        ;(dotimes (j (length (elt (elt *black-shape-list* i) 2)))
         ; (if (equal (elt (elt *comp-target* 2) 0) (elt (elt (elt *black-shape-list* i) 2) j))
          ;    (progn
           ;     (setf new-target nil)
            ;    (setf *comp-target* (elt *black-shape-list* i)))))));;taken out because shapes don't get renumbered.
	  )
	(if new-target
	    (setf *comp-target* (elt *black-shape-list* (random (length *black-shape-list*))))))));

(defun comp-ai ()
"Runs computers AI to determine move, then makes the move.  As of now, selects a random adjasency to the targeted shape" 
  (acquire-target)
  (let ((target-adjasencies (adjasencies (elt *comp-target* 0) t)) (target-move nil))
    (setf target-move (elt target-adjasencies (random (length target-adjasencies))))
    (make-play (elt target-move 0) (elt target-move 1) -1)));

(defun pressure-piece (x y)
"Determines the pressure upon a space, occupied or unoccupied, as color/(10*distance) for every (other) piece on the board."
  (let ((the-pressure 0))
    (dotimes (i (length *occupied-list*))
      (setf the-pressure (+ the-pressure (* (elt (elt *occupied-list* i) 2) 
					    (/ 1 (distance-between x y (elt (elt *occupied-list* i) 0) (elt (elt *occupied-list* i) 1)) 10)))))
    (return-from pressure-piece the-pressure)));
    
(defun distance-between (a b x y)
  (sqrt (+ (expt (- a x) 2) (expt (- b y) 2))));
    
(defun pressure-board-map ()
  (dotimes (x 19)
    (dotimes (y 19)
      (if (not (or (find (list x y 1) *occupied-list* :test #'equal) (find (list x y -1) *occupied-list* :test #'equal)))
          (setf (aref *pressure-board* x y) (round-to-third (pressure-piece x y)))
          (setf (aref *pressure-board* x y) (aref *board* x y 0))))));

(defun space-adjasencies (x y)
"Outputs the spaces adjacent to any space, needed because spaces on the edges and corners don't have 4 adjacencies."
  (let ((adj-list (list )))
    (setf adj-list (list (list (- x 1) y) (list x (+ y 1)) (list (+ x 1 ) y) (list x (- y 1))))
    (setf adj-list (remove-if #'(lambda (z) (or (< (elt z 0) 0)  (< (elt z 1) 0))) adj-list))
    (return-from space-adjasencies adj-list)));

(defun pressure-shape-maker ()
"Groups all spaces of the board into connected shapes depending on pressure. Not incremental, redoes it entirely every time. I think this is necessary because a single added peice changes the pressure of the entire board."
  (setf *pressure-shape-list* (list ))
  (let ((shapes-to-be-added nil) (new-shape (list )) (new-shape-number 0))
    (dotimes (x 19)
      (dotimes (y 19)
        (setf shapes-to-be-added nil)
        (let ((space-adjs (space-adjasencies x y)))
          (dotimes (i (length *pressure-shape-list*))
            (dotimes (j (length space-adjs))
              (if (and (find (elt space-adjs j) (elt (elt *pressure-shape-list* i) 2) :test #'equal)
                       (or (signp (aref *pressure-board* x y) (aref *pressure-board* (elt (elt space-adjs j) 0) (elt (elt space-adjs j) 1)))
                           (and (= (aref *pressure-board* x y) 0) (= (aref *pressure-board* (elt (elt space-adjs j) 0) (elt (elt space-adjs j) 1)) 0))))
                  (setf shapes-to-be-added (remove-duplicates (append shapes-to-be-added (list (elt (elt *pressure-shape-list* i) 0)))))))))
        (if shapes-to-be-added
            (progn
              (setf new-shape (list (list x y)))
              (setf new-shape-number (seq-min shapes-to-be-added))
              (dotimes (k (length shapes-to-be-added))
                (setf new-shape (append new-shape (find-shape *pressure-shape-list* (elt shapes-to-be-added k) t)))
                (setf *pressure-shape-list* (remove-shape-from-list *pressure-shape-list* (elt shapes-to-be-added k)))))
            (progn
              (setf new-shape (list (list x y)))
              (setf new-shape-number (length *pressure-shape-list*))))
        (let (parity)
          (if (< (aref *pressure-board* x y) 0)
              (setf parity -1)
              (setf parity 1))
          (if (= (aref *pressure-board* x y) 0)
              (setf parity 0))
          (setf *pressure-shape-list* (append *pressure-shape-list* (list (list new-shape-number parity new-shape)))))))));
                                                                        
(defun seq-min (seq)
"Gets the minimum from a sequence."
  (let (mini)
        (if (> (length seq) 1)
	    (progn
	      (setf mini (elt seq 0))
	      (dotimes (i (- (length seq) 1))
		(setf mini (min mini (elt seq (+ i 1))))))
	    (setf mini (elt seq 0)))
        mini));

(defun round-to-third (number)
"Rounds a number to the third decimal place."
  (return-from round-to-third (/ (round (* 1000 number)) 1000)));              
        
(defun signp (a b)
"Checks if two numbers a and b have the same sign."
  (if (> (* a b) 0)
      t
      nil));

(defun surrounded-check (x y color)
"Checks if a space on the board is surrounded."
  (let ((checklist (space-adjasencies x y)))
    (dotimes (i (length checklist))
      (if (= (aref *board* (elt (elt checklist i) 0) (elt (elt checklist i) 1) 0) color)
          (return-from surrounded-check T))))
  NIL);

(defun eyes-p (shape-number)
  (if (not (is-shape-p shape-number))
      (return nil))
  
  

    
(defun emptyspace ()
  (defparameter *empty-shapes* (list ))
  (let ((emptys *playable-list*))
    (loop
     (if (= (length emptys) 0)
         (return)
         (progn
           (let ((current-shape (list )))
             (push (pop emptys) current-shape)
             (let ((check-list (
                         
       
        
        
  
              
  


   
  

                      
                                        
                   

                         
                