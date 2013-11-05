(defun landscape (n f1 f2) 
  (defparameter *nodes* nil)
  (defparameter *edges* nil)
  (defparameter *objects* nil)
  (defparameter *object-locations* nil)
  (defparameter *location* nil)
  (let* ((count 1)
	 (num-edges (round (within f1 f2))))
    (dotimes (i n)
      (make-land count num-edges)
      (if (eql count 3)
	  (setf count 1)
	  (setf count (+ count 1)))
      (setf num-edges (- num-edges 1)))
    (setf *gensym-counter* 0
	  *location* (caar *nodes*))
    (car '(Success))))

;(pickup (car (objects-at *location* *objects* *object-locations*)))
    
(defun rwalk ()
 ; (mapcar #'(lambda (caar node)
  (when (look)
    (mapcar #'pickup *objects*)
    (walk 'east)
    (look)
    (mapcar #'pickup *objects*)
    (walk 'west)
    (look)
    (mapcar #'pickup *objects*)
    (walk 'downstairs)
    (look)
    (mapcar #'pickup *objects*)
    (walk 'upstairs)
    (look)
    (mapcar #'pickup *objects*)
    (rwalk)))  
  
(defun put-object (object)
  (push (list object (caar (list (any *nodes*)))) *object-locations*))

(defun make-objects (num)
  (let* ((a (gensym "sword"))
	 (b (gensym "staff"))
	 (c (gensym "bucket")))
    (case num
      ((1) (push a *objects*)
       (put-object a))
      ((2) (push b *objects*)
       (put-object b))
      ((3) (push c *objects*)
       (put-object c))))) 

(defun make-land (count edges)
  (when (eql count 1)
    (defparameter *p1* (gensym "living-room"))
    (defparameter *p2* (gensym "garden"))
    (defparameter *p3* (gensym "attic")))
  (case count 
    ((1) (land count *p3*))
    ((2) (land count *p2*))
    ((3) (land count *p1*)))
  (make-edges count *p1* *p2* *p3* edges))

(defun land (num name)
  (defparameter *place* name)
  (case num
    ((1) (push `(,name (,(format nil "You are in the ~a. There is a giant welding torch in the corner." name))) *nodes*))
    ((2) (push `(,name (,(format nil "You are in a beautiful ~a. There is a well in front of you." name))) *nodes*))   
    ((3) (push `(,name (,(format nil "You are in a ~a. A wizard is snoring loudly on the couch." name))) *nodes*))))

;(let ((my-name (caadr *nodes*))
  (defun make-edges (num name name2 name3 edges)
    (if (> edges 0)
	(case num
	  ((1) (push `(,name3 (,name downstairs ladder)) *edges*));gahhh
	  ((2) (push `(,name2 (,name east door)) *edges*))
	  ((3) (push `(,name (,name2 west door)(,name3 upstairs ladder)) *edges*))))
    (make-objects num))

(defun any (alist)
  (nth (randi (length alist)) alist))    
	
(defun within (x y)
  (+ x (* (- y x) (randf 1.0))))(defun landscape (n f1 f2)
  (reset-seed)
  (setf *nodes* nil
	*edges* nil
	*objects* nil
	*object-locations* nil
	*location* nil
	*gensym-counter* 0)
  (let* ((count 1)
	 (num-edges (round (within f1 f2))))
    (dotimes (i n)
      (make-land count num-edges)
      (if (eql count 3)
	  (setf count 1)
	  (setf count (+ count 1))))
    (setf num-edges (- num-edges 1)))
  (setf *location* (caar *nodes*))
  (car '(Success)))

;(pickup (car (objects-at *location* *objects* *object-locations*)))
    
(defun rwalk ()
  (look)
  (mapcar #'pickup *objects*)
  (mapcar #'(lambda (loc)
	      (walk loc)
	      (look)
	      (mapcar #'pickup *objects*))
	  '(west east upstairs downstairs)))

(defun put-object (object)
  (push (list object (caar (list (any *nodes*)))) *object-locations*))

(defun make-objects (num)
  (let* ((a (gensym "sword"))
	 (b (gensym "staff"))
	 (c (gensym "bucket")))
    (case num
      ((1) (push a *objects*)
       (put-object a))
      ((2) (push b *objects*)
       (put-object b))
      ((3) (push c *objects*)
       (put-object c))))) 

(defun make-land (count edges)
  (when (eql count 1)
    (defparameter *p1* (gensym "living-room"))
    (defparameter *p2* (gensym "garden"))
    (defparameter *p3* (gensym "attic")))
  (case count 
    ((1) (land count *p3*))
    ((2) (land count *p2*))
    ((3) (land count *p1*)))
  (make-edges count *p1* *p2* *p3* edges))

(defun land (num name)
  (defparameter *place* name)
  (case num
    ((1) (push `(,name (,(format nil "You are in the ~a. There is a giant welding torch in the corner." name))) *nodes*))
    ((2) (push `(,name (,(format nil "You are in a beautiful ~a. There is a well in front of you." name))) *nodes*))   
    ((3) (push `(,name (,(format nil "You are in a ~a. A wizard is snoring loudly on the couch." name))) *nodes*))))

;(let ((my-name (caadr *nodes*))
  (defun make-edges (num name name2 name3 edges)
    (if (> edges 0)
	(case num
	  ((3) (push `(,name3 (,name2 downstairs ladder)) *edges*))
	  ((2) (push `(,name2 (,name east door)) *edges*))
	  ((1) (push `(,name (,name2 west door)(,name3 upstairs ladder)) *edges*))))
    (make-objects num))

(defun any (alist)
  (nth (randi (length alist)) alist))    
	
(defun within (x y)
  (+ x (* (- y x) (randf 1.0))))

(deftest !do-rwalk-worker (&aux (a 11) (b 2) (c 6))
  (test '(items-) (!rwalk-worker a b c)))

(defun !rwalk-worker (&optional (rooms 20) (f1 1) (f2 5) (steps 100))
  (reset-seed)
  (landscape 20 1 5)
  (format t "nodes: ~a~%~%objects: ~a~%~%"
	  *nodes* *edges* *objects* *object-locations*)
  (dotimes (i steps)
    (rwalk))
  (inventory))