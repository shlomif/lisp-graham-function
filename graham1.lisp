(declaim (optimize (debug 3)))

;(defun look-for-squaring-facts (n p)
;  (if (= n 1)
;    ()
;    (if (= (mod n p) 0)
;      ; If it's divisable by p, calculate the division factors
;      (let ((division-factors (look-for-squaring-facts (truncate (/ n p)) p)))
;        (if (and (not (null division-factors))
;                 (= (car division-factors) p))
;          (cdr division-factors)
;          (cons p division-factors)))
;      ; If it's not divisable by p, try the next number.
;      (look-for-squaring-facts n (1+ p)))))

(defun look-for-squaring-facts (n p)
  (flet ((recurse ()
           (let ((division-factors (look-for-squaring-facts (truncate (/ n p)) p)))
             (if (and (not (null division-factors))
                      (= (car division-factors) p))
               (cdr division-factors)
               (cons p division-factors)))))
    (cond ((= n 1) ())
          ; If it's divisable by p, calculate the division factors
          ((= (mod n p) 0) (recurse))
          ; If it's not divisable by p, try the next number.
          (t (look-for-squaring-facts n (1+ p))))))

(defun get-squaring-facts (n)
  (look-for-squaring-facts n 2))

(defclass sqfacts ()
  ((factors
     :initarg :factors)))

(defun get-sqfacts-from-factors (factors)
  (make-instance 'sqfacts :factors factors))

(defun get-sqfacts-from-n (n)
  (get-sqfacts-from-factors (get-squaring-facts n)))

(defgeneric factors (f))

(defmethod factors ((f sqfacts))
  (slot-value f 'factors))

(defgeneric clone (f))

(defmethod clone ((f sqfacts))
  (get-sqfacts-from-factors (copy-list (factors f))))

(defun mult-factors-list (n m)
  (if (or (null n) (null m))
    ; If either list is empty return the remaining elements.
    (concatenate 'list n m)
    ; else if the first elements are equal move on
    (let ((first-n (car n))
          (first-m (car m)))
      (if (= first-n first-m)
        (mult-factors-list (cdr n) (cdr m))
        ; Otherwise - append the smallest element.
        (if (< first-n first-m)
          (cons first-n (mult-factors-list (cdr n) m))
          (cons first-m (mult-factors-list n (cdr m))))))))

(defgeneric mult (f g))
(defmethod mult ((n-ref sqfacts) (m-ref sqfacts))
  (let ((n (factors n-ref))
        (m (factors m-ref)))
    (get-sqfacts-from-factors (mult-factors-list n m))))

(defgeneric is-square (n))

(defmethod is-square ((n sqfacts))
  (null (factors n)))

(defgeneric exists (n f))

(defmethod exists ((n sqfacts) a-factor)
  (find a-factor (factors n)))

(defgeneric last-factor (n))

(defmethod last-factor ((n sqfacts))
  (car (last (factors n))))

(defgeneric product (n))

(defmethod product ((n sqfacts))
  (apply '* (factors n)))

(defgeneric first-factor (n))

(defmethod first-factor ((n sqfacts))
  (car (factors n)))

(defclass dipole (sqfacts)
  ((result
     :initarg :result)
   (compose
     :initarg :compose)))

(defun make-dipole (&key result compose)
  (make-instance 'dipole :result result :compose compose))

(defun make-dipole-from-n (n)
  (make-dipole :result (get-sqfacts-from-n n) 
               :compose (get-sqfacts-from-factors (list n))))

(defgeneric result (d))

(defmethod result ((d dipole))
  (slot-value d 'result))

(defgeneric compose (d))

(defmethod compose ((d dipole))
  (slot-value d 'compose))

(defmethod clone ((d dipole))
  (make-dipole :result (clone (result d))
               :compose (clone (compose d))))

(defmethod factors ((d dipole))
  (factors (result d)))

(defmethod mult ((d1 dipole) (d2 dipole))
  (make-dipole :result (mult (result d1) (result d2))
               :compose (mult (compose d1) (compose d2))))

;; We don't need to over-ride first-factor, exists or is-square because
;; they access the factors using the "factors" method which was already 
;; over-rided for dipole.

(defgeneric get-ret (d))

(defmethod get-ret ((d dipole))
  (copy-list (factors (compose d))))

(eval (list 'defclass 'graham-function ()
            (map 'list #'(lambda (name) (list name :accessor name)) 
                 '(base max-base-id n n-vec next-id n-sq-factors primes-to-ids-map))))

(defun make-graham-function (n)
  (let ((g (make-instance 'graham-function)))
    (setf (n g) n)
    (setf (primes-to-ids-map g) (make-hash-table))
    g))

(defgeneric %get-num-facts (g n))

(defmethod %get-num-facts ((g graham-function) n)
  (get-sqfacts-from-n n))

(defgeneric %get-facts (g factors))

(defmethod %get-facts ((g graham-function) factors)
  (get-sqfacts-from-factors factors))

(defgeneric %get-num-dipole (g n))

(defmethod %get-num-dipole ((g graham-function) n)
  (make-dipole-from-n n))

(defgeneric %calc-n-sq-factors (g))

(defmethod %calc-n-sq-factors ((g graham-function))
  (setf (n-sq-factors g) (%get-num-dipole g (n g))))

(defgeneric %get-next-id (g))

(defmethod %get-next-id ((g graham-function))
  (incf (next-id g)))

(defgeneric %get-prime-id (g p))

(defmethod %get-prime-id ((g graham-function) p)
  (gethash p (primes-to-ids-map g)))

(defgeneric %register-prime (g p))

(defmethod %register-prime ((g graham-function) p)
  (setf (gethash p (primes-to-ids-map g)) (%get-next-id g)))

(defgeneric %prime-exists (g p))

(defmethod %prime-exists (g p)
  (%get-prime-id g p))

(defgeneric %get-min-id (g v))

(defmethod %get-min-id ((self graham-function) vec)
  (reduce #'(lambda (a b) (if (or (not (car a)) (> (car a) (car b))) b a))
          (map 'list #'(lambda (p) (list (%get-prime-id self p) p))
               (factors (result vec)))
          :initial-value (list () 0)))

(defgeneric %try-to-form-n (g))

(defmethod %try-to-form-n ((self graham-function))
  (if (is-square (n-vec self))
    t
    (let ((id (car (%get-min-id self (n-vec self)))))
      (if (null (gethash id (base self)))
        nil
        (progn
          (setf (n-vec self) (mult (n-vec self) (gethash id (base self))))
          (%try-to-form-n self))))))

(defgeneric %get-final-factors (g))

(defgeneric %main-solve (self))

(defmethod %get-final-factors ((self graham-function))
  (%calc-n-sq-factors self)
  (if (is-square (n-sq-factors self))
    (get-ret (n-sq-factors self))
    (%main-solve self)))

(defgeneric solve (g))

(defmethod solve ((self graham-function))
  (list 'factors (%get-final-factors self)))

(defgeneric %main-init (g))

(defmethod %main-init ((self graham-function))
  (setf (next-id self) 0)
  (setf (max-base-id self) -1)
  (setf (base self) (make-hash-table))
  (dolist (p (factors (n-sq-factors self))) (%register-prime self p) )
  (setf (n-vec self) (clone (n-sq-factors self))))

(defgeneric %put-base-vec (g id vec))

(defmethod %put-base-vec ((self graham-function) id vec)
  (setf (gethash id (base self)) vec)
  (if (> id (max-base-id self))
    (setf (max-base-id self) id)))

(defgeneric %update-base (g final-vec))

(defmethod %update-base ((self graham-function) final-vec)
  (let* ((gmi-ret (%get-min-id self final-vec))
         (min-id (car gmi-ret))
         (min-p (cadr gmi-ret)))
    (when min-id
      (progn
        (%put-base-vec self min-id final-vec)
        (dotimes (j (1+ (max-base-id self)))
          (let ((vec (gethash j (base self))))
            (when (and (not (or (= j min-id) (null vec)))
                       (exists vec min-p))
                (setf (gethash j (base self)) (mult vec final-vec)))))))))

(defgeneric %get-final-composition (g i-vec))

(defmethod %get-final-composition ((self graham-function) i-vec)
  (labels ((helper (rest-of-factors final-vec)
                   (if (null rest-of-factors)
                     final-vec
                     (let ((p (car rest-of-factors)))
                       (if (not (%prime-exists self p))
                         (progn (%register-prime self p)
                                (helper (cdr rest-of-factors) final-vec))
                         (let* ((id (%get-prime-id self p))
                                (vec (gethash id (base self))))
                           (helper (cdr rest-of-factors)
                                   (if vec (mult final-vec vec) final-vec))))))))
    (helper (factors i-vec) i-vec)))

(defgeneric %get-i-vec (self i))

(defmethod %get-i-vec ((self graham-function) i)
  (let ((i-vec (%get-num-dipole self i)))
    ; Skip perfect squares - they do not add to the solution
    ; 
    ; Check if $i is a prime number
    ; We need n > 2 because for n == 2 it does include a prime number.
    ;
    ; Prime numbers cannot be included because 2*n is an upper bound
    ; to G(n) and so if there is a prime p > n than its next multiple
    ; will be greater than G(n).
    (if (or (is-square i-vec) (and (> (n self) 2) (= (first-factor i-vec) i)))
      nil
      i-vec)))

(defgeneric %solve-iteration (self i))

(defmethod %solve-iteration ((self graham-function) i)
  (let ((i-vec (%get-i-vec self i)))
    (if (not i-vec)
      nil
      (let ((final-vec (%get-final-composition self i-vec)))
        (%update-base self final-vec)
        (if (%try-to-form-n self)
          (get-ret (n-vec self))
          nil)))))


(defmethod %main-solve ((self graham-function))
  (%main-init self)
  ; (do ((i (1+ (n self)) (1+ i))) ((%solve-iteration self i))))
  (labels ((helper (i) (let ((ret (%solve-iteration self i)))
                         (if ret ret (helper (1+ i)))))) 
    (helper (1+ (n self)))))
