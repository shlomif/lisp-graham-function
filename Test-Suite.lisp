;;; Load the testing framework
(load "cybertigger-test.lisp")
(import 'cybertiggyr-test:deftest)

;;; Load the library to be tested. 
(load "graham1.lisp")

;;; Define some tests
(deftest test-squaring-facts-1 ()
  (equal (get-squaring-facts 1) (list)))

(deftest test-squaring-facts-3 ()
  (equal (get-squaring-facts 3) (list 3)))

(deftest test-squaring-facts-15 ()
  (equal (get-squaring-facts 15) (list 3 5)))

(deftest test-squaring-facts-9 ()
  (equal (get-squaring-facts 9) '()))
  
(deftest test-squaring-facts-12 ()
  (equal (get-squaring-facts 12) '(3)))

(deftest test-squaring-facts-24 ()
  (equal (get-squaring-facts 24) '(2 3)))

(deftest test-sqfacts-clone-24 ()
  (let* ((f1 (get-sqfacts-from-n 24))
         (f2 (clone f1)))
    (equal (factors f2) '(2 3))))

(deftest test-mult-1 ()
  (let ((n (get-sqfacts-from-factors '(2 3)))
        (m (get-sqfacts-from-factors '(2 5))))
    (equal (factors (mult n m)) '(3 5))))

(deftest test-mult-2 ()
  (let ((n (get-sqfacts-from-factors '(2 3 5)))
        (m (get-sqfacts-from-factors '(2 5))))
    (equal (factors (mult n m)) '(3))))

(deftest test-mult-3 ()
  (let ((n (get-sqfacts-from-factors '(2 3 5 7)))
        (m (get-sqfacts-from-factors '(2 5))))
    (equal (factors (mult n m)) '(3 7))))

(deftest test-mult-4 ()
  (let ((n (get-sqfacts-from-factors '()))
        (m (get-sqfacts-from-factors '(2 5))))
    (equal (factors (mult n m)) '(2 5))))

(deftest test-mult-5 ()
  (let ((n (get-sqfacts-from-factors '()))
        (m (get-sqfacts-from-factors '())))
    (equal (factors (mult n m)) '())))

(deftest test-mult-6 ()
  (let ((n (get-sqfacts-from-factors '(2 3 7)))
        (m (get-sqfacts-from-factors '(2 3 7))))
    (equal (factors (mult n m)) '())))

(deftest test-is-square-1 ()
  (is-square (get-sqfacts-from-n 25)))

(deftest test-is-square-2 ()
  (is-square (get-sqfacts-from-n 9)))

(deftest test-is-square-3 ()
  (is-square (get-sqfacts-from-n 4)))

(deftest test-is-square-4 ()
  (not (is-square (get-sqfacts-from-n 10))))

(deftest test-is-square-5 ()
  (not (is-square (get-sqfacts-from-n 5))))

(deftest test-is-square-6 ()
  (not (is-square (get-sqfacts-from-n 6))))

(deftest test-exists-1 ()
  (exists (get-sqfacts-from-n 6) 2))

(deftest test-exists-2 ()
  (exists (get-sqfacts-from-n 6) 3))

(deftest test-exists-3 ()
  (not (exists (get-sqfacts-from-n 6) 5)))

(deftest test-exists-4 ()
  (exists (get-sqfacts-from-n 10) 2))

(deftest test-exists-5 ()
  (not (exists (get-sqfacts-from-n 15) 2)))

;; Test the last-factor method.

(deftest test-last-factor-1 ()
  (eq (last-factor (get-sqfacts-from-n 6)) 3))

(deftest test-last-factor-2 ()
  (eq (last-factor (get-sqfacts-from-n 10)) 5))

(deftest test-last-factor-3 ()
  (eq (last-factor (get-sqfacts-from-n 15)) 5))

(deftest test-last-factor-4 ()
  (eq (last-factor (get-sqfacts-from-n 2)) 2))

(deftest test-last-factor-5 ()
  (eq (last-factor (get-sqfacts-from-n 90)) 5))

(deftest test-product-1 ()
  (eq (product (get-sqfacts-from-n 6)) 6))

(deftest test-product-2 ()
  (eq (product (get-sqfacts-from-n 18)) 2))

(deftest test-product-3 ()
  (eq (product (get-sqfacts-from-n (* 3 3 2 2 2 5))) 10))

(deftest test-first-factor-1 ()
  (eq (first-factor (get-sqfacts-from-n 6)) 2))

(deftest test-first-factor-2 ()
  (eq (first-factor (get-sqfacts-from-n 10)) 2))

(deftest test-first-factor-3 ()
  (eq (first-factor (get-sqfacts-from-n 15)) 3))

(deftest test-first-factor-4 ()
  (eq (first-factor (get-sqfacts-from-n 2)) 2))

(deftest test-first-factor-6 ()
  (eq (first-factor (get-sqfacts-from-n (* 3 3 3 5))) 3))

(deftest test-first-factor-7 ()
  (eq (first-factor (get-sqfacts-from-n (* 3 3 5 7 7 7))) 5))

(deftest test-dipole-clone-1 ()
     (let* ((d (make-dipole :result (get-sqfacts-from-n 10) :compose (get-sqfacts-from-factors '(10))))
            (d* (clone d)))
       (equal (factors d*) '(2 5))))

(deftest test-make-dipole-from-n-1 ()
     (let* ((d (make-dipole-from-n 10))
            (d* (clone d)))
       (equal (factors d*) '(2 5))))

(deftest test-dipole-mult-1 ()
     (let* ((d1 (make-dipole-from-n 10))
            (d2 (make-dipole-from-n 6))
            (d* (mult d1 d2)))
       (and (equal (factors d*) '(3 5))
            (equal (factors (compose d*)) '(6 10)))))

(deftest test-dipole-is-square-1 ()
         (let* ((d (make-dipole-from-n 9)))
           (is-square d)))

(deftest test-dipole-is-square-2 ()
         (let* ((d (make-dipole-from-n 17)))
           (not (is-square d))))

(deftest test-graham-func-1 ()
         (let* ((g (make-instance 'graham-function)))
           (setf (n g) 5)
           (eq (n g) 5)))

(deftest test-make-graham-func-1 ()
         (let* ((g (make-graham-function 5)))
           (eq (n g) 5)))

;(deftest test-graham-func-5 ()
;         (let*
;           ((g (make-graham-function 5)))
;           (equal (getf (solve g) 'factors) '(5 8 10))))
(deftest test-graham-func-1 ()
    (let ((g (make-graham-function 1)))
      (equal (getf (solve g) 'factors) '(1))))

(deftest test-graham-func-2 ()
    (let ((g (make-graham-function 2)))
      (equal (getf (solve g) 'factors) '(2 3 6))))

(deftest test-graham-func-3 ()
    (let ((g (make-graham-function 3)))
      (equal (getf (solve g) 'factors) '(3 6 8))))

(deftest test-graham-func-4 ()
    (let ((g (make-graham-function 4)))
      (equal (getf (solve g) 'factors) '(4))))

(deftest test-graham-func-5 ()
    (let ((g (make-graham-function 5)))
       (equal (getf (solve g) 'factors) '(5 8 10))))

(deftest test-graham-func-6 ()
    (let ((g (make-graham-function 6)))
      (equal (getf (solve g) 'factors) '(6 8 12))))

(deftest test-graham-func-7 ()
    (let ((g (make-graham-function 7)))
      (equal (getf (solve g) 'factors) '(7 8 14))))

(deftest test-graham-func-8 ()
    (let ((g (make-graham-function 8)))
      (equal (getf (solve g) 'factors) '(8 10 12 15))))

(deftest test-graham-func-9 ()
    (let ((g (make-graham-function 9)))
      (equal (getf (solve g) 'factors) '(9))))

(deftest test-graham-func-10 ()
    (let ((g (make-graham-function 10)))
      (equal (getf (solve g) 'factors) '(10 12 15 18))))

(deftest test-graham-func-11 ()
    (let ((g (make-graham-function 11)))
      (equal (getf (solve g) 'factors) '(11 18 22))))

(deftest test-graham-func-12 ()
    (let ((g (make-graham-function 12)))
      (equal (getf (solve g) 'factors) '(12 15 20))))

(deftest test-graham-func-13 ()
    (let ((g (make-graham-function 13)))
      (equal (getf (solve g) 'factors) '(13 18 26))))

(deftest test-graham-func-14 ()
    (let ((g (make-graham-function 14)))
      (equal (getf (solve g) 'factors) '(14 15 18 20 21))))

(deftest test-graham-func-15 ()
    (let ((g (make-graham-function 15)))
      (equal (getf (solve g) 'factors) '(15 18 20 24))))

(deftest test-graham-func-16 ()
    (let ((g (make-graham-function 16)))
      (equal (getf (solve g) 'factors) '(16))))

(deftest test-graham-func-17 ()
    (let ((g (make-graham-function 17)))
      (equal (getf (solve g) 'factors) '(17 18 34))))

(deftest test-graham-func-18 ()
    (let ((g (make-graham-function 18)))
      (equal (getf (solve g) 'factors) '(18 24 27))))

(deftest test-graham-func-19 ()
    (let ((g (make-graham-function 19)))
      (equal (getf (solve g) 'factors) '(19 24 27 38))))

(deftest test-graham-func-20 ()
    (let ((g (make-graham-function 20)))
      (equal (getf (solve g) 'factors) '(20 24 30))))

(deftest test-graham-func-21 ()
    (let ((g (make-graham-function 21)))
      (equal (getf (solve g) 'factors) '(21 27 28))))

(deftest test-graham-func-22 ()
    (let ((g (make-graham-function 22)))
      (equal (getf (solve g) 'factors) '(22 24 33))))

(deftest test-graham-func-23 ()
    (let ((g (make-graham-function 23)))
      (equal (getf (solve g) 'factors) '(23 24 27 46))))

(deftest test-graham-func-24 ()
    (let ((g (make-graham-function 24)))
      (equal (getf (solve g) 'factors) '(24 27 32))))

(deftest test-graham-func-25 ()
    (let ((g (make-graham-function 25)))
      (equal (getf (solve g) 'factors) '(25))))

(deftest test-graham-func-26 ()
    (let ((g (make-graham-function 26)))
      (equal (getf (solve g) 'factors) '(26 27 32 39))))

(deftest test-graham-func-27 ()
    (let ((g (make-graham-function 27)))
      (equal (getf (solve g) 'factors) '(27 28 30 32 35))))

(deftest test-graham-func-28 ()
    (let ((g (make-graham-function 28)))
      (equal (getf (solve g) 'factors) '(28 32 35 40))))

(deftest test-graham-func-29 ()
    (let ((g (make-graham-function 29)))
      (equal (getf (solve g) 'factors) '(29 32 58))))

(deftest test-graham-func-30 ()
    (let ((g (make-graham-function 30)))
      (equal (getf (solve g) 'factors) '(30 35 42))))

(deftest test-graham-func-31 ()
    (let ((g (make-graham-function 31)))
      (equal (getf (solve g) 'factors) '(31 32 62))))

(deftest test-graham-func-32 ()
    (let ((g (make-graham-function 32)))
      (equal (getf (solve g) 'factors) '(32 40 45))))

(deftest test-graham-func-33 ()
    (let ((g (make-graham-function 33)))
      (equal (getf (solve g) 'factors) '(33 35 40 42 44))))

(deftest test-graham-func-34 ()
    (let ((g (make-graham-function 34)))
      (equal (getf (solve g) 'factors) '(34 35 42 45 51))))

(deftest test-graham-func-35 ()
    (let ((g (make-graham-function 35)))
      (equal (getf (solve g) 'factors) '(35 40 42 48))))

(defun %g (n) (solve (make-graham-function n)))

(deftest test-graham-func-36 ()
    (let ((g (make-graham-function 36)))
      (equal (getf (solve g) 'factors) '(36))))

(deftest test-graham-func-100 ()
    (let ((g (make-graham-function 100)))
      (equal (getf (solve g) 'factors) '(100))))

(cybertiggyr-test:run)
