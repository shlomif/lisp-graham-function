(declaim (optimize (debug 3)))

(defun mult-factors-list (n m)
    (if (or (null n) (null m))
      ; If either list is empty return the remaining elements.
      ;;;;;;;;;;;;;;; (copy-list (concatenate 'list n m))
      (copy-list (concatenate n m))
      ; else if the first elements are equal move on
      (let 
        ((first-n (car n))
         (first-m (car m)))
        (if (= first-n first-m)
          (mult-factors-list (cdr n) (cdr m))
          ; Otherwise - append the smallest element.
          (if (< first-n first-m)
            (cons first-n (mult-factors-list (cdr n) m))
            (cons first-m (mult-factors-list n (cdr m))))))))

