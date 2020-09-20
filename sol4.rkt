#lang racket
(provide (all-defined-out))

(define (number tree) (car tree)) ; get node number
(define (right_child tree) (car (cdr (cdr tree)))) ; get right subtree
(define (left_child tree) (car (cdr tree))) ;get left subtree
;problem 1
(define (check_bst tree)
  ( if (null? tree) #t ( letrec (
         [node_number (number tree)] ;current node number
         [lst (left_child tree)] ; left sub tree
         [rst (right_child tree)] ;right sub tree
         [check_right (lambda(rtree tnumber)(if (null? rtree)#t ( and (< tnumber (number rtree)) (check_right (right_child rtree) tnumber ) ; all elements of subtree is biggner than current node
                                                                      (check_right (left_child rtree) tnumber) )))]
         [check_left  (lambda(ltree tnumber)(if (null? ltree)#t ( and (> tnumber (number ltree)) (check_left (left_child ltree) tnumber) ; all elements of subtree is samller than current node
                                                                         (check_left (right_child ltree) tnumber) )))]
         )
      (and (check_right rst node_number) (check_left lst node_number) (check_bst rst) (check_bst lst))) ))

(define (apply f tree) (if (null? tree) '() ( letrec(
         [node_number (number tree)] 
         [lst (left_child tree)]
         [rst (right_child tree)]
         )
      (cons (f node_number) (cons (apply f lst) (cons (apply f rst) null))))))

;problem 2
(define (my-append xs ys) ; list append function
(if (null? xs) ys
(cons (car xs) (my-append (cdr xs) ys))))

;fucntion make all node numbers into list
(define (travel tree numbers) (if (null? tree) numbers ( letrec(
         [node_number (number tree)] 
         [lst (left_child tree)]
         [rst (right_child tree)]
         )( my-append (travel lst (cons (number tree) numbers)) (travel rst null)))))

;check ys has all elements of xs
(define (same xs ys) (if (null? xs) #t
  (letrec ([xs1 (car xs)]
       [same_helper (lambda(xs1 ys_)(if (null? ys_) #f  (or (= xs1 (car ys_)) (same_helper xs1 (cdr ys_))) ))] 
       )
    ( if (null? ys) #t (and (same_helper xs1 ys) (same (cdr xs) ys))))))

;problem 3
(define (equals t1 t2) (if(and (check_bst t1 ) (check_bst t2) ) (let([xs (travel t1 null)]
                                                                      [ys (travel t2 null)])
                                                                   (cond [(and (null? xs)(null? ys)) #t]
                                                                              [(null? xs) #f ]
                                                                              [(null? ys) #f]
                                                                              [#t (and (same xs ys) (same ys xs))]
                                                                              )) #f) )

     