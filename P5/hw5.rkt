;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; 1a)

(define (racketlist->mupllist rkts)
  (cond [(null? rkts) (aunit)]
        [#t (apair (car rkts) (racketlist->mupllist (cdr rkts)))]))


;; 1b)

(define (mupllist->racketlist mupls)
  (cond [(aunit? mupls) null]
        [#t (cons (apair-e1 mupls) (mupllist->racketlist (apair-e2 mupls)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([e1 (eval-under-env (ifgreater-e1 e) env )]
               [e2 (eval-under-env (ifgreater-e2 e) env)]
               )
           (cond [(not (and (int? e1) (int? e2))) (error "MUPL ifgreater applied to non int argument")]
                 [(> (int-num e1) (int-num e2)) (eval-under-env (ifgreater-e3 e) env)]
                 [#t (eval-under-env (ifgreater-e4 e) env)]
                 ))]
        [(mlet? e)
             (let ([valbinding (cons (mlet-var e) (eval-under-env (mlet-e e) env))])
               (eval-under-env (mlet-body e) (cons valbinding env)))]
        [(call? e)
         (let ([close (eval-under-env (call-funexp e) env)]
               [arg   (eval-under-env (call-actual e) env)])
              (if (closure? close)
                  (let ([close-body         (fun-body (closure-fun close))]
                        [close-arg-binding  (cons (fun-formal  (closure-fun close)) arg)]
                        [close-name         (fun-nameopt (closure-fun close))]
                        [close-name-binding (cons (fun-nameopt (closure-fun close)) close)]
                        [close-env          (closure-env close)]) 
                  (if close-name
                      (eval-under-env close-body (cons close-arg-binding (cons close-name-binding close-env)))
                      (eval-under-env close-body (cons close-arg-binding close-env))
                      ))
              (error "MUPL call applied to non-closure")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([e-eval (eval-under-env (fst-e e) env)])
           (if (apair? e-eval)
               (apair-e1 e-eval)
               (error "MUPL fst called on expression that does not evaluate to an apair.")))]
        [(snd? e)
         (let ([e-eval (eval-under-env (snd-e e) env)])
           (if (apair? e-eval)
               (apair-e2 e-eval)
               (error "MUPLT snd called on expression that does not evaluate to an apair.")))]
        [(isaunit? e)
         (let ([e-eval (eval-under-env (isaunit-e e) env)])
           (if (aunit? e-eval)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) 
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) 
  (if (null? lstlst)
      e2
      (mlet (caar lstlst)
            (cdar lstlst)
            (mlet* (cdr lstlst) e2))))
                                               
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons  "_y" e2))
        (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map 
  (fun "map" "f" 
       (fun "apply" "lst" 
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "f") (fst (var "lst"))) 
                            (call (var "apply") (snd (var "lst"))))))))
                         

(define mupl-mapAddN 
  (mlet "map" mupl-map (fun "mupl-mapAddN" "n"
                            (call (var "map") 
                                  (fun #f "x" (add (var "x") (var "n")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
