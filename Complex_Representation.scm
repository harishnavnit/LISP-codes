#lang racket

; /* Implementing the representation of the complex-arithmetic package */


; /* Basic definitions */
(define z (cons 2 3))       ; z = 2 + 3i
(define (square x) (* x x))
(define (make_from_real_imag x y) (cons x y))
(define (make_from_mag_ang r a) (cons (* r (cos a)) (* r (sin a))))
(define (real_part z) (car z))
(define (imag_part z) (cdr z))
(define (magnitude z) (sqrt (+ (square (real_part z)) (square (imag_part z)))))


(make_from_real_imag (real_part z) (imag_part z))
(make_from_mag_ang (real_part z) (imag_part z))

; /* Basic Arithmetic Operations on Complex Numbers */
(define (add_complex z1 z2)
  (make_from_real_imag (+ (real_part z1) (real_part z2))
                       (+ (imag_part z1) (imag_part z2))))

(define (sub_complex z1 z2)
  (make_from_real_imag (- (real_part z1) (real_part z2))
                       (- (imag_part z1) (imag_part z2))))

(define (mul_complex z1 z2)
  (make_from_mag_ang(* (magnitude z1) (magnitude z2))
                    (+ (angle z1) (angle z2))))

(define (div_complex z1 z2) 
  (make_from_mag_ang(/ (magnitude z1) (magnitude z2))
                    (- (angle z1) (angle z2))))

; /* Tagging of Data */
(define (attach_tag type_tag contents)
  (cons type_tag contents))

(define (type_tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad Tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad Tagged datum -- CONTENTS" datum)))

; /* Checking type */
(define (rectangular? z)
  (eq? (type_tag z) 'rectangular))
(rectangular? (cons 1 2))

(define (polar? z)
  (eq? (type_tag z) 'polar))
(polar? (cons 2 3))  