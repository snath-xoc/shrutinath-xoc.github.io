;;; Copyright (c) 2000 Matteo Frigo
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;

(require 'macro)

(define-syntax define-datatype
  (syntax-rules ()
    ((define-datatype <type>   ; <type> is ignored, documentation only
       (<constructor> <args> ...) ...)
     (begin
       (%match-define-constructor <constructor> <args> ...)
       ...))))

(define-syntax %match-define-constructor
  (syntax-rules ()
    ((%match-define-constructor (<constructor> <predicate>) <args> ...)
     (begin
       (%match-define-constructor <constructor> <args> ...)
       (define <predicate>
	 (let ((<c> <constructor>))
	   (lambda (thing)
	     (thing <c> (lambda _ #t) (lambda () #f)))))))
    ((%match-define-constructor <constructor> <args> ...)
     (define <constructor> 
       (letrec ((<c> 
		 (lambda (<args> ...) 
		     (lambda (token win lose)
		       (if (eq? token <c>) (win <args> ...) (lose))))))
	   <c>)))))

(define-syntax match
  (syntax-rules ()
    ((match <E> (<pattern> <exp>) ...)
     (let ((E <E>))
       (%match-aux E (<pattern> <exp>) ...)))))

(define-syntax %match-aux
  (syntax-rules (else)
    ((%match-aux E) (%match-no-match))
    ((%match-aux E (else <exp>)) <exp>)
    ((%match-aux E ((<constructor> <args> ...) <exp>) <rest> ...)
     (let ((fail (lambda () (%match-aux E <rest> ...))))
       (E <constructor> 
	  (%match-bind (<args> ...) () <exp> fail)
	  fail)))))

(define-syntax %match-bind
  (syntax-rules (_)
    ((%match-bind ((<arg> ...) <args> ...) (<formals> ...) <exp> <fail>)
     (%match-bind (<args> ...) 
		  (<formals> ... temp) 
		  (match temp
		    ((<arg> ...) <exp>)
		    (else (<fail>)))
		  <fail>))
    ((%match-bind (_ <args> ...) (<formals> ...) <exp> <fail>)
     (%match-bind (<args> ...) (<formals> ... temp) <exp> <fail>))
    ((%match-bind (<arg> <args> ...) (<formals> ...) <exp> <fail>)
     (%match-bind (<args> ...) (<formals> ... <arg>) <exp> <fail>))
    ((%match-bind () (<formals> ...) <exp> <fail>)
     (lambda (<formals> ...) <exp>))))
      
(define (%match-no-match) (error "match: no matching pattern"))


;;; Local Variables: ***
;;; eval: (put 'match 'scheme-indent-function 1) ***
;;; eval: (put '%letrec 'scheme-indent-function 2) ***
;;; eval: (put '%lambda 'scheme-indent-function 2) ***
;;; End: ***

