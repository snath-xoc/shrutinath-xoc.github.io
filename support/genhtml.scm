
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

(define-datatype html
  ($VERBATIM item)
  ($STRING item)
  ($NEWLINE)
  ($SEQUENTIALLY:l l))

(define ($SEQUENTIALLY . l)
  ($SEQUENTIALLY:l l))

(define (print-html l)
  (match l 
    (($VERBATIM item) (display item))
    (($STRING item)
     (begin
       (display "\"")
       (display item)
       (display "\"")))
    (($NEWLINE) (newline))
    (($SEQUENTIALLY:l l)
     (for-each print-html l))))

(define NIL ($SEQUENTIALLY))

(define (comment what)
  ($SEQUENTIALLY 
   ($VERBATIM "<!--")
   ($VERBATIM what)
   ($VERBATIM "-->")))

(define (DTD what)
  ($SEQUENTIALLY 
   ($VERBATIM "<!")
   ($VERBATIM what)
   ($VERBATIM ">")))

(define (space) ($VERBATIM " "))

(define (DOCTYPE)
  (DTD "DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\""))

(define (attributes a)
  ($SEQUENTIALLY:l
   (map (lambda (item)
	  ($SEQUENTIALLY ($VERBATIM (symbol->string (car item)))
			 (if (null? (cdr item))
			     NIL
			     ($SEQUENTIALLY
			      ($VERBATIM "=")
			      ($STRING (cadr item))))
			 (space)))
	a)))

(define (tag t)
  (lambda (body . attr)
    ($SEQUENTIALLY
      ($SEQUENTIALLY 
       ($VERBATIM "<") 
       ($VERBATIM t)
       ($VERBATIM " ") 
       (attributes attr)
       ($VERBATIM ">"))
      body
      ($SEQUENTIALLY
       ($VERBATIM "</")
       ($VERBATIM t)
       ($VERBATIM ">")))))

(define (unbalanced-tag t)
  (lambda (body . attr)
    ($SEQUENTIALLY
      ($SEQUENTIALLY 
       ($VERBATIM "<") 
       ($VERBATIM t)
       ($VERBATIM " ") 
       (attributes attr)
       ($VERBATIM ">"))
      body)))

(define HEAD (tag "HEAD"))
(define HTML (tag "HTML"))
(define TITLE (tag "TITLE"))
(define BODY (tag "BODY"))
(define FONT (tag "FONT"))
(define TABLE (tag "TABLE"))
(define TR:l (tag "TR"))
(define (TR . l) (TR:l ($SEQUENTIALLY:l l)))
(define TD (tag "TD"))
(define DIV (tag "DIV"))
(define CENTER (tag "CENTER"))
(define STRONG (tag "STRONG"))
(define SUP (tag "SUP"))
(define CITE (tag "CITE"))
(define CODE (tag "CODE"))
(define A (tag "A"))
(define B (tag "B"))
(define P (tag "P"))
(define EM (tag "EM"))
(define H1 (tag "H1"))
(define H2 (tag "H2"))
(define H3 (tag "H3"))
(define H4 (tag "H4"))
(define H5 (tag "H5"))
(define H6 (tag "H6"))
(define UL:l (tag "UL"))
(define (UL . l) (UL:l ($SEQUENTIALLY:l l)))
(define LI (unbalanced-tag "LI"))
(define PRE:t (tag "PRE"))
(define (PRE x) (PRE:t ($VERBATIM x)))

; shortcut for common case
(define (A:HREF link body)
  (A body (list 'href link)))

(define (A:NAME link body)
  (A body (list 'name link)))

(define (unbalanced-tag-no-body name)
  (lambda attr
    ($SEQUENTIALLY 
     ($VERBATIM "<") ($VERBATIM name) ($VERBATIM " ")
     (attributes attr)
     ($VERBATIM ">")
     ($NEWLINE))))

(define META (unbalanced-tag-no-body "META"))
(define LINK (unbalanced-tag-no-body "LINK"))
(define IMG (unbalanced-tag-no-body "IMG"))
(define HR (unbalanced-tag-no-body "HR"))
(define BR (unbalanced-tag-no-body "BR"))

; shortcut for text
(define (TEXT . l)
  ($SEQUENTIALLY:l
   (map (lambda (thing)
	  (if (string? thing)
	      ($VERBATIM thing)
	      thing))
	l)))

