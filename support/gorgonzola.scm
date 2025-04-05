(require 'posix-time)

(define lifting-web-page "https://github.com/snath-xoc/Lifting")
(define swa-web-page "https://github.com/snath-xoc/sample-weight-audit-nondet/tree/main")
(define oxford-web-page "https://www.physics.ox.ac.uk/our-people/nath")
(define xarray-lazy-stream-web-page "https://github.com/snath-xoc/xarray_loader/tree/master")


(define (on-paperdir filename)
  (string-append "https://shrutinath-xoc.github.io/~gorgonzola/papers/ filename))

(define (simple-link link name) 
  (A:HREF link (TEXT name)))
	    
(define gorg-maintainer
  (simple-link "mailto:shrutinath.xoc@gmail.com" "Shruti Nath"))

(define (gorg-head titl topdir)
  (define (href name)
    (list 'HREF (string-append topdir name)))
  (HEAD
   ($SEQUENTIALLY
    (A:NAME "top-of-logo" (TEXT ""))
    (META '(http-equiv "name") '(content "Shruti Nath Home Page"))
    (META '(http-equiv "distribution") '(content "global"))
    (TITLE ($VERBATIM titl))
    (LINK '(rel "BOOKMARK") (list 'title titl) (href "index.html"))
    (LINK '(rel "Contents") (href "index.html")))))

(define horizontal-rule
  (P (HR '(noshade)
	 '(width "90%")
	 '(size "1"))
     '(align "center")))

(define (gorg-tail topdir)
  ($SEQUENTIALLY
   horizontal-rule
   (TEXT "This page maintained by: " 
	 gorg-maintainer
	 ". Last updated: "
	 (asctime (localtime (current-time)))
	 )))

(define (gorg-body titl topdir contents)
  (BODY
   ($SEQUENTIALLY contents
		  (gorg-tail topdir))
   '(text "#000000")
   '(bgcolor "#FFFFFF")))
   
(define (gorg-page titl topdir contents)
  ($SEQUENTIALLY
   (DOCTYPE)
   ($NEWLINE)
   (comment "This file was automatically generated. DO NOT EDIT!") 
   ($NEWLINE)
   (HTML 
    ($SEQUENTIALLY
     (gorg-head titl topdir)
     (gorg-body titl topdir contents)
     ))))
	      
(define (section name)
  ($SEQUENTIALLY
   (H2 (TEXT name))))

(define Shruti-Nath 
  (simple-link "https://shrutinath-xoc.github.io/~gorgonzola" "Shruti Nath"))


(define (create-abstract text)
  text)


