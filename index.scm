(load "support/genhtml.scm")
(load "support/gorgonzola.scm")

(define page-contents
  ($SEQUENTIALLY
   (H1 (TEXT "Shruti Nath's home page"))
   (section "Software")

   (UL
    
    (LI 
     (TEXT (simple-link lifting-web-page "Lifting Scheme")
	   "  Wavelet based scheme for irregular mesh compression "
	   "extended to a spatio-temporal framework."))

    (LI 
     (TEXT (simple-link swa-web-page "Auditing tool for imbalanced learning")
	   " Sample weights applied under imbalanced learning need to be correct. "
	   " This tool checks that for non-deterministic estimators properties"
	   "such as equivalence between weighted and repeated samples are respected."))

    (LI (TEXT (simple-link xarray-lazy-stream-web-page "Xarray lazy streamer")
	      " allows streaming and on-the-fly batching of large multi-dimensional."
		  "data. It is still a work in progress with the hope of pipelining weather"
		  "data for cloud-based AI model training." ))

    )

   (section "Papers")
   (TEXT "See " (simple-link "papers.html" "this web page") ".")

(define page 
  (gorg-page "Shruti Nath's Home Page" "./" page-contents))

(print-html page)