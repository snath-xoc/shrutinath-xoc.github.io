(load "support/genhtml.scm")
(load "support/gorgonzola.scm")

(define (phd-thesis when)
  (TEXT 
   "Ph. D. Thesis, Institute of Atmospheric and Climate Sciences"
   (BR)
   when "."))


(define (by authors)
  (case (length authors)
    ((1) (car authors))
    ((2) (TEXT (car authors) " and " (cadr authors)))
    (else
     (let ((butlast (reverse (cdr (reverse authors))))
	   (last-guy (car (reverse authors))))
       (TEXT
	(apply $SEQUENTIALLY
	       (map (lambda (guy) (TEXT guy ", ")) butlast))
	" and "
	(TEXT last-guy))))))

(define some-html
  (lambda (url remarks)
    ($SEQUENTIALLY
     (if (not (null? remarks))
	 ($SEQUENTIALLY
	  (apply $SEQUENTIALLY (map TEXT remarks))
	  (BR))
	 NIL)
     (if url
	 (simple-link url "[Full text]")
	 NIL))))

(define abstract-link
  (lambda (title authors abstract abstract-text url remarks)
    (with-output-to-file (string-append "abstracts/" abstract
					".html") 
      (lambda ()
	(print-html (athena-page "Abstract" "../../" 
			       ($SEQUENTIALLY
				(A:NAME "afterheading" (TEXT ""))
				(HR)
				(CENTER (H3 (TEXT title)))
				(CENTER (TEXT "by " (by authors) "."))
				(P (CENTER (some-html url remarks)))
				(HR)
				(P (CENTER (H3 (TEXT "Abstract"))))
				abstract-text)))))
    ($SEQUENTIALLY 
     (TEXT " ")
     (simple-link (string-append "abstracts/"
				 abstract ".html#afterheading")
		  "Abstract")
     (BR))))
     
(define paper 
  (let ((count 0))
    (lambda (title authors abstract-text url . remarks)
      (set! count (+ count 1))
      (let ((abstract
	     (string-append "abstract" (number->string count))))
	(LI 
	 ($SEQUENTIALLY
	  (STRONG (TEXT title))
	  (BR)
	  (TEXT "by " (by authors) ".")
	  (BR)
	  (some-html url remarks)
	  (if abstract-text
	      (abstract-link title authors abstract abstract-text url remarks)
	      NIL)
	  (P NIL) ))))))

(define papers-list
  (UL

   (paper "Everyone Loves File: File Storage Service (FSS) in Oracle
Cloud Infrastructure"
	  (list
           Shruti-Nath,
           "Quentin Lejeune", 
		   "Lea Beusch", 
		   "Carl-Friedrich Schleussner", "Sonia I Seneviratne")
	  (P (TEXT
	      "The degree of trust placed in climate model projections is commensurate 
	with how well their uncertainty can be quantified, particularly at timescales 
	relevant to climate policy makers. On inter-annual to decadal timescales, 
	model projection uncertainty due to natural variability dominates at the
	local level and is imperative to describing near-term and seasonal climate 
	events but difficult to quantify owing to the computational constraints of 
	producing large ensembles. To this extent, emulators are valuable tools for 
	approximating climate model runs, allowing for the exploration of the uncertainty
	space surrounding selected climate variables at a substantially reduced 
	computational cost. Most emulators, however, operate at annual to seasonal 
	timescales, leaving out monthly information that may be essential to assessing 
	climate impacts. This study extends the framework of an existing spatially 
	resolved, annual-scale Earth system model (ESM) emulator (MESMER, Beusch et al., 2020) 
	by a monthly downscaling module (MESMER-M), thus providing local monthly temperatures 
	from local yearly temperatures. We first linearly represent the mean response of the 
	monthly temperature cycle to yearly temperatures using a simple harmonic model, thus 
	maintaining month-to-month correlations and capturing changes in intra-annual variability. 
	We then construct a month-specific local variability module which generates spatio-temporally 
	correlated residuals with yearly temperature- and month-dependent skewness incorporated within. 
	The emulator's ability to capture the yearly temperature-induced monthly temperature response 
	and its surrounding uncertainty due to natural variability is demonstrated for 38 different 
	ESMs from the sixth phase of the Coupled Model Intercomparison Project (CMIP6). The emulator 
	is furthermore benchmarked using a simple gradient-boosting-regressor-based model trained on 
	biophysical information. We find that while regional-scale, biophysical feedbacks may induce 
	non-uniformities in the yearly to monthly temperature downscaling relationship, 
	statistical emulation of regional effects shows comparable skill to the more physically informed 
	approach. Thus, MESMER-M is able to statistically generate ESM-like, large initial-condition ensembles
	 of spatially explicit monthly temperature fields, providing monthly temperature probability 
	 distributions which are of critical value to impact assessments."))
          (on-paperdir "https://doi.org/10.5194/esd-13-851-2022)
	  (CITE (TEXT "Earth System Dynamics"))
          )

   
))

(define page-contents
  ($SEQUENTIALLY
   (section "Papers (co-)authored by  Shruti Nath")
   papers-list))

(define page 
  (gorgonzola "Papers (co-)authored by Shruti nath" "./" page-contents))

(print-html page)
