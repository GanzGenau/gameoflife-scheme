; Alias defined for legibility
(define (matrix-row-ref matrix r)
	(vector-ref matrix r))

(define (matrix-col-ref matrix c)
	(vector-map (lambda (x) (vector-ref x c)) matrix))

(define (matrix-row-size matrix)
	(vector-length matrix))

(define (matrix-col-size matrix)
	(vector-length (vector-ref matrix 0)))


(define (matrix-set! matrix i j value)
	(vector-set! (vector-ref matrix i) j value))

(define (make-matrix w h)
        (define matrix (make-initialized-vector w (lambda (x) (make-vector h))))
        matrix)

(define (matrix-ref matrix i j)
	(vector-ref (vector-ref matrix i) j))

;j return #f if out of bounds instead of causing an error
(define (bounded-matrix-ref matrix i j)
	(if (or (< i 0)
		(>= i (matrix-row-size matrix))
		(< j 0)
		(>= j (matrix-col-size matrix)))
		#f
		(matrix-ref matrix i j)))



; returns the number of neighbors living around cell i,j
(define (living-neighbors matrix i j)
	(define living 0)
	(if (bounded-matrix-ref matrix (- i 1) (- j 1)) (set! living (+ living 1)))
	(if (bounded-matrix-ref matrix (- i 1) j)       (set! living (+ living 1)))
	(if (bounded-matrix-ref matrix (- i 1) (+ j 1)) (set! living (+ living 1)))
	(if (bounded-matrix-ref matrix i       (- j 1)) (set! living (+ living 1)))
	(if (bounded-matrix-ref matrix i       (+ j 1)) (set! living (+ living 1)))
	(if (bounded-matrix-ref matrix (+ i 1) (- j 1)) (set! living (+ living 1)))
	(if (bounded-matrix-ref matrix (+ i 1) j)       (set! living (+ living 1)))
	(if (bounded-matrix-ref matrix (+ i 1) (+ j 1)) (set! living (+ living 1)))
	living)


(define (update-life input-matrix output-matrix)
	(do ((i 0 (+ i 1)))
		((>= i (matrix-row-size input-matrix)))
		(do ((j 0 (+ j 1)))
			((>= j (matrix-col-size input-matrix)))
			(if (matrix-ref input-matrix i j)
				(if (or (> (living-neighbors input-matrix i j) 3) (< (living-neighbors input-matrix i j) 2))
					(matrix-set! output-matrix i j #f)
					(matrix-set! output-matrix i j #t))
				(if (= (living-neighbors input-matrix i j) 3)
					(matrix-set! output-matrix i j #t)
					(matrix-set! output-matrix i j #f))))))

(define (print-matrix matrix)
	(do ((i 0 (+ i 1)))
		((>= i (matrix-row-size matrix)))
		(do ((j 0 (+ j 1)))
			((>= j (matrix-col-size matrix)))
			(display (if (matrix-ref matrix i j) "#" ".")))
		(newline)))

(define (life)
	(define matrix-buffer0 (make-matrix 32 64))
	(define matrix-buffer1 (make-matrix 32 64))

	(matrix-set! matrix-buffer0 14 15 #t)
	(matrix-set! matrix-buffer0 15 15 #t)
	(matrix-set! matrix-buffer0 16 15 #t)

	(matrix-set! matrix-buffer0 15 14 #t)
	(matrix-set! matrix-buffer0 14 16 #t)

	(define garbage 0)

	(define (life-loop i)
		(update-life matrix-buffer0 matrix-buffer1)
		(print-matrix matrix-buffer1) (newline)
		;(read)
		(update-life matrix-buffer1 matrix-buffer0)
		(print-matrix matrix-buffer0) (newline)
		;(read)
		(if (>= i 80)
			(exit)
			(life-loop (+ i 1))))

	(life-loop 0))
