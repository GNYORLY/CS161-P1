
;PAD takes an integer input N and returns the Nth value of the Padovan sequence
(defun PAD (N)
    ;if N is below 3, return 1 since the first 3 values of the sequence is 1
    ;else recursively compute the sum of PAD(N-2) and PAD(N-3)
    (cond 
        ((= N 0) 1)
        ((= N 1) 1)
        ((= N 2) 1)
        (t (+ (PAD (- N 2)) (PAD (- N 3)))))
)

;SUMS takes an integer input N and returns the number of additions computed by PAD to find the Nth Padovan number
(defun SUMS (N)
    ;if N is below 3, there are no additions so return 0
    ;else there is an addition so recursively add 1 to the sum of SUMS(N-2) and SUMS(N-3) in order to count them
    (cond 
        ((= N 0) 0)
        ((= N 1) 0)
        ((= N 2) 0)
        (t (+ (SUMS (- N 2)) (SUMS (- N 3)) 1)))      
)

;ANON takes in a list or atom and outputs the same list or atom with all symbols turned into ?
(defun ANON (L)
    (cond 
        ;if L is not a list, just return '?
        ((not (listp L)) '?) 
        ;if the current element of L is a nested list, recursively call ANON with this element and insert 
        ;it into the tail of L, if this element is not the last element of L, call ANON with the tail as well
        ((listp (car L)) (cons (ANON (car L)) 
            (cond 
                ((null (cdr L)) (cdr L))
                (t (ANON (cdr L))))))
        ;if the current element of L is not a nested list, replace this element with ? and call ANON with the
        ;tail of L if it was not the last element
        ((null (cdr L)) '(?) )
        (t (cons '? (ANON (cdr L))))
    )
)
