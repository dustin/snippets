; arch-tag: 0D1F8723-1DB6-11D9-A167-000A957659CC
(load "lcs.scm")

(define l1 '(1 0 1 2 3 4 5 98 1 2 3))
(define l2 '(14 11 5 8 9 0 1 2 3 4 23 99 3 2 1))

(dbg-subseq
 (commonSubseqs l1 l2 list-ref length))

(lcs l1 l2 list-ref length)

; OK, now try it with a string

(define s1 "Dustin was here today")
(define s2 "Noelani was here yesterday")

(dbg-subseq
 (commonSubseqs s1 s2 string-ref string-length))

s1
s2
(string-length s1)(string-length s2)
(lcs s1 s2 string-ref string-length)
