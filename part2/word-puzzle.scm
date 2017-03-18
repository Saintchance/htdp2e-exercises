;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname word-game-revise) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; htdp2e, 12.3, Word puzzle
; exercise 209 - 214
; some skipped

(define DICT (read-lines "/usr/share/dict/words"))

; Data Definitions
; Letter: 1String
; Word: List-of-1String
; LOW: List-of-Words
; Dict: List-of-Strings

; String -> Dict
; find all alternative words made up from the same
; letters as the given string
(define (alter-words str)
  (find-in-dict (all-alter (explode str))))

; LOW -> Dict
; find all real "word" in a LOW, refer to the dictionary
(define (find-in-dict low)
  (cond
    ((empty? low) '())
    (else (append (if (in-dict? (first low) DICT)
                      (list (implode (first low)))
                      '())
                  (find-in-dict (rest low))))))

; Word Dict -> Boolean
; test if a Word in the dictionary
(define (in-dict? w dict)
  (cond
    ((empty? dict) #f)
    (else (or (string=? (implode w) (first dict))
              (in-dict? w (rest dict))))))

; Word -> LOW
; get all permutations of the Word
(define (all-alter w)
  (cond
    ((empty? w) '())
    (else (insert (first w) (all-alter (rest w))))))

; Letter LOW -> LOW
; insert a Letter to everywhere of a given LOW
(define (insert l low)
  (cond
    ((empty? low) (list (list l)))
    ((empty? (rest low)) (insert-word l (first low)))
    (else (append (insert-word l (first low))
                  (insert l (rest low))))))

; Letter Word -> LOW
; insert a Letter to every position of a Word.
(define (insert-word l w)
  (do-insert l '() w))

; Letter Word Word -> LOW
; the actual function to do the insert, because we need
; a place to keep the value pre of the inserted letter
(define (do-insert l pre post)
  (cond
    ((empty? post) (list (append pre (list l) post)))
    (else (append (list (append pre (list l) post))
                  (do-insert
                   l (append pre (list (first post)))
                   (rest post))))))


