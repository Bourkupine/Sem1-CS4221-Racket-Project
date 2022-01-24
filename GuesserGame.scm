#lang racket

;Hangman
;by Euan Bourke
;ID: 21332142
;==============

;Hangman game:
;Take guesses at letters
;Once you get enough letters, guess the word
;==============

;Commands:

;(checker 0 "[letter]") -> (Must include "") Guesses letters
;(guess "[word]") -> guesses the word
;==============

;random number generator, which will be used for setting the word to be guessed
(define rand (random 5))

;Adding a lives system (Thanks Blake Ryan for the idea on the forums)
(define lives 3)

;defining the word variable which will be used later as the word to be guessed
(define word "")

;Current List of words:
; - lambda
; - compsci
; - jedward
; - racket
; - university


;setting the word
(if (= 0 rand) (set! word "lambda")
    (if (= 1 rand) (set! word "compsci")
        (if (= 2 rand) (set! word "jedward")
            (if (= 3 rand) (set! word "racket")
                (set! word "university")))))

;splitting the word into characters
(define wordlist (string->list word))

;checks if a letter entered is contained in the word

(define i 0) ;will be used later to check if a character was found in the word

(define checker (lambda (x y) (if (and 
                                   (= (string-length y) 1) ;Data Validation: checks y is a string of length 1
                                   (char? (string-ref y 0))) ;Data Validation: checks if y is a character
                                  (if (< x (string-length word)) ;as we will be rerunning  the function, this will ensure it stops when x becomes the value of the string (when checking a list for a character, we start at 0)
                                      (if (equal? (list-ref wordlist x) (car (string->list y)))
                                          (and
                                           (set! i 1) ;sets i to 1 so at the end it can check
                                           (checker (+ x 1) y)) ;recursion
                                          (checker (+ x 1) y)) ;recurion
                                      (if (= i 1) ;after running the function until the end, it will check if the counter was set to 1 (the letter was found)
                                          (and
                                           (display "Yes \n")
                                           (set! i 0)) ;resets i so you can guess again
                                          (display "No \n")))
                                  (display "Please only enter 1 character \n")) ;displays this if y isnt valid data
                  ))
                                     

                                 
;checks if the guessed word is correct
(define guess (lambda (x) (if (= lives 0) ;start by checking if lives = 0, if they dont we continue otherwise print No lives left.
                              (print "No lives left \n")
                              (if (equal? x word)
                                  (display "Correct! \n")
                                  (and
                                   (display "Incorrect \n")
                                   (and
                                    (set! lives (- lives 1)) ;decreases lives  after an incorrect guess
                                    (and (print lives) (display " lives left \n"))))))
                ))


;Changes 1:
; - Added more complex words
; - Added a lives system (thanks to blake ryan)
; - Changed wording of responses e.g. "no" to "Incorrect"

;Changes 2:
; - formatted code to make it more readable
; - ADDED RECURSION!
; - Added data validation
;==============

;Test Run:

;> (checker 1 "a")
;Yes 
;> (checker 1 "t")
;Yes 
;> (guess "racket")
;Incorrect 
;2 lives left
;> (guess "jedward")
;Correct!

;Test Data:


;One of the following should return Yes (as all words contain one of them)
(checker 0 "a")
(checker 0 "i")

;Should return No
(checker 0 "z")

;Should return Incorrect & display 2 lives left
(guess "java")

;Should return Please only enter 1 character 
(checker 0 "aaaa")






