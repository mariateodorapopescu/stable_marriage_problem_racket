#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
  
  
  (filter (lambda(y) (not (equal? y '()))) (foldl (lambda (x result)
           (let* ((a (car x)) (b (cdr x)))
             (if (or (better-match-exists? a b (get-pref-list wpref a) mpref (map (lambda (elem) (cons (cdr elem) (car elem))) engagements))
                     (better-match-exists? b a (get-pref-list mpref b) wpref engagements))
                 (append result (list (cons a b)))
                 (append result '())
                 )))
         '() engagements)
  ))
      

; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.

 (define (engage free-men engagements mpref wpref)
     (let define1 ((single-men free-men) (eng engagements))
      (if (null? single-men)
          eng
          (let define2 ((men-pref (get-pref-list mpref (car single-men)))
                        (man (car single-men)))
            (let ((partener (get-partner eng (car men-pref)))
                  (women-pref(get-pref-list wpref (car men-pref))))
              (if(null? men-pref)
                 eng
                 (if (not partener)
                     (define1 (cdr single-man) (cons (cons (car men-pref) man) eng))
                     (if (preferable? women-pref  man partener)
                        (define1 (cdr (append single-men (list partener)))
                          (let built ((L engagements)
                               (mpreff men-pref)
                               (the-men men))
                            (map(lambda (x)
                                  (if (equal? (car mpreff) (car x))
                                      (cons (car x) the-men)
                                      (cons (car x) (cdr x))))
                                L)))
                        (define2 (cdr men-pref) man)))))))))


;(define (engage free-men engagements mpref wpref)
;  (let define1 ((single-men free-men) (eng engagements) (new-eng '()))
;    (if (null? single-men)
;        (append new-eng eng)
;        (let* ((man (car single-men))
;               (men-pref (get-pref-list mpref man))
;               (partner (get-partner eng (car men-pref)))
;               (women-pref (get-pref-list wpref (car men-pref))))
;          (if (null? men-pref)
;              (append new-eng eng)
;              (if (not partner)
;                  (loop (cdr single-men) (cons (cons (car men-pref) man) eng) new-eng)
;                  (let* ((old-partner (cdr partner))
;                         (old-partner-pref (get-pref-list wpref old-partner))
;                         (preferable? (preferable? women-pref man old-partner)))
;                    (if preferable?
;                        (let ((new-eng (cons (cons (car men-pref) man) new-eng)))
;                          (define1 (cdr single-men) eng new-eng))
;                        (define1 (cdr single-men) eng new-eng)))))))))


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (let* ((the-men (get-men mpref))
         (the-eng-list '()))
  (engage the-men the-eng-list mpref wpref)))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldl (lambda (x acc) (append acc (list (car x) (cdr x)))) '() pair-list))

