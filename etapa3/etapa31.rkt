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
  (filter (lambda (x)
            (let ((man (car x))
                  (woman (cdr x))
                  (man-pref-list (get-pref-list mpref (cdr x)))
                  (woman-pref-list (get-pref-list wpref (car x)))
                  (woman-first (map (lambda (x) (cons (cdr x) (car x))) engagements))
                  )
              (if (better-match-exists? woman man man-pref-list wpref engagements)
                  #t
                (if  (better-match-exists? man woman woman-pref-list mpref woman-first)
                     #t
                     #f)
              )))
          engagements))


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
  (let loop ((those-men free-men) (eng engagements))
    (if (null? those-men)
        eng
        (let other-loop ((man-pref (if (get-pref-list mpref (car those-men)) (get-pref-list mpref (car those-men)) '()))) 
          (if (null? man-pref)
              (loop (cdr those-men) eng)
              (let* ((w (car man-pref))
                    (m (car those-men)))
                (if (get-partner eng w)
                    (if (preferable? (get-pref-list wpref w) m (get-partner eng w))
                        (loop (append (cdr those-men) (list (get-partner eng w))) (update-engagements eng w m))
                        (other-loop (cdr man-pref)))
                   (loop (cdr those-men) (append eng (list (cons w m)))))))))))

; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref)
  )


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (let cevaa ((L pair-list))
    (if (null? L)
        '()
        (append (list(car (car L)))(append (list (cdr (car L)) ) (cevaa (cdr L))) )
        )))

