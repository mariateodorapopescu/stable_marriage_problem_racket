#lang racket
(define (match person engagements pref1 pref2 queue)
  (let do-sth ((eng engagements))
    (if (null? eng)
       (list person)
        (append (do-sth (cdr eng)) (list (cdr (car eng)))))))
(define E '((bobo . cora) (adi . bia)))
(define M
  '([adi  ana  bia cora]
    [bobo cora ana bia ]
    [cos  cora bia ana ]))
(define W
  '([ana  bobo adi cos ]
    [bia  adi  cos bobo]
    [cora bobo cos adi ]))
(match 'ana E W M '(cos))

(define (diff A B)
  (map (filter (lambda (x y) (eq? x y)) A B) A)
  )
(diff '(1 2 3 4) '(2 4 6 8))