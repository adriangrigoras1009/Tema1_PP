#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue
(define-struct counter (index tt et queue close) #:transparent)
(define (empty-counter index)           ; testată de checker
  (define C (make-counter index 0 0 empty-queue 0))
  C)

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (define x (cons name items))
    (define y (list x))
    (define total_tt (counter-tt ((tt+ C) items)))
    (define total_et (if (queue-empty? (counter-queue C))
                         (counter-et ((et+ C) items))
                         (counter-et C)))
    (define C_nou (struct-copy counter C [queue (if (queue-empty? (counter-queue C))
                                                    (enqueue x empty-queue)
                                                    (enqueue x (counter-queue C)))] [tt total_tt] [et total_et]))
    C_nou))

(define tt+
  (lambda (C)
    (lambda (minutes)
      (define x (+ (counter-tt C) minutes))
      (define C_nou (struct-copy counter C [tt x]))
      C_nou)))

(define et+
  (lambda (C)
    (lambda (minutes)
      (define x (+ (counter-et C) minutes))
      (define C_nou (struct-copy counter C [et x]))
      C_nou)))

(define (update f counters index)
  (map (lambda (x) (if (= (counter-index x) index) (f x) x)) counters))

(define (remove-first-from-counter C)   ; testată de checker
  (if (queue-empty? (counter-queue C))
      C
      (if (null? (dequeue (counter-queue C)))
          (struct-copy counter C [queue empty-queue] [tt 0]
                       [et 0])
          (struct-copy counter C [queue (dequeue (counter-queue C))] [tt (foldl (lambda (x acc) (+ acc (cdr x))) 0 (queue-left (dequeue (counter-queue C))))]
                       [et (if (queue-empty? (dequeue (counter-queue C)))
                               0
                               (cdr (top (dequeue (counter-queue C)))))]
                       ))
      ))

(define functie
  (lambda (counters)
    (define primul (car counters))
    (foldl (lambda (x acc)
             (if (< (cdr x) (cdr acc))
                 x
                 acc))
           primul
           (cdr counters))))
(define (min-tt counters)
   (define lista (foldl (lambda (x acc) (append acc (list (cons (counter-index x) (counter-tt x))))) '() counters))
   (functie lista))
(define (min-et counters)
  (define lista (foldl (lambda (x acc) (append acc (list (cons (counter-index x) (counter-et x))))) '() counters))
   (functie lista))

(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C [tt (if (> minutes (counter-tt C))
                                   0
                                   (- (counter-tt C) minutes))]
                 [et (if (> minutes (counter-et C))
                         0
                         (- (counter-et C) minutes))])))

  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
;(define (serve requests fast-counters slow-counters)
 ; 'your-code-here)


(define (help_add index counters name n-items)
  (define casa_update (if (null? (filter (lambda (x) (and (= (counter-index x) index) (= (counter-close x) 0))) counters))
                          null
                          ((add-to-counter name n-items) (car (filter (lambda (x)(and (= (counter-index x) index) (= (counter-close x) 0))) counters)))))
  (update (lambda (x) casa_update) counters index))

(define (help_remove index counters)
  (define casa_update (if (null? (filter (lambda (x) (= (counter-index x) index)) counters))
                           null
                           (remove-first-from-counter (car (filter (lambda (x) (= (counter-index x) index)) counters)))))
  (update (lambda (x) casa_update) counters index))
                                                                                                 

(define (suma-tt counters)
  (foldl (lambda (x acc) (+ (counter-tt x) acc)) 0 counters))

(define (lista_oameni counters lista_noua)
  (if (null? counters)
      lista_noua
      (lista_oameni (cdr counters) (append lista_noua (list (cons (counter-index (car counters)) (counter-queue (car counters))))))))

(define (help_close counters index)
  (update
   (lambda (y) (if (null? (filter (lambda (x) (= index (counter-index x))) counters))
                         null
                         (struct-copy counter (car (filter (lambda (x) (= index (counter-index x))) counters)) [close 1])))
   counters
   index))
  

(define (serve_helper requests fast-counters slow-counters lista_clienti)
  (define (add name n-items)
    (if (> n-items ITEMS)
        (serve_helper (cdr requests)
               fast-counters
               (help_add (car (min-tt (filter (lambda (C) (= (counter-close C) 0)) slow-counters))) slow-counters name n-items)
               lista_clienti)
        (serve_helper (cdr requests)
               (help_add (car (min-tt (filter (lambda (C) (= (counter-close C) 0)) (append fast-counters slow-counters)))) fast-counters name n-items)
               (help_add (car (min-tt (filter (lambda (C) (= (counter-close C) 0)) (append fast-counters slow-counters)))) slow-counters name n-items)
               lista_clienti)))
  
  (define (delay_function index minutes)
    (serve_helper (cdr requests)
           (update (lambda (x) (if (= index (counter-index x))
                                     ((et+ ((tt+ x) minutes)) minutes)
                                     x)) fast-counters index)
           (update (lambda (x) (if (= index (counter-index x))
                                     ((et+ ((tt+ x) minutes)) minutes)
                                     x)) slow-counters index)
           lista_clienti))
    
  (define (ensure_function index start lista nr)
    (if (< index
           (/ (suma-tt (filter (lambda (C) (= (counter-close C) 0)) (append fast-counters slow-counters))) start))
        (ensure_function index (+ 1 start) (append lista (list (+ 1 nr))) nr)
        (serve_helper (cdr requests)
               fast-counters
               (append slow-counters (map (lambda (x) (empty-counter x)) lista))
               lista_clienti)))

  (define (late_f number)
    (let loop ((fast fast-counters)
               (slow slow-counters)
               (lista_noua lista_clienti)
               (mins number))
      (if (= mins 0)
          (serve_helper (cdr requests)
                 fast
                 slow
                 lista_noua)
          (loop  (map (lambda (x) (if (= 1 (counter-et x))
                                     (struct-copy counter x
                                                  [queue (if (null? (counter-queue x))
                                                             (counter-queue x)
                                                             (dequeue (counter-queue x)))]
                                                  [tt (sub1 (counter-tt x))]
                                                  [et (if (queue-empty? (dequeue (counter-queue x)))
                                                          0
                                                          (cdr (top (dequeue (counter-queue x)))))])
                                     ((pass-time-through-counter 1) x))) fast)
                 (map (lambda (x) (if (= 1 (counter-et x))
                                     (struct-copy counter x
                                                  [queue (if (null? (counter-queue x))
                                                             (counter-queue x)
                                                             (dequeue (counter-queue x)))]
                                                  [tt (sub1 (counter-tt x))]
                                                  [et (if (queue-empty? (dequeue (counter-queue x)))
                                                          0
                                                          (cdr (top (dequeue (counter-queue x)))))])
                                     ((pass-time-through-counter 1) x))) slow)
                 (append lista_noua (map (lambda (x) (cons (counter-index x) (car (top (counter-queue x)))))                                                    
                                         (filter (lambda (x) (and (not (queue-empty? (counter-queue x))) (= (counter-et x) 1))) (append fast slow))))
                 (sub1 mins)))))
                                                  
             
  (define (close_f index)
    (serve_helper (cdr requests)
           (help_close fast-counters index)
           (help_close slow-counters index)
           lista_clienti))
    
                
        
  (if (null? requests)
      (cons lista_clienti (lista_oameni (append (filter (lambda (x) (not (queue-empty? (counter-queue x)))) fast-counters) (filter (lambda (x) (not (queue-empty? (counter-queue x)))) slow-counters)) '()))
      (match (car requests)
        [(list 'ensure index) (ensure_function index (length (filter (lambda (C) (= (counter-close C) 0)) (append fast-counters slow-counters))) '() (length (append fast-counters slow-counters)))]
        [(list 'close index) (close_f index)]
        [(list name n-items) (add name n-items)]
        [(list 'delay index minutes) (delay_function index minutes)]
        [number (late_f number)]
        )))
  
(define (serve requests fast-counters slow-counters)
  (serve_helper requests fast-counters slow-counters '()))