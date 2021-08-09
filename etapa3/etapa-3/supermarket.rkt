#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (define C (make-counter index 0 0 empty-queue))
  C)

(define (update f counters index)
  (map (lambda (x) (if (= (counter-index x) index) (f x) x)) counters))

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


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
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
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (help_add index counters name n-items)
  (define casa_update (if (null? (filter (lambda (x) (= (counter-index x) index)) counters))
                          null
                          ((add-to-counter name n-items) (car (filter (lambda (x) (= (counter-index x) index)) counters)))))
  (update (lambda (x) casa_update) counters index))

(define (help_remove index counters)
  (define casa_update (if (null? (filter (lambda (x) (= (counter-index x) index)) counters))
                           null
                           (remove-first-from-counter (car (filter (lambda (x) (= (counter-index x) index)) counters)))))
  (update (lambda (x) casa_update) counters index))
                                                                                                 

(define (suma-tt counters)
  (foldl (lambda (x acc) (+ (counter-tt x) acc)) 0 counters))

(define (serve_helper requests fast-counters slow-counters lista_clienti)
  (define (add name n-items)
    (if (> n-items ITEMS)
        (serve_helper (cdr requests)
               fast-counters
               (help_add (car (min-tt slow-counters)) slow-counters name n-items)
               lista_clienti)
        (serve_helper (cdr requests)
               (help_add (car (min-tt (append fast-counters slow-counters))) fast-counters name n-items)
               (help_add (car (min-tt (append fast-counters slow-counters))) slow-counters name n-items)
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
    
  (define (ensure_function index start lista)
    (if (< index
           (/ (suma-tt (append fast-counters slow-counters)) start))
        (ensure_function index (+ 1 start) (append lista (list (+ 1 start))))
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
                                                  
             
             

        
  (if (null? requests)
      (let loop_append ((case_toate (cdr (append fast-counters slow-counters)))
                        (lista_clienti_aux (list lista_clienti (car (append fast-counters slow-counters))))
                        )
        (if (null? (cdr case_toate))
            (append lista_clienti_aux (list (car case_toate)))
            (loop_append (cdr case_toate) (append lista_clienti_aux (list (car case_toate)))))) 
      (match (car requests)
        [(list 'ensure index) (ensure_function index (length (append fast-counters slow-counters)) '())]
        [(list name n-items) (add name n-items)]
        [(list 'delay index minutes) (delay_function index minutes)]
        [number (late_f number)]
        )))
  
(define (serve requests fast-counters slow-counters)
  (serve_helper requests fast-counters slow-counters '()))


  
  
        

