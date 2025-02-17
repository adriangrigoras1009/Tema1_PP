#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (define C (make-counter index 0 0 '()))
  C)


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (update f counters index)
  (map (lambda (x) (if (= (counter-index x) index) (f x) x)) counters))


; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.

(define tt+
  (lambda (C)
    (lambda (minutes)
      (define x (+ (counter-tt C) minutes))
      (define C_nou (struct-copy counter C [tt x]))
      C_nou)))




; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define et+
  (lambda (C)
    (lambda (minutes)
      (define x (+ (counter-et C) minutes))
      (define C_nou (struct-copy counter C [et x]))
      C_nou)))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define add-to-counter
  (lambda (C)
    (lambda (name)
      (lambda (n-items)
        (define x (cons name n-items))
        (define y (list x))
        (define total_tt (counter-tt ((tt+ C) n-items)))
        (define total_et (if (null? (counter-queue C))
                             (counter-et ((et+ C) n-items))
                             (counter-et C)))
        (define C_nou (struct-copy counter C [queue (append (counter-queue C) y)] [tt total_tt] [et total_et]))
        C_nou))))


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)
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
   ; lista)
;  (functie lista))); folosind funcția de mai sus
(define (min-et counters)
  (define lista (foldl (lambda (x acc) (append acc (list (cons (counter-index x) (counter-et x))))) '() counters))
   (functie lista))
  ; folosind funcția de mai sus


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (if (null? (counter-queue C))
      C
      (if (null? (cdr (counter-queue C)))
          (struct-copy counter C [queue '()] [tt 0]
                       [et 0])
          (struct-copy counter C [queue (cdr (counter-queue C))] [tt (foldl (lambda (x acc) (+ acc (cdr x))) 0 (cdr (counter-queue C)))]
                       [et (cdr (cadr (counter-queue C)))]
                       ))))
    

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)
(define (help_add index counters name n-items)
  (define casa_update (if (null? (filter (lambda (x) (= (counter-index x) index)) counters))
                          null
                          (((add-to-counter (car (filter (lambda (x) (= (counter-index x) index)) counters))) name) n-items)))
  (update (lambda (x) casa_update) counters index))

(define (help_remove index counters)
  (define casa_update (if (null? (filter (lambda (x) (= (counter-index x) index)) counters))
                           null
                           (remove-first-from-counter (car (filter (lambda (x) (= (counter-index x) index)) counters)))))
  (update (lambda (x) casa_update) counters index))

(define (suma-tt counters)
  (foldl (lambda (x acc) (+ (counter-tt x) acc)) 0 counters))


(define (serve requests fast-counters slow-counters)
  (define (add name n-items)
    (if (> n-items ITEMS)
        (serve (cdr requests)
               fast-counters
               (help_add (car (min-tt slow-counters)) slow-counters name n-items))
        (serve (cdr requests)
               (help_add (car (min-tt (append fast-counters slow-counters))) fast-counters name n-items)
               (help_add (car (min-tt (append fast-counters slow-counters))) slow-counters name n-items))))
  
  (define (remove_first_function)
    (if (null? (append (filter (lambda (x) (not (null? (counter-queue x)))) fast-counters) (filter (lambda (x) (not (null? (counter-queue x)))) slow-counters)))
        (serve (cdr requests)
               fast-counters
               slow-counters)
        (serve (cdr requests)
               (help_remove (car (min-et (append (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) fast-counters) (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) slow-counters)))) fast-counters)
               (help_remove (car (min-et (append (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) fast-counters) (filter (lambda (x) (and (> (counter-et x) 0) (not (null? (counter-queue x))))) slow-counters)))) slow-counters))))
  
  (define (delay_function index minutes)
    (serve (cdr requests)
           (update (lambda (x) (if (= index (counter-index x))
                                     ((et+ ((tt+ x) minutes)) minutes)
                                     x)) fast-counters index)
           (update (lambda (x) (if (= index (counter-index x))
                                     ((et+ ((tt+ x) minutes)) minutes)
                                     x)) slow-counters index)))
    
  (define (ensure_function index start lista)
    (if (< index
           (/ (suma-tt (append fast-counters slow-counters)) start))
        (ensure_function index (+ 1 start) (append lista (list (+ 1 start))))
        (serve (cdr requests)
               fast-counters
               (append slow-counters (map (lambda (x) (empty-counter x)) lista)))))
    
    

        
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure index) (ensure_function index (length (append fast-counters slow-counters)) '())]
        [(list name n-items) (add name n-items)]
        [(list 'delay index minutes) (delay_function index minutes)]
        [(list 'remove-first) (remove_first_function)])))

        

 