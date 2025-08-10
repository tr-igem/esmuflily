;; This file is part of Esmuflily - Support for SMuFL/Ekmelos
;; Copyright (c) 2025 Thomas Richter
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;
;; File: ekmd.scm
;;
;; Parse metadata JSON file of a SMuFL-compliant font
;; and create Scheme metadata table for use in Esmuflily.
;;

(use-modules
  ((ice-9 rdelim) #:select (read-delimited)))

(define-public ekmd:dir #f)
(define-public ekmd:defaults '())
(define-public ekmd:glyphs '())

(define ekmd:dig (string->char-set "-0123456789"))
(define ekmd:num (string->char-set "0123456789.-+Ee"))

(define (ekmd:error t)
  (ly:error "JSON error: ~a" t)
  '())

(define (ekmd:next p)
  (let ((c (read-char p)))
    (case c
      ((#\sp #\ht #\lf #\cr) (ekmd:next p))
      (else c))))

(define (ekmd:word p word val)
  (let rd ((w word) (r '()))
    (if (null? w)
      (if (null? r) val (reverse-list->string r))
      (let ((c (read-char p)))
        (rd (cdr w)
          (if (eq? #t (car w))
            (cons* c r)
          (if (eqv? c (car w))
            r
            (ekmd:error "unknown word"))))))))

(define ekmd:esc '(
  (#\u . #t)
  (#\" . #\")
  (#\\ . #\\)
  (#\/ . #\/)
  (#\b . #\bs)
  (#\f . #\ff)
  (#\n . #\lf)
  (#\r . #\cr)
  (#\t . #\ht)
))

(define (ekmd:string p)
  (let ((s (read-delimited "\"\\" p 'split)))
    (case (cdr s)
      ((#\")
        (car s))
      ((#\\)
        (let* ((c (or (assv-ref ekmd:esc (read-char p))
                      (ekmd:error "unknown escape")))
               (c (if (eq? #t c)
                    (integer->char (string->number (ekmd:word p '(#t #t #t #t) 0) 16))
                    c)))
          (string-append (car s) (string c) (ekmd:string p))))
      (else
        (ekmd:error "open string") ""))))

(define (ekmd:find k s tab)
  (find (lambda (e)
    (or (eq? (car e) #t)
        (if (symbol? (car e)) (eq? (car e) k)
        (string-prefix? (car e) s))))
    tab))

(define (ekmd:new k s tmpl)
  (let ((t (ekmd:find k s tmpl)))
    (if t
      (let ((g (cons k (copy-tree (cdr t)))))
        (set! ekmd:glyphs (cons* g ekmd:glyphs))
        g)
      #f)))

(define (ekmd:object p mask tmpl)
  (let obj ((r '()))
    (case (ekmd:next p)
      ((#\,)
        (obj r))
      ((#\")
        (let* ((s (ekmd:string p))
               (k (string->symbol s))
               (m (ekmd:find k s mask))
               (v (if (eqv? #\: (ekmd:next p))
                    (ekmd:value p (if (and m (pair? (cdr m))) (cdr m) '()) tmpl)
                    #f)))
          (obj
            (if m
              (if (char? (cdr m))
                (if (eqv? #\c (cdr m))
                  (if (and (string? v) (string-prefix? "U+" v))
                    (string->number (string-drop v 2) 16)
                    v)
                (if (eqv? #\d (cdr m))
                  (begin (set! ekmd:defaults (assq-set! ekmd:defaults k v))
                         '())
                  (cons* (cdr m) v r)))
              (if (and (pair? v) (char? (car v)))
                (let ((g (or (assq k ekmd:glyphs) (ekmd:new k s tmpl))))
                  (if g
                    (let setpv ((pv v))
                      (if (null? pv)
                        r
                        (let ((i (char->integer (first pv)))
                              (v (second pv)))
                          (if (pair? v)
                            (let ((e (list-ref g i)))
                              (set-car! e (car v))
                              (if (pair? (cdr v)) (set-cdr! e (cadr v))))
                            (list-set! g i v))
                          (setpv (cddr pv)))))
                    r))
                (acons k v r)))
              r))))
      ((#\})
        r)
      (else
        (ekmd:error "open object") #f))))

(define (ekmd:value p mask tmpl)
  (let ((c (ekmd:next p)))
    (case c
      ((#\{)
        (ekmd:object p mask tmpl))
      ((#\[)
        (let lst ((l '()))
          (let ((c (ekmd:next p)))
            (case c
              ((#\,)
                (lst l))
              ((#\])
                (reverse l))
              (else
                (unread-char c p)
                (lst (cons* (ekmd:value p mask tmpl) l)))))))
      ((#\")
        (ekmd:string p))
      ((#\t)
        (ekmd:word p '(#\r #\u #\e) #t))
      ((#\f)
        (ekmd:word p '(#\a #\l #\s #\e) #f))
      ((#\n)
        (ekmd:word p '(#\u #\l #\l) '()))
      (else
        (if (char-set-contains? ekmd:dig c)
          (string->number
            (reverse-list->string
              (let num ((n (list c)) (d (read-char p)))
                (cond
                  ((eof-object? d)
                    n)
                  ((char-set-contains? ekmd:num d)
                    (num (cons* d n) (read-char p)))
                  (else
                    (unread-char d p)
                    n)))))
          (ekmd:error "unknown element"))))))


(define (ekmd:loc dir font)
  (let* ((sys (utsname:sysname (uname)))
         (ls (if (string-contains-ci sys "windows")
              (list (getenv "LOCALAPPDATA")
                    (getenv "CommonProgramFiles")
                    (getenv "CommonProgramFiles(x86)"))
             (if (string-contains-ci sys "linux")
              (list (getenv "XDG_DATA_HOME")
                    (getenv "XDG_DATA_DIRS"))
              (list "~/Library/Application Support"
                    "/Library/Application Support"))))
         (ls (map (lambda (l)
              (string-join
                (append! (string-split l #\\) (list "SMuFL/Fonts" font))
                "/"))
              ls)))
    (if (string-null? dir) ls (cons* dir ls))))

(define-public (ekmd:read dir font name mask tmpl)
  (let loc ((ls (ekmd:loc dir font)))
    (if (null? ls)
      #f
      (let ((l (string-append (car ls) "/" (string-downcase name) ".json")))
        (if (file-exists? l)
          (let* ((p (open-input-file l #:encoding "utf-8"))
                 (t (ekmd:value p mask tmpl)))
            (close-port p)
            t)
          (loc (cdr ls)))))))

(define-public (ekmd:tag! tag styles)
  (let tg ((g ekmd:glyphs))
    (if (null? g) #t
    (begin
      (if (eq? tag (cadar g))
        (let* ((e (car g))
               (n (symbol->string (car e)))
               (s (let st ((s styles))
                    (if (null? s) 'default
                    (if (string-contains-ci n (symbol->string (car s))) (car s)
                    (st (cdr s))))))
               (l (let lg ((l 10) (d 1024))
                    (if (< d 8) #f
                    (if (string-contains n (number->string d)) l
                    (lg (1- l) (/ d 2)))))))
          (list-set! e 1 s)
          (list-set! e 2 l)))
      (tg (cdr g))))))

(define-public (ekmd:name->cp gn)
  (let n->c ((g ekmd:glyphs))
    (if (null? g) #t
    (let ((c (assq-ref gn (caar g))))
      (if c (set-car! (car g) c))
      (n->c (cdr g))))))

(define-public (ekmd:load name)
  (let ((fn (ly:find-file (or name "glyphnames.scm"))))
    (if fn
      (let* ((p (open-input-file fn))
             (tab (read p)))
        (close-port p)
        (if name (begin
          (set! ekmd:dir (dirname fn))
          (set! ekmd:defaults (assq-ref tab 'defaults))
          (set! ekmd:glyphs (assq-ref tab 'glyphs))))
        tab)
      #f)))

(define-public (ekmd:save fn tab)
  (let* ((p (open-output-file fn)))
    (write tab p)
    (close-port p)))
