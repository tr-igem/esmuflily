%% This file is part of Esmuflily - Support for SMuFL/Ekmelos.
%% Copyright (C) 2025-2026  Thomas Richter <thomas-richter@aon.at>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%
%%
%% File: cosmufl.ily  -  Include file combining Esmuflily and Ekmelily
%%
%%

\version "2.24.0"

#(define ekm:tuning #f)
#(define ekm:file #f)
#(define ekm:language-name #f)
#(define ekm:style #f)
#(define ekm:switches '())

#(define (ekm:get-use fn c ls)
  (if (or (zero? c) (null? ls) (eq? '- (car ls)) (eq? '+ (car ls)))
    ls
    (let* ((e (car ls))
           (e (if (number? e) (number->string e 10)
              (if (symbol? e) (symbol->string e) e)))
           (r (if (string-null? e) 0 (fn (string-downcase e))))) ; 0:skip, 1:next, #f:end
      (ekm:get-use fn (if r (- c r) 0) (if r (cdr ls) ls)))))

#(let* ((ls (or (ly:get-option 'ekmuse)
                (and (defined? 'ekmUse) ekmUse)
                (and (defined? 'ekmSystem) ekmSystem)
                ""))
        (ls (if (string? ls) (string-split ls char-set:whitespace)
            (if (list? ls) ls (list ls)))))
  (set! ekm:switches
    (ekm:get-use (lambda (e)
      (cond
        ((or (char-numeric? (string-ref e 0))
             (equal? "arabic" e))
          (set! ekm:tuning e) 1)
        (else #f)))
      1 ls))
  (set! ekm:tuning (or ekm:tuning "24"))
  (set! ekm:file
    (if (equal? "72" ekm:tuning)
      "ekmel.ily"
      (string-append "ekmel-" ekm:tuning ".ily")))
  (if (ly:find-file ekm:file)
    (ly:parser-include-string (format #f "\\include \"~a\"\n" ekm:file))
    (ly:error "Tuning '~a' does not exist" ekm:tuning)))

#(let* ((sw (ekm:get-use (lambda (e)
          (cond
            ((and (not ekm:language-name) (assq (string->symbol e) ekmLanguages))
              (set! ekm:language-name e) 1)
            ((and (not ekm:style) (assq (string->symbol e) ekmNotations))
              (set! ekm:style e) 1)
            (else #f)))
          2 ekm:switches))
        (sw (filter-map (lambda (e)
          (if (string? e) (if (string-null? e) #f (string->symbol e)) e)) sw)))
  (set! ekm:switches
    (if (null? sw) '(-)
    (if (eq? '+ (car sw)) (cdr sw)
    sw)))
  (if ekm:language-name
    (ekm:set-language ekm:language-name)
    (set! ekm:language-name (symbol->string (caar ekmLanguages))))
  (if ekm:style
    (ekm:set-notation ekm:style))
  (set! ekm:style ekm:notation-name))

\include "esmufl.ily"

\layout {
  \context {
    \Score
    \ekmSmuflOn #ekm:switches
  }
}


#(define-markup-command (ekm-tuning layout props)
  ()
  (interpret-markup layout props ekm:tuning))

#(define-markup-command (ekm-combine-accidental layout props alt alt2 arg)
  (ekm:genalter? ekm:genalter? markup?)
  #:properties ((x-padding 0.2)
                (y-padding 0.1)
                (acc-size -1))
  (let ((sil (interpret-markup layout props arg)))
    (if (eq? 'X alt2)
      (stack-stencil-line x-padding (list
        sil
        (interpret-markup layout props
          (make-fontsize-markup acc-size
            (make-ekmelic-char-text-markup alt)))
        point-stencil))
      (let* ((w (ekm-extent sil X))
             (acc (map (lambda (a)
                    (if (eq? 'none a)
                      empty-stencil
                      (interpret-markup layout props
                        (make-hcenter-in-markup w
                          (make-fontsize-markup acc-size
                            (make-ekmelic-char-markup a))))))
                    (list alt alt2))))
        (ly:stencil-combine-at-edge
        (ly:stencil-combine-at-edge
          (ly:stencil-aligned-to sil X CENTER)
          Y UP (first acc) y-padding)
          Y DOWN (second acc) y-padding)))))

ekmScriptAccidental =
#(define-music-function (name alt alt2)
  (symbol? ekm:genalter? ekm:genalter?)
  (make-articulation name
    'tweaks
    `((details . (,(make-ekm-combine-accidental-markup
                    alt alt2 (make-ekm-script-markup (symbol->string name) MAIN)))))))

ekmStartTrillSpanAccidental =
#(define-event-function (tempo alt)
  (number-or-pair? ekm:genalter?)
  (make-music 'TrillSpanEvent
    'span-direction START
    'tweaks `((zigzag-width . ,tempo)
              (text . (,(make-halign-markup LEFT
                        (make-ekm-combine-accidental-markup
                          alt 'none (make-ekm-script-markup 'trill MAIN))) . 0)))))
