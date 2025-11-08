%% This file is part of Esmuflily - Support for SMuFL/Ekmelos.
%% Copyright (C) 2025  Thomas Richter <thomas-richter@aon.at>
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
#(define ekm:style #f)
#(define ekm:language-name #f)

#(let* ((s (or (ly:get-option 'ekmuse)
               (and (defined? 'ekmUse) ekmUse)
               (and (defined? 'ekmSystem) ekmSystem)
               ""))
        (s (if (symbol? s) (symbol->string s)
           (if (number? s) (number->string s 10) s)))
        (s (string-split s #\-))
        (t (if (string-null? (car s)) "24" (car s)))
        (f (if (string=? "72" t)
            "ekmel.ily"
            (string-append "ekmel-" t ".ily"))))
  (set! ekm:tuning t)
  (set! ekm:file f)
  (set! ekm:style
    (if (or (null? (cdr s)) (string-null? (second s))) #f (second s)))
  (set! ekm:language-name
    (if (or (> 3 (length s)) (string-null? (third s))) #f (third s)))
  (if (ly:find-file f)
    (ly:parser-include-string (format #f "\\include \"~a\"\n" f))
    (ly:error "Tuning '~a' does not exist" t)))

\include "esmufl.ily"

#(if (and ekm:style
          (assq (string->symbol ekm:style) ekmNotations))
  (ekm:set-notation ekm:style))

#(if (and ekm:language-name
          (assq (string->symbol ekm:language-name) ekmLanguages))
  (ekm:set-language ekm:language-name)
  (set! ekm:language-name (symbol->string (caar ekmLanguages))))


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
              (text . (,(make-ekm-combine-accidental-markup
                          alt 'none (make-ekm-script-markup 'trill MAIN)) . 0)))))
