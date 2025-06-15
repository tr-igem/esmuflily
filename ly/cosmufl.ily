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


#(define ekm:system #f)

#(let* ((s (ly:get-option 'ekmsystem))
        (s (if s (symbol->string s)
           (if (defined? 'ekmSystem) ekmSystem "24")))
        (s (if (number? s) (number->string s 10) s))
        (file (if (string=? "72" s)
                 "ekmel.ily"
                 (string-append "ekmel-" s ".ily"))))
  (set! ekm:system s)
  (if (ly:find-file file)
    (ly:parser-include-string (format #f "\\include \"~a\"\n" file))))

\include "esmufl.ily"


#(define-markup-command (ekm-system layout props)
  ()
  (interpret-markup layout props ekm:system))

#(define-markup-command (ekm-trill-accidental layout props alt)
  (rational?)
  #:properties ((trill-padding 0.2))
  (let* ((tr (interpret-markup layout props
               (make-ekm-char-markup #xE566)))
         (acc (interpret-markup layout props
                (make-fontsize-markup -1
                  (make-ekmelic-char-text-markup alt)))))
    (stack-stencil-line trill-padding (list tr acc point-stencil))))

ekmStartTrillSpanAccidental =
#(define-event-function (tempo alt)
  (number-or-pair? rational?)
  (make-music 'TrillSpanEvent
    'span-direction START
    'tweaks `((zigzag-width . ,tempo)
              (text . (,(make-ekm-trill-accidental-markup alt) . 0)))))
