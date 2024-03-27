%%
%% Sample for Esmuflily
%% Brace glyphs with increasing size (height)
%% using different fonts and sizing techniques.
%%

\version "2.24.0"

\include "esmufl.ily"

\pointAndClickOff

\markup \column {
  \line {
    Sample for
    \with-url #"http://www.ekmelic-music.org/en/extra/esmuflily.htm"
    \with-color #darkblue "Esmuflily"
  }
  \line {
    Brace glyphs with increasing size (height)
    using different fonts and sizing techniques.
  }
  \vspace #2
}

%% ---------------------------------------------------------------------

#(define ekm-tick-list '(
  (rmoveto -0.2 0.98)
  (rlineto 0.2 0)))

#(define-markup-command (ekm-braces layout props pad from to style)
  (number? integer? integer? string?)
  #:properties ((font-size 0))
  (let ((ruler
          (interpret-markup layout props
            (make-path-markup 0.1
              (cons*
                '(moveto 0 0)
                '(rlineto 0.2 0)
                (let ldup ((n (quotient (1+ to) 2)) (l '()))
                  (if (= n 0) l (ldup (- n 1) (append ekm-tick-list l)))))))))
    (stack-stencil-line
      pad
      (map
        (lambda (size)
          (ly:stencil-stack
            (case (string->symbol style)
              ;; fetaBraces glyphs, each for a specific size (0 - 575)
              ((feta)
                (interpret-markup layout props
                  (markup
                    #:general-align Y DOWN
                    #:left-brace (* size 2.45))))
              ;; Ekmelos size variants, each for a range of sizes
              ((variant)
                (interpret-markup layout props
                  (markup
                    #:ekm-brace size LEFT)))
              ;; glyph U+E000 of font `style` for all sizes
              (else
                (let* ((sz (* size 125))
                       (sz (magnification->font-size (/ sz 1000))))
                  (interpret-markup layout
                    (cons
                      `((font-size . ,(+ font-size 5 sz))
                        (font-name . ,style))
                      props)
                    (ly:wide-char->utf-8 #xE000)))))
            X RIGHT
            ruler
            pad))
        (iota (1+ (- to from)) from 1)))))


\markup { \column {
  \line { 1 - 44 staff units: }

  \vspace #1.5
  \line { fetaBraces glyphs drawn with \typewriter "\\left-brace" : }
  \vspace #0.5
  \ekm-braces #0.6 #1 #44 "feta"

  \vspace #2
  \line { Ekmelos glyphs drawn with \typewriter "\\ekm-brace" : }
  \vspace #0.5
  \ekm-braces #0.6 #1 #44 "variant"

  \vspace #2
  \line { Ekmelos glyph U+E000 for all sizes: }
  \vspace #0.5
  \ekm-braces #0.3 #1 #44 "Ekmelos"

  \vspace #2
  \line { Bravura glyph U+E000 for all sizes: }
  \vspace #0.5
  \ekm-braces #0.6 #1 #44 "Bravura"
}}

\pageBreak

\markup { \column {
  \vspace #1
  \line { 45 - 64 staff units: }

  \vspace #1.5
  \ekm-braces #0.6 #45 #64 "feta"

  \vspace #1.5
  \ekm-braces #0.6 #45 #64 "variant"

  \vspace #1.5
  \ekm-braces #0.2 #45 #64 "Ekmelos"

  \vspace #1.5
  \ekm-braces #0.6 #45 #64 "Bravura"
}}
