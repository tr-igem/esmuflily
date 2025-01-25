%%
%% Sample for Esmuflily + Ekmelily
%% Trill spanner with accidental
%% drawn with "\ekmStartTrillSpanScript" from Esmuflily
%% and "\ekmelic-char-text" from Ekmelily.
%%

\version "2.24.0"

% ekmFont = "Bravura"

\include "esmufl.ily"
\include "ekmel.ily"

\ekmelicStyle sims

\pointAndClickOff


\markup \column {
  \line {
    Sample for
    \with-url #"https://github.com/tr-igem/esmuflily"
    \with-color #darkblue "Esmuflily"
    +
    \with-url #"https://github.com/tr-igem/ekmelily"
    \with-color #darkblue "Ekmelily"
  }
  \line {
    Trill spanner with accidental
    drawn with \typewriter "\\ekmStartTrillSpanScript" from Esmuflily
  }
  \line{
    and \typewriter "\\ekmelic-char-text" from Ekmelily.
  }
  \line { Font: \typewriter { \ekmelic-font-name }}
  \line { Include files: \typewriter { esmufl.ily } , \typewriter { ekmel.ily }}
  \line { Notation style: \typewriter { \ekmelic-style-name }}
  \vspace #2
}

%%----------------------------------------------------------------------

#(define-markup-command (ekm-trill-accidental layout props alt)
  (rational?)
  #:properties ((trill-padding 0.2))
  (let* ((tr (interpret-markup layout props
               (make-ekm-char-markup #xE566)))
         (acc (interpret-markup layout props
                (make-fontsize-markup -1
                  (make-ekmelic-char-text-markup alt)))))
    (stack-stencil-line trill-padding (list tr acc point-stencil))))

%%----------------------------------------------------------------------

\score {
  \relative c'' {
    \ekmSmuflOn #'trill

    c1 \ekmStartTrillSpanScript
         #2
         \markup \ekm-trill-accidental #2/3
    c2. cisil4 \stopTrillSpan
    R1

    c1 \ekmStartTrillSpanScript
         #-4
         \markup \ekm-trill-accidental #-3/4
    c c ceseh \stopTrillSpan
  }
}
