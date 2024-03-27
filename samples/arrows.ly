%%
%% Sample for Esmuflily
%% Table of arrow symbols drawn with \ekm-arrow.
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
    Table of arrow symbols drawn with \typewriter "\\ekm-arrow" .
  }
  \vspace #2
}

%% ---------------------------------------------------------------------

ekmWName = #'(0 . 28)
ekmWG = 5

#(define-markup-command (ekm-group-all layout props name)
  (symbol?)
  (interpret-markup layout props
    (markup #:column (
      #:line (
        #:pad-to-box ekmWName '(0 . 4.5) #:typewriter (symbol->string name)
        #:hcenter-in ekmWG #:ekm-arrow name N
        #:hcenter-in ekmWG #:ekm-arrow name NE
        #:hcenter-in ekmWG #:ekm-arrow name E
        #:hcenter-in ekmWG #:ekm-arrow name SE
        #:hcenter-in ekmWG #:ekm-arrow name S
        #:hcenter-in ekmWG #:ekm-arrow name SW
        #:hcenter-in ekmWG #:ekm-arrow name W
        #:hcenter-in ekmWG #:ekm-arrow name NW
        #:hcenter-in ekmWG #:ekm-arrow name NS
        #:hcenter-in ekmWG #:ekm-arrow name NESW
        #:hcenter-in ekmWG #:ekm-arrow name EW
        #:hcenter-in ekmWG #:ekm-arrow name SENW
      )))))

#(define-markup-command (ekm-group-nodiagbil layout props name)
  (symbol?)
  (interpret-markup layout props
    (markup #:column (
      #:line (
        #:pad-to-box ekmWName '(0 . 4.5) #:typewriter (symbol->string name)
        #:hcenter-in ekmWG #:ekm-arrow name N
        #:hcenter-in ekmWG #:ekm-arrow name NE
        #:hcenter-in ekmWG #:ekm-arrow name E
        #:hcenter-in ekmWG #:ekm-arrow name SE
        #:hcenter-in ekmWG #:ekm-arrow name S
        #:hcenter-in ekmWG #:ekm-arrow name SW
        #:hcenter-in ekmWG #:ekm-arrow name W
        #:hcenter-in ekmWG #:ekm-arrow name NW
        #:hcenter-in ekmWG #:ekm-arrow name NS
        #:hcenter-in ekmWG ""
        #:hcenter-in ekmWG #:ekm-arrow name EW
        #:hcenter-in ekmWG ""
      )))))

#(define-markup-command (ekm-group-nobil layout props name)
  (symbol?)
  (interpret-markup layout props
    (markup #:column (
      #:line (
        #:pad-to-box ekmWName '(0 . 4.5) #:typewriter (symbol->string name)
        #:hcenter-in ekmWG #:ekm-arrow name N
        #:hcenter-in ekmWG #:ekm-arrow name NE
        #:hcenter-in ekmWG #:ekm-arrow name E
        #:hcenter-in ekmWG #:ekm-arrow name SE
        #:hcenter-in ekmWG #:ekm-arrow name S
        #:hcenter-in ekmWG #:ekm-arrow name SW
        #:hcenter-in ekmWG #:ekm-arrow name W
        #:hcenter-in ekmWG #:ekm-arrow name NW
      )))))

#(define-markup-command (ekm-group-nodiag layout props name)
  (symbol?)
  (interpret-markup layout props
    (markup #:column (
      #:line (
        #:pad-to-box ekmWName '(0 . 4.5) #:typewriter (symbol->string name)
        #:hcenter-in ekmWG #:ekm-arrow name N
        #:hcenter-in ekmWG ""
        #:hcenter-in ekmWG #:ekm-arrow name E
        #:hcenter-in ekmWG ""
        #:hcenter-in ekmWG #:ekm-arrow name S
        #:hcenter-in ekmWG ""
        #:hcenter-in ekmWG #:ekm-arrow name W
        #:hcenter-in ekmWG ""
      )))))


\markup { \column {
  \typewriter \line {
    \pad-to-box #ekmWName #'(0 . 0) " "
    \hcenter-in #ekmWG "N"
    \hcenter-in #ekmWG "NE"
    \hcenter-in #ekmWG "E"
    \hcenter-in #ekmWG "SE"
    \hcenter-in #ekmWG "S"
    \hcenter-in #ekmWG "SW"
    \hcenter-in #ekmWG "W"
    \hcenter-in #ekmWG "NW"
    \hcenter-in #ekmWG "NS"
    \hcenter-in #ekmWG "NESW"
    \hcenter-in #ekmWG "EW"
    \hcenter-in #ekmWG "SENW"
  }
  \vspace #1.0

  \ekm-group-nodiagbil #'black
  \ekm-group-nodiagbil #'white
  \ekm-group-nodiagbil #'open

  \ekm-group-all #'simple
  \ekm-group-nodiagbil #'double
  \ekm-group-nodiag #'triple
  \ekm-group-nodiag #'quadruple

  \ekm-group-nodiagbil #'black-wide
  \ekm-group-nodiagbil #'white-wide

  \ekm-group-nodiagbil #'triangle
  \ekm-group-nodiagbil #'triangle-bar
  \ekm-group-nodiag #'two-headed

  \ekm-group-nodiag #'dashed
  \ekm-group-nodiag #'triangle-dashed
  \ekm-group-nodiag #'opposite
  \ekm-group-nodiag #'triangle-opposite
  \ekm-group-nodiag #'paired
  \ekm-group-nodiag #'triangle-paired

  \vspace #1.0

  \ekm-group-nobil #'bent-tip
  \ekm-group-nobil #'long-bent-tip
  \ekm-group-nodiag #'curving

  \vspace #1.0

  \ekm-group-nobil #'black-head
  \ekm-group-nobil #'white-head
  \ekm-group-nobil #'open-head
  \ekm-group-nobil #'equilateral-head
  \ekm-group-nobil #'three-d-head
}}

\pageBreak

\markup { \column {
  \vspace #2.0

  \typewriter \line {
    \pad-to-box #ekmWName #'(0 . 0) " "
    \hcenter-in #ekmWG "N"
    \hcenter-in #ekmWG "NE"
    \hcenter-in #ekmWG "E"
    \hcenter-in #ekmWG "SE"
    \hcenter-in #ekmWG "S"
    \hcenter-in #ekmWG "SW"
    \hcenter-in #ekmWG "W"
    \hcenter-in #ekmWG "NW"
    \hcenter-in #ekmWG "NS"
    \hcenter-in #ekmWG "NESW"
    \hcenter-in #ekmWG "EW"
    \hcenter-in #ekmWG "SENW"
  }
  \vspace #1.0

  \ekm-group-nobil #'black-triangle
  \ekm-group-nobil #'white-triangle
  \ekm-group-nodiag #'black-small-triangle
  \ekm-group-nodiag #'white-small-triangle
  \ekm-group-nodiag #'half-circle
  \ekm-group-nodiag #'circle-half-black
  \ekm-group-nobil #'square-half-black
  \ekm-group-nodiag #'diamond-half-black
  \ekm-group-nodiag #'circle-quarters
}}
