%%
%% Sample for Esmuflily
%% Table of percussion beater symbols drawn with \ekm-beater.
%%

\version "2.24.0"

\include "esmufl.ily"

\pointAndClickOff

\markup \column {
  \line {
    Sample for
    \with-url #"https://github.com/tr-igem/esmuflily"
    \with-color #darkblue "Esmuflily"
  }
  \line {
    Table of percussion beater symbols drawn with \typewriter "\\ekm-beater" .
  }
  \vspace #2
}

%% ---------------------------------------------------------------------

ekmWName = #'(0 . 28)
ekmWG = 5

#(define-markup-command (ekm-group layout props name)
  (symbol?)
  (interpret-markup layout props
    (markup #:column (
      #:line (
        #:pad-to-box ekmWName '(0 . 5) #:typewriter (symbol->string name)
        #:hcenter-in ekmWG #:ekm-beater name N
        #:hcenter-in ekmWG #:ekm-beater name NE
        #:hcenter-in ekmWG #:ekm-beater name E
        #:hcenter-in ekmWG #:ekm-beater name SE
        #:hcenter-in ekmWG #:ekm-beater name S
        #:hcenter-in ekmWG #:ekm-beater name SW
        #:hcenter-in ekmWG #:ekm-beater name W
        #:hcenter-in ekmWG #:ekm-beater name NW
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
  }
  \vspace #1.0

  \ekm-group #'xyl-soft
  \ekm-group #'xyl-medium
  \ekm-group #'xyl-hard
  \ekm-group #'xyl-wood
  \ekm-group #'glsp-soft
  \ekm-group #'glsp-hard
  \ekm-group #'timpani-soft
  \ekm-group #'timpani-medium
  \ekm-group #'timpani-hard
  \ekm-group #'timpani-wood
  \ekm-group #'yarn-soft
  \ekm-group #'yarn-medium
  \ekm-group #'yarn-hard
  \ekm-group #'gum-soft
  \ekm-group #'gum-medium
  \ekm-group #'gum-hard
  \ekm-group #'bass-soft
  \ekm-group #'bass-medium
  \ekm-group #'bass-hard
  \ekm-group #'bass-metal
  \ekm-group #'bass-double
  \ekm-group #'hammer-plastic
  \ekm-group #'hammer-wood
  \ekm-group #'hammer-metal
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
  }
  \vspace #1.0

  \ekm-group #'stick
  \ekm-group #'stick-snare
  \ekm-group #'stick-jazz
  \ekm-group #'triangle
  \ekm-group #'triangle-plain
  \ekm-group #'wound-soft
  \ekm-group #'wound-hard
  \ekm-group #'hand
  \ekm-group #'hand-finger
  \ekm-group #'hand-fist
  \ekm-group #'hand-fingernail
  \ekm-group #'superball
  \ekm-group #'metal
  \ekm-group #'brass
  \ekm-group #'brushes
  \ekm-group #'mallet
}}
