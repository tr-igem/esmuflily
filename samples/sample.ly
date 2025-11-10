%%
%% Sample for Esmuflily + Ekmelily
%% Adaption of sample from LilySMuFL
%% <https://github.com/backseatviolist/lilysmufl>
%% by Nathan Ho, Joram Berger.
%%
%% Changes:
%% * "\niente" is replaced with "\n".
%% * "\smufllig" is replaced with "\ekm-chars".
%%

\version "2.24.0"

% ekmFont = "Bravura"
% ekmFont = "Sebastian"
% ekmFont = "Petaluma"

\include "cosmufl.ily"

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
    Adaption of sample from
    \with-url #"https://github.com/backseatviolist/lilysmufl"
    \with-color #darkblue "LilySMuFL"
    by Nathan Ho, Joram Berger.
  }
  \line { Changes: }
  \line { * \typewriter "\\niente" is replaced with \typewriter "\\n" . }
  \line { * \typewriter "\\smufllig" is replaced with \typewriter "\\ekm-chars" . }
  \vspace #1
  \line { Font: \typewriter { \ekmelic-font-name }}
  \line { Tuning: \typewriter { \ekm-tuning }}
  \line { Include file: \typewriter { cosmufl.ily }}
  \line { Notation style: \typewriter { \ekmelic-style-name }}
  \vspace #2
}

%%----------------------------------------------------------------------

music = \relative c' {
  \clef alto
  \time 3/4
  c4-.(\f\< d4-. es4-.) |
  \time 4/4
  fis8.---\trill\sfz\> e!16\downbow d16->\n r16 r8 c2-\prall |
  \time 2/2
  \clef treble
  r2-\fermata c8( eeh8)-^ \tuplet 3/2 { eeh8( gisih8 b')-! } |
  c,,4. c8 c4.. c16 |
}

<<
  \new Staff \music
>>

\layout {
  \context {
    \Score
    \ekmSmuflOn #'all
  }
}

\markup \column {
  \vspace #2
  \line {
    Some ligatures and combined glyphs with \typewriter "\\ekm-chars" :
  }
  \vspace #0.5
  \ekm-chars #'(
    #xE262
    #xE2B4
    #xE2B2
  )
  \vspace #0.5
  \ekm-chars #'(
    #xE262
    #xE566
    #xEAA0
    #xEAA1
    #xEAA2
    #xEAA3
    #xEAA4
    #xEAA5
    #xEAA6
    #xEAA7
    #xEAA8)
  \vspace #0.5
  \ekm-chars #'(
    #xEAC4
    #xEAC5
    #xEAC6
    #xEAC7
    #xEAC8
    #xEAC9
    #xEACA
    #xEACB)
  \vspace #0.5
  \ekm-chars #'(
    #xE680
    #xE682
    #xE681
    #xE683
    #xE682
    #xE682
    #xE681
    #xE680)
}
