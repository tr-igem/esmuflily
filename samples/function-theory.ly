%%
%% Sample for Esmuflily
%% Function theory symbols drawn with "\ekmFuncList"
%% in a Lyrics context to synchronise the symbols to music
%% and to ensure a consistent vertical alignment.
%% The Lyrics context requires the "Text_spanner_engraver"
%% and is aligned to a NullVoice context.
%%
%% Adaption of LSR <http://lsr.di.unimi.it/LSR/Item?id=967>
%% by Klaus Blum.
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
  \wordwrap {
    Function theory symbols drawn with \typewriter "\\ekmFuncList"
    in a "Lyrics" context to synchronise the symbols to music
    and to ensure a consistent vertical alignment.
    The "Lyrics" context requires the \typewriter "Text_spanner_engraver"
    and is aligned to a "NullVoice" context.
  }
  \vspace #0.6
  \line {
    The sample is taken from
    \with-url #"http://lsr.di.unimi.it/LSR/Item?id=967"
    \with-color #darkblue "lsr.di.unimi.it/LSR/Item?id=967"
    and adapted for Esmuflily.
  }
  \vspace #2
}

%%----------------------------------------------------------------------

funcSoprano = \relative c'' {
  e4 e e( d)
  c4 d d2
  d4 e8 d c4 c
  d8( c) <b g>4 c2
}

funcAltTenor = \relative c'' {
  <c g>4 <bes g> <a f>2
  <a d,>4 <c a> <c a>( <b g>)
  <b e,>2 <g e>4 <a f>
  <a d,>4 d,8( f) <g e>2
}

funcBass = \relative c {
  \clef bass
  c4 cis d2
  f4 fis g2
  gis2 bes4 a8 g
  fis4 g c,2
}

funcAligner = \relative c {
  c4 cis d d
  f4 fis g g
  gis4 gis8 gis bes4 a8 g
  fis8 fis g g c,2
}

funcSymbols = \lyricmode {
  \set stanza = #"C major:"
  \ekmFuncList #'(
    "T,,3"  " (*"  "/D,3^7^9>"  ")*"  "Sp^9-"  "^8."
    "S^5^6"  "(D,3^7)"  "D^2^4-"  "^1^3."
    "(D,3^7-"  "^8"  "^7."  "_) [Tp] +"  "(D,7)"  "S,3-"  " ,2."
    "DD,3^8-"  "^7."  "D^5-"  "^7."  "T"
  )
}

\score {
  \new GrandStaff
  <<
    \new Staff
    \new Voice \partCombine \funcSoprano \funcAltTenor

    \new Staff
    <<
      \new Voice \funcBass
      \new NullVoice = "funcaligner" \funcAligner
      \new Lyrics \lyricsto "funcaligner" \funcSymbols
    >>
  >>
  \layout {
    \context {
      \Lyrics
      \consists "Text_spanner_engraver"
      \override StanzaNumber.font-series = #'medium
    }
  }
}
