%%
%% Samples for Pivot chord symbols
%% spanned over 2 or more notes
%%

\version "2.24.0"

% ekmFont = "Bravura"

\include "esmufl.ily"

\pointAndClickOff


\markup \sans \column {
  "Samples for Pivot chord symbols"
  \vspace #0.5
  \line {
    The \typewriter "Text_spanner_engraver" must be included
    in the "Lyrics" context to draw pivot spans.
  }
  \vspace #2
}

%%----------------------------------------------------------------------

\layout {
  \context {
    \Lyrics
    \consists "Text_spanner_engraver"
  }
}

%%----------------------------------------------------------------------

\score {
<<
  \new ChordNames \chordmode {
    c2
    f
    g
    e:m
    a:m
    d
    g1
  }
  \new Staff
    \new Voice \fixed c' {
      <c e g>2
      <c f a>
      <b, d g>
      <b, e g>
      <c e a>
      <d fis a>
      <d g b>1
    }

  \new Staff
  <<
    \new Voice \fixed c {
      \clef bass
      c2
      f
      g
      e
      a
      d
      g,1
    }
    \new NullVoice = "funcaligner" \fixed c {
      c2
      f
      g4
      g
      e2
      a
      d
      g1
    }
    \new Lyrics \lyricsto "funcaligner" {
      \ekmFuncList #'(
        "C:|+"
        "I|"
        "IV|"
        "V__|"
        "|G:~"
        "iii|vi"
        "vi|ii."
        "|V"
        "|I"
      )
    }
  >>
>>
}

%%----------------------------------------------------------------------

\markup \sans \column {
  \vspace #2
  "J.S.Bach, BWV 851"
  \vspace #1
}

\score {
  \new GrandStaff
  <<
    \new Staff
    <<
      \new Voice \relative c' {
        \key d \minor
        \time 3/4
        \voiceOne
        f8 d'~ d16[ c bes a] c[ bes a g]
        a4
      }
      \new Voice \relative c' {
        \voiceTwo
        d4 r r
        r8 f
      }
    >>

    \new Staff
    <<
      \new Voice \relative c {
        \clef bass
        \key d \minor
        \time 3/4
        r8 d e[ f g e]
        f16 d cis d
      }
      \new NullVoice = "funcaligner" {
        d8 r e f g r
        f r
      }
      \new Lyrics \lyricsto "funcaligner" {
        \ekmFuncList #'(
          "d:i"
          "bVII^5^6|F:V^5^6~"
          "bIII|I"
          "bVII^5^6|V^5^6"
          "i|vi."
        )
      }
    >>
  >>
}
