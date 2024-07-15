%%
%% Sample for Esmuflily
%% System start delimiters brace and bracket
%% using the "Ekmelos" or "Bravura" font.
%%

\version "2.24.0"

% ekmFont = "Bravura"

\include "esmufl.ily"

\pointAndClickOff

\paper {
  indent = 1.0 \cm
  short-indent = 1.0 \cm
  ragged-right = ##t
  ragged-bottom = ##t
}


\markup \column {
  \line {
    Sample for
    \with-url #"http://www.ekmelic-music.org/en/extra/esmuflily.htm"
    \with-color #darkblue "Esmuflily"
  }
  \line {
    System start delimiters brace and bracket.
  }
  \line { Font: \typewriter { #ekm:font-name }}
  \line { Include file: \typewriter { esmufl.ily }}
  \vspace #2
}

%% ---------------------------------------------------------------------

#(define (is-bravura l p) (string=? "Bravura" ekm:font-name))
#(define sp " ")
#(define mark "      |")

myinit = {
  \ekmSmuflOn #'systemstart
  \override SystemStartBrace.collapse-height = #0
  \override SystemStartBracket.collapse-height = #0
  % \override SystemStartSquare.collapse-height = #0
  \override SystemStartBar.collapse-height = #0
}

%% ---------------------------------------------------------------------

\markup { \column {
  \line { Brace }
  \vspace #1
}}

\score {
  \new GrandStaff
  <<
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\score {
  \new GrandStaff
  <<
    \new Staff { c''1 }
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\score {
  \new GrandStaff
  <<
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\score {
  \new GrandStaff
  <<
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\pageBreak

\score {
  \new GrandStaff
  <<
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\score {
  \new GrandStaff
  <<
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\pageBreak

\score {
  \new GrandStaff
  <<
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\pageBreak

\markup { \column {
  \vspace #3

  \if #is-bravura
  \fill-line {
    \center-column { \ekm-charf ##xE000 #0  "brace"       "" }
    \center-column { \ekm-charf ##xE000 #1  "braceSmall"  "salt01" }
    \center-column { \ekm-charf ##xE000 #2  "braceLarge"  "salt02" }
    \center-column { \ekm-charf ##xE000 #3  "braceLarger" "salt03" }
    \center-column { \ekm-charf ##xE000 #4  "braceFlat"   "salt04" }
  }
  \if #is-bravura
  \vspace #3

  \fill-line {
    \ekm-system-start #'brace #4
    \ekm-system-start #'brace #5
    \ekm-system-start #'brace #6
    \ekm-system-start #'brace #7
    \ekm-system-start #'brace #8
    \ekm-system-start #'brace #9
    \ekm-system-start #'brace #10
    \ekm-system-start #'brace #11
    \ekm-system-start #'brace #12
    \ekm-system-start #'brace #13
    \ekm-system-start #'brace #14
    \ekm-system-start #'brace #15
    \ekm-system-start #'brace #16
    \ekm-system-start #'brace #17
    \ekm-system-start #'brace #18
    \ekm-system-start #'brace #19
    \ekm-system-start #'brace #20
    \ekm-system-start #'brace #21
    \ekm-system-start #'brace #22
    \ekm-system-start #'brace #23
    \ekm-system-start #'brace #24
    \ekm-system-start #'brace #25
    \ekm-system-start #'brace #26
    \ekm-system-start #'brace #27
    \ekm-system-start #'brace #28
  }
  \fill-line {
    "4"  "5"  "6"  "7"  "8" "9"  "10" "11" "12" "13" "14" "15"
    "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28"
  }
  \if #is-bravura
  \fill-line {
    #sp #sp #sp #sp #mark
    #sp #sp #sp #sp #sp #sp #sp #sp #mark
    #sp #sp #sp #sp #sp #sp #sp #sp #mark
    #sp #sp
  }
  \unless #is-bravura
  \fill-line {
    #sp #mark
    #sp #sp #sp #sp #sp #mark
    #sp #sp #sp #sp #sp #sp #sp #mark
    #sp #sp #sp #sp #sp #sp #sp #mark
    #sp
  }
  \vspace #3

  \fill-line {
    \ekm-system-start #'brace #29
    \ekm-system-start #'brace #30
    \ekm-system-start #'brace #31
    \ekm-system-start #'brace #32
    \ekm-system-start #'brace #33
    \ekm-system-start #'brace #34
    \ekm-system-start #'brace #35
    \ekm-system-start #'brace #36
    \ekm-system-start #'brace #37
    \ekm-system-start #'brace #38
    \ekm-system-start #'brace #39
    \ekm-system-start #'brace #40
    \ekm-system-start #'brace #41
    \ekm-system-start #'brace #42
    \ekm-system-start #'brace #43
    \ekm-system-start #'brace #44
    \ekm-system-start #'brace #45
    \ekm-system-start #'brace #46
    \ekm-system-start #'brace #47
    \ekm-system-start #'brace #48
    \ekm-system-start #'brace #49
    \ekm-system-start #'brace #50
  }
  \fill-line {
    "29" "30" "31" "32" "33" "34" "35" "36" "37" "38" "39"
    "40" "41" "42" "43" "44" "45" "46" "47" "48" "49" "50"
  }
  \if #is-bravura
  \fill-line {
    #sp #sp #sp #sp #sp #sp #mark
    #sp #sp #sp #sp #sp #sp #sp #sp #mark
    #sp #sp #sp #sp #mark
    #sp
  }
  \unless #is-bravura
  \fill-line {
    #sp #sp #sp #sp #sp #sp #mark
    #sp #sp #sp #sp #sp #sp #sp #mark
    #sp #sp #sp #sp #sp #sp #sp
  }
}}

\pageBreak

\markup { \column {
  \vspace #3

  \fill-line {
    \ekm-system-start #'brace #52
    \ekm-system-start #'brace #60
    \ekm-system-start #'brace #68
    \ekm-system-start #'brace #76
    \ekm-system-start #'brace #84
    \ekm-system-start #'brace #97
    \ekm-system-start #'brace #140
  }
  \fill-line {
    "52" "60" "68" "76" "84" "97" "140"
  }
}}

\pageBreak

%% ---------------------------------------------------------------------

\markup { \column {
  \line { Bracket }
  \vspace #1
}}

\score {
  \new StaffGroup
  <<
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\score {
  \new StaffGroup
  <<
    \new Staff { c''1 }
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\score {
  \new StaffGroup
  <<
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\score {
  \new StaffGroup
  <<
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\pageBreak

%% ---------------------------------------------------------------------

\markup { \column {
  \line { Nested brace and bracket }
  \vspace #1
}}

\score {
  \new StaffGroup
  <<
    \set StaffGroup.systemStartDelimiterHierarchy =
      #'(SystemStartSquare
          (SystemStartBrace
            (SystemStartBracket a
              (SystemStartSquare b)) c) d)

    \override StaffGroup.SystemStartSquare.collapse-height = #4

    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
    \new Staff { c''1 }
  >>
  \layout {
    \context {
      \Score
      \myinit
    }
  }
}

\pageBreak
