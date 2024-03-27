%%
%% Collection of commands making use of Esmuflily.
%%
%% Thomas Richter <thomas-richter AT aon.at>
%% This program is free software. Use, redistribute, and modify it
%% as you wish.
%%
%%
%% Most of the names are derived from SMuFL glyph names.
%% Several scripts have no corresponding script name in LilyPond.
%% So an alternative name is specified with \ekmScript.
%%
%% [E] = draws an Ekmelos specific glyph
%%

\version "2.24.0"


%% Expressive marks

accentlarge = \ekmScript #'accent #'((#xE4A0 1) . (#xE4A1 1)) % U+F42A . U+F42B
accentrossini = \ekmScript #'accent #'((#xE4A0 2) . (#xE4A1 2)) % U+F532 . U+F533
accentstaccato = \ekmScript #'accent #'(#xE4B0 . #xE4B1)
doubleaccent = \ekmScript #'accent #'((#xE4A0 #xE4A0) . (#xE4A1 #xE4A1)) % [E] U+F694 . U+F695
tripleaccent = \ekmScript #'accent #'((#xE4A0 #xE4A0 #xE4A0) . (#xE4A1 #xE4A1 #xE4A1)) % [E] U+F696 . U+F697

tenutoaccent = \ekmScript #'tenuto #'(#xE4B4 . #xE4B5)
tenutodoubleaccent = \ekmScript #'tenuto #'((#xE4A4 #xE4A0 #xE4A0) . (#xE4A5 #xE4A1 #xE4A1)) % [E] U+F698 . U+F699
tenutomarcato = \ekmScript #'tenuto #'((#xE4A4 #xE4AC) . (#xE4A5 #xE4AD)) % [E] U+F68C . U+F68D

staccatissimowedge = \ekmScript #'staccatissimo #'(#xE4A8 . #xE4A9)
staccatissimostroke = \ekmScript #'staccatissimo #'(#xE4AA . #xE4AB)
staccatissimocolon = \ekmScript #'staccatissimo #'(#xF68E . #xF68F) % [E]

marcatostaccato = \ekmScript #'marcato #'(#xE4AE . #xE4AF)
marcatostaccatissimo = \ekmScript #'marcato #'((#xE4AC #xE4A6) . (#xE4AD #xE4A7)) % [E] U+F690 . U+F691
marcatotenuto = \ekmScript #'marcato #'(#xE4BC . #xE4BD)
marcatotenutostaccato = \ekmScript #'portato #'((#xE4AC #xE4A4 #xE4A2) . (#xE4AD #xE4A5 #xE4A3)) % [E] U+F692 . U+F693

doublemarcato = \ekmScript #'marcato #'((#xE4AC #xE4AC) . (#xE4AD #xE4AD)) % [E] U+F69A . U+F69B
triplemarcato = \ekmScript #'marcato #'((#xE4AC #xE4AC #xE4AC) . (#xE4AD #xE4AD #xE4AD)) % [E] U+F69C . U+F69D

stress = \ekmScript #'accent #'(#xE4B6 . #xE4B7)
unstress = \ekmScript #'accent #'(#xE4B8 . #xE4B9)

espressivostaccato = \ekmScript #'espressivo #'(#xED42 . #xED43)
espressivotenuto = \ekmScript #'espressivo #'(#xED44 . #xED45)
espressivotenutostaccato = \ekmScript #'espressivo #'(#xED46 . #xED47)


%% Ornaments

trillflat = \ekmScript #'trill #'((#xE260 #xE566)) % U+F5BD
trillnatural = \ekmScript #'trill #'((#xE261 #xE566)) % U+F5BE
trillsharp = \ekmScript #'trill #'((#xE262 #xE566)) % U+F5BF
trillsimple = \ekmScript #'trill #'((#xE566 1)) % [E] U+F6B9

turnup = \ekmScript #'turn ##xE56A
turnups = \ekmScript #'turn ##xE56B
turnflat = \ekmScript #'turn #'((#xE260 #xE567)) % U+F5C0
turnflatsharplower = \ekmScript #'turn #'((#xE260 #xE567 #xE262)) % U+F5C1
turnflatlower = \ekmScript #'turn #'((#xE567 #xE260)) % U+F5C2
turnnatural = \ekmScript #'turn #'((#xE261 #xE567)) % U+F5C3
turnnaturallower = \ekmScript #'turn #'((#xE567 #xE261)) % U+F5C4
turnsharp = \ekmScript #'turn #'((#xE262 #xE567)) % U+F5C5
turnsharpflatlower = \ekmScript #'turn #'((#xE262 #xE567 #xE260)) % U+F5C6
turnsharplower = \ekmScript #'turn #'((#xE567 #xE262)) % U+F5C7


%% Performance indications

downbowturned = \ekmScript #'downbow ##xE611
overpressuredownbow = \ekmScript #'downbow ##xE61B
overpressurepossibiledownbow = \ekmScript #'downbow ##xE61D
downbowtowardsbody = \ekmScript #'downbow ##xEE80
downbowawayfrombody = \ekmScript #'downbow ##xEE82
downbowbeyondbridge = \ekmScript #'downbow ##xEE84

upbowturned = \ekmScript #'upbow ##xE613
overpressureupbow = \ekmScript #'upbow ##xE61C
overpressurepossibileupbow = \ekmScript #'upbow ##xE61E
upbowtowardsbody = \ekmScript #'upbow ##xEE81
upbowawayfrombody = \ekmScript #'upbow ##xEE83
upbowbeyondbridge = \ekmScript #'upbow ##xEE85

overpressurenodirection = \ekmScript #'upbow ##xE61F

changebowdirection = \ekmScript #'downbow ##xE626
changebowdirectionliga = \ekmScript #'downbow #'((#xE626 1)) % U+F431
changebowdirectionimposed = \ekmScript #'downbow #'((#xE626 2)) % U+F43E

halfflageolet = \ekmScript #'flageolet ##xE615
halfopenvertical = #(make-articulation 'halfopenvertical)

jete = \ekmScript #'downbow #'(#xE620 . #xE621)


snappizzicatogerman = \ekmScript #'snappizzicato #'((#xE631 1) . (#xE630 1)) % U+F433 . U+F432
buzzpizzicato = \ekmScript #'snappizzicato ##xE632
stoppedharmonic = \ekmScript #'stopped #'(#xF6AD . #xF6AE) % [E]


openpedal = \ekmScript #'open ##xE83D
halfopenpedal = \ekmScript #'halfopen ##xE83E
closepedal = \ekmScript #'stopped ##xE83F


doubletongue = \ekmScript #'downbow #'(#xE5F0 . #xE5F1)
doubletonguenoslur = \ekmScript #'downbow #'((#xE5F0 1) . (#xE5F1 1)) % U+F42D . U+F42E
tripletongue = \ekmScript #'downbow #'(#xE5F2 . #xE5F3)
tripletonguenoslur = \ekmScript #'downbow #'((#xE5F2 1) . (#xE5F3 1)) % U+F42F . U+F430

openhole = \ekmScript #'open ##xE5F9
halfopenhole = \ekmScript #'halfopen ##xE5F7
halfopenholevertical = \ekmScript #'halfopenvertical ##xE5F6
halfopenholedot = \ekmScript #'halfopen ##xE5F8
quarteropenhole = \ekmScript #'halfopen ##xE5F5
closedhole = \ekmScript #'stopped ##xE5F4


openmute = \ekmScript #'open ##xE5E7
halfopenmute = \ekmScript #'halfopen ##xE5E6
closedmute = \ekmScript #'stopped ##xE5E5


heel = \ekmScript #'lheel ##xE663
lheelold = \ekmScript #'lheel #'((#xE661 1)) % [E] U+F6B1
rheelold = \ekmScript #'rheel #'((#xE662 1)) % [E] U+F6B2
heeltoe = \ekmScript #'lheel ##xE666
heeltotoe = \ekmScript #'rtoe ##xE674
toetoheel = \ekmScript #'lheel ##xE675

bebungtwo = \ekmScript #'downbow #'(#xE668 . #xE669)
bebungthree = \ekmScript #'downbow #'(#xE66A . #xE66B)
bebungfour = \ekmScript #'downbow #'(#xE66C . #xE66D)


extrashortfermata = \ekmScript #'veryshortfermata #'(#xF69E . #xF69F) % [E]
extralongfermata = \ekmScript #'verylongfermata #'(#xF6A0 . #xF6A1) % [E]

comma = #(make-articulation "comma")
commawedge = #(make-articulation "varcomma")

breathe = \ekmBreathing ##xE4CE
breathewedge = \ekmBreathing ##xF640 % [E]
breathetick = \ekmBreathing ##xE4CF
breatheupbow = \ekmBreathing ##xE4D0
breathesalzedo = \ekmBreathing ##xE4D5

caesura = \ekmBreathing ##xE4D1
caesurathick = \ekmBreathing ##xE4D2
caesurashort = \ekmBreathing ##xE4D3
caesuracurved = \ekmBreathing ##xE4D4
caesurasingle = \ekmBreathing #'(#xE4D1 1) % U+F42C


segnojapanese = \ekmScript #'segno #'((#xE047 1)) % U+F404
codajapanese = \ekmScript #'coda  #'((#xE048 1)) % U+F405

%% these commands set begin-of-line-invisible
DS = {
  \once \override Score.RehearsalMark.break-visibility = #'#(#t #t #f)
  \mark \markup { \small \ekm-char ##xE045 }
}
DScoda = {
  \once \override Score.RehearsalMark.break-visibility = #'#(#t #t #f)
  \mark \markup { \small \ekm-line #'(#xE045 "al coda") }
}
DSfine = {
  \once \override Score.RehearsalMark.break-visibility = #'#(#t #t #f)
  \mark \markup { \small \ekm-line #'(#xE045 "al fine") }
}
DC = {
  \once \override Score.RehearsalMark.break-visibility = #'#(#t #t #f)
  \mark \markup { \small \ekm-char ##xE046 }
}
DCcoda = {
  \once \override Score.RehearsalMark.break-visibility = #'#(#t #t #f)
  \mark \markup { \small \ekm-line #'(#xE046 "al coda") }
}
DCfine = {
  \once \override Score.RehearsalMark.break-visibility = #'#(#t #t #f)
  \mark \markup { \small \ekm-line #'(#xE046 "al fine") }
}


%% Absolute dynamic marks

pf = #(make-dynamic-script "pf")
fz = #(make-dynamic-script "fz")
sfpp = #(make-dynamic-script "sfpp")
sfzp = #(make-dynamic-script "sfzp")
sffz = #(make-dynamic-script "sffz")
sfffz = #(make-dynamic-script "sfffz")
sffffz = #(make-dynamic-script "sffffz")
rf = #(make-dynamic-script "rf")
