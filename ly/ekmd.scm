;; This file is part of Esmuflily - Support for SMuFL/Ekmelos
;; Copyright (c) 2025 Thomas Richter
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;
;; File: ekmd.scm
;;
;; Parse metadata JSON file of a SMuFL-compliant font
;; and create Scheme metadata table for use in Esmuflily.
;;

(use-modules
  ((ice-9 rdelim) #:select (read-delimited)))

(define-public ekmd:dir #f)
(define-public ekmd:defaults '())
(define-public ekmd:glyphs '())

(define ekmd:dig (string->char-set "-0123456789"))
(define ekmd:num (string->char-set "0123456789.-+Ee"))

(define (ekmd:error t)
  (ly:error "JSON error: ~a" t)
  '())

(define (ekmd:next p)
  (let ((c (read-char p)))
    (case c
      ((#\sp #\ht #\lf #\cr) (ekmd:next p))
      (else c))))

(define (ekmd:word p word val)
  (let rd ((w word) (r '()))
    (if (null? w)
      (if (null? r) val (reverse-list->string r))
      (let ((c (read-char p)))
        (rd (cdr w)
          (if (eq? #t (car w))
            (cons* c r)
          (if (eqv? c (car w))
            r
            (ekmd:error "unknown word"))))))))

(define ekmd:esc '(
  (#\u . #t)
  (#\" . #\")
  (#\\ . #\\)
  (#\/ . #\/)
  (#\b . #\bs)
  (#\f . #\ff)
  (#\n . #\lf)
  (#\r . #\cr)
  (#\t . #\ht)
))

(define (ekmd:string p)
  (let ((s (read-delimited "\"\\" p 'split)))
    (case (cdr s)
      ((#\")
        (car s))
      ((#\\)
        (let* ((c (or (assv-ref ekmd:esc (read-char p))
                      (ekmd:error "unknown escape")))
               (c (if (eq? #t c)
                    (integer->char (string->number (ekmd:word p '(#t #t #t #t) 0) 16))
                    c)))
          (string-append (car s) (string c) (ekmd:string p))))
      (else
        (ekmd:error "open string") ""))))

(define (ekmd:find k s tab)
  (find (lambda (e)
    (or (eq? (car e) #t)
        (if (symbol? (car e)) (eq? (car e) k)
        (string-prefix? (car e) s))))
    tab))

(define (ekmd:new k s tmpl)
  (let ((t (ekmd:find k s tmpl)))
    (if t
      (let ((g (cons k (copy-tree (cdr t)))))
        (set! ekmd:glyphs (cons* g ekmd:glyphs))
        g)
      #f)))

(define (ekmd:object p mask tmpl)
  (let obj ((r '()))
    (case (ekmd:next p)
      ((#\,)
        (obj r))
      ((#\")
        (let* ((s (ekmd:string p))
               (k (string->symbol s))
               (m (ekmd:find k s mask))
               (v (if (eqv? #\: (ekmd:next p))
                    (ekmd:value p (if (and m (pair? (cdr m))) (cdr m) '()) tmpl)
                    #f)))
          (obj
            (if m
              (if (char? (cdr m))
                (if (eqv? #\c (cdr m))
                  (if (and (string? v) (string-prefix? "U+" v))
                    (string->number (string-drop v 2) 16)
                    v)
                (if (eqv? #\d (cdr m))
                  (begin (set! ekmd:defaults (assq-set! ekmd:defaults k v))
                         '())
                  (cons* (cdr m) v r)))
              (if (and (pair? v) (char? (car v)))
                (let ((g (or (assq k ekmd:glyphs) (ekmd:new k s tmpl))))
                  (if g
                    (let setpv ((pv v))
                      (if (null? pv)
                        r
                        (let ((i (char->integer (first pv)))
                              (v (second pv)))
                          (if (pair? v)
                            (let ((e (list-ref g i)))
                              (set-car! e (car v))
                              (if (pair? (cdr v)) (set-cdr! e (cadr v))))
                            (list-set! g i v))
                          (setpv (cddr pv)))))
                    r))
                (acons k v r)))
              r))))
      ((#\})
        r)
      (else
        (ekmd:error "open object") #f))))

(define (ekmd:value p mask tmpl)
  (let ((c (ekmd:next p)))
    (case c
      ((#\{)
        (ekmd:object p mask tmpl))
      ((#\[)
        (let lst ((l '()))
          (let ((c (ekmd:next p)))
            (case c
              ((#\,)
                (lst l))
              ((#\])
                (reverse l))
              (else
                (unread-char c p)
                (lst (cons* (ekmd:value p mask tmpl) l)))))))
      ((#\")
        (ekmd:string p))
      ((#\t)
        (ekmd:word p '(#\r #\u #\e) #t))
      ((#\f)
        (ekmd:word p '(#\a #\l #\s #\e) #f))
      ((#\n)
        (ekmd:word p '(#\u #\l #\l) '()))
      (else
        (if (char-set-contains? ekmd:dig c)
          (string->number
            (reverse-list->string
              (let num ((n (list c)) (d (read-char p)))
                (cond
                  ((eof-object? d)
                    n)
                  ((char-set-contains? ekmd:num d)
                    (num (cons* d n) (read-char p)))
                  (else
                    (unread-char d p)
                    n)))))
          (ekmd:error "unknown element"))))))


(define (ekmd:loc dir font)
  (let* ((sys (utsname:sysname (uname)))
         (ls (if (string-contains-ci sys "windows")
              (list (getenv "LOCALAPPDATA")
                    (getenv "CommonProgramFiles")
                    (getenv "CommonProgramFiles(x86)"))
             (if (string-contains-ci sys "linux")
              (list (getenv "XDG_DATA_HOME")
                    (getenv "XDG_DATA_DIRS"))
              (list "~/Library/Application Support"
                    "/Library/Application Support"))))
         (ls (map (lambda (l)
              (string-join
                (append! (string-split l #\\) (list "SMuFL/Fonts" font))
                "/"))
              ls)))
    (if (string-null? dir) ls (cons* dir ls))))

(define-public (ekmd:read dir font name mask tmpl)
  (let loc ((dl (ekmd:loc dir font))
            (nl (list
              (string-append (string-downcase name) "_metadata")
              (string-downcase name)
              "metadata")))
    (if (null? dl)
      #f
      (let ((fn (filter-map (lambda (n)
                  (let ((fn (string-append (car dl) "/" n ".json")))
                    (if (file-exists? fn) fn #f)))
                  nl)))
        (if (null? fn)
          (loc (cdr dl) nl)
          (let* ((p (open-input-file (car fn) #:encoding "utf-8"))
                 (t (ekmd:value p mask tmpl)))
            (close-port p)
            t))))))

(define-public (ekmd:name->cp gn)
  (let n->c ((g ekmd:glyphs))
    (if (null? g) #t
    (let ((c (assq-ref gn (caar g))))
      (if c (set-car! (car g) c))
      (n->c (cdr g))))))

(define-public (ekmd:find-file dir font name)
  (or (ly:find-file name)
      (let loc ((dl (ekmd:loc dir font)))
        (if (null? dl)
          #f
          (let ((fn (string-append (car dl) "/" name)))
            (if (file-exists? fn)
              fn
              (loc (cdr dl))))))))

(define-public (ekmd:load dir font name)
  (let ((fn (ekmd:find-file dir font name)))
    (if fn
      (let* ((p (open-input-file fn))
             (tab (read p)))
        (close-port p)
        (set! ekmd:dir (dirname fn))
        tab)
      #f)))

(define-public (ekmd:set-dg! tab)
  (if tab (begin
    (set! ekmd:defaults (or (assq-ref tab 'defaults) '()))
    (set! ekmd:glyphs (or (assq-ref tab 'glyphs) '()))))
  tab)

(define-public (ekmd:save fn tab)
  (let* ((p (open-output-file fn)))
    (write tab p)
    (close-port p)))

(define-public ekmd:glyphnames
  '((noteheadXWhole . 57511) (noteheadXOrnateEllipse . 57515) (noteheadXOrnate . 57514) (noteheadXHalf . 57512) (noteheadXDoubleWhole . 57510) (noteheadXBlack . 57513) (noteheadWholeWithX . 57525) (noteheadWholeFilled . 57594) (noteheadWhole . 57506) (noteheadVoidWithX . 57527) (noteheadTriangleUpWhole . 57531) (noteheadTriangleUpWhite . 57533) (noteheadTriangleUpRightWhite . 57544) (noteheadTriangleUpRightBlack . 57545) (noteheadTriangleUpHalf . 57532) (noteheadTriangleUpDoubleWhole . 57530) (noteheadTriangleUpBlack . 57534) (noteheadTriangleRoundDownWhite . 57548) (noteheadTriangleRoundDownBlack . 57549) (noteheadTriangleRightWhite . 57537) (noteheadTriangleRightBlack . 57538) (noteheadTriangleLeftWhite . 57535) (noteheadTriangleLeftBlack . 57536) (noteheadTriangleDownWhole . 57540) (noteheadTriangleDownWhite . 57542) (noteheadTriangleDownHalf . 57541) (noteheadTriangleDownDoubleWhole . 57539) (noteheadTriangleDownBlack . 57543) (noteheadSquareWhite . 57528) (noteheadSquareBlackWhite . 57627) (noteheadSquareBlackLarge . 57626) (noteheadSquareBlack . 57529) (noteheadSlashedWhole2 . 57556) (noteheadSlashedWhole1 . 57555) (noteheadSlashedHalf2 . 57554) (noteheadSlashedHalf1 . 57553) (noteheadSlashedDoubleWhole2 . 57558) (noteheadSlashedDoubleWhole1 . 57557) (noteheadSlashedBlack2 . 57552) (noteheadSlashedBlack1 . 57551) (noteheadSlashX . 57606) (noteheadSlashWhiteWhole . 57602) (noteheadSlashWhiteMuted . 57609) (noteheadSlashWhiteHalf . 57603) (noteheadSlashWhiteDoubleWhole . 57610) (noteheadSlashVerticalEndsSmall . 57605) (noteheadSlashVerticalEndsMuted . 57607) (noteheadSlashVerticalEnds . 57600) (noteheadSlashHorizontalEndsMuted . 57608) (noteheadSlashHorizontalEnds . 57601) (noteheadSlashDiamondWhite . 57604) (noteheadRoundWhiteWithDotLarge . 57618) (noteheadRoundWhiteWithDot . 57621) (noteheadRoundWhiteSlashedLarge . 57623) (noteheadRoundWhiteSlashed . 57625) (noteheadRoundWhiteLarge . 57617) (noteheadRoundWhiteDoubleSlashed . 57629) (noteheadRoundWhite . 57620) (noteheadRoundBlackSlashedLarge . 57622) (noteheadRoundBlackSlashed . 57624) (noteheadRoundBlackLarge . 57616) (noteheadRoundBlackDoubleSlashed . 57628) (noteheadRoundBlack . 57619) (noteheadRectangularClusterWhiteTop . 57669) (noteheadRectangularClusterWhiteMiddle . 57670) (noteheadRectangularClusterWhiteBottom . 57671) (noteheadRectangularClusterBlackTop . 57666) (noteheadRectangularClusterBlackMiddle . 57667) (noteheadRectangularClusterBlackBottom . 57668) (noteheadPlusWhole . 57517) (noteheadPlusHalf . 57518) (noteheadPlusDoubleWhole . 57516) (noteheadPlusBlack . 57519) (noteheadParenthesisRight . 57590) (noteheadParenthesisLeft . 57589) (noteheadParenthesis . 57550) (noteheadNull . 57509) (noteheadNancarrowSine . 61088) (noteheadMoonWhite . 57546) (noteheadMoonBlack . 57547) (noteheadLargeArrowUpWhole . 57582) (noteheadLargeArrowUpHalf . 57583) (noteheadLargeArrowUpDoubleWhole . 57581) (noteheadLargeArrowUpBlack . 57584) (noteheadLargeArrowDownWhole . 57586) (noteheadLargeArrowDownHalf . 57587) (noteheadLargeArrowDownDoubleWhole . 57585) (noteheadLargeArrowDownBlack . 57588) (noteheadHeavyXHat . 57593) (noteheadHeavyX . 57592) (noteheadHalfWithX . 57526) (noteheadHalfFilled . 57595) (noteheadHalf . 57507) (noteheadDoubleWholeWithX . 57524) (noteheadDoubleWholeSquare . 57505) (noteheadDoubleWhole . 57504) (noteheadDiamondWholeOld . 57568) (noteheadDiamondWhole . 57560) (noteheadDiamondWhiteWide . 57566) (noteheadDiamondWhite . 57565) (noteheadDiamondOpen . 57596) (noteheadDiamondHalfWide . 57562) (noteheadDiamondHalfOld . 57569) (noteheadDiamondHalfFilled . 57571) (noteheadDiamondHalf . 57561) (noteheadDiamondDoubleWholeOld . 57567) (noteheadDiamondDoubleWhole . 57559) (noteheadDiamondClusterWhiteTop . 57660) (noteheadDiamondClusterWhiteMiddle . 57661) (noteheadDiamondClusterWhiteBottom . 57662) (noteheadDiamondClusterWhite3rd . 57658) (noteheadDiamondClusterWhite2nd . 57656) (noteheadDiamondClusterBlackTop . 57663) (noteheadDiamondClusterBlackMiddle . 57664) (noteheadDiamondClusterBlackBottom . 57665) (noteheadDiamondClusterBlack3rd . 57659) (noteheadDiamondClusterBlack2nd . 57657) (noteheadDiamondBlackWide . 57564) (noteheadDiamondBlackOld . 57570) (noteheadDiamondBlack . 57563) (noteheadCowellThirteenthNoteSeriesWhole . 61104) (noteheadCowellThirteenthNoteSeriesHalf . 61105) (noteheadCowellThirteenthNoteSeriesBlack . 61106) (noteheadCowellThirdNoteSeriesWhole . 61089) (noteheadCowellThirdNoteSeriesHalf . 61090) (noteheadCowellThirdNoteSeriesBlack . 61091) (noteheadCowellSeventhNoteSeriesWhole . 61095) (noteheadCowellSeventhNoteSeriesHalf . 61096) (noteheadCowellSeventhNoteSeriesBlack . 61097) (noteheadCowellNinthNoteSeriesWhole . 61098) (noteheadCowellNinthNoteSeriesHalf . 61099) (noteheadCowellNinthNoteSeriesBlack . 61100) (noteheadCowellFifthNoteSeriesWhole . 61092) (noteheadCowellFifthNoteSeriesHalf . 61093) (noteheadCowellFifthNoteSeriesBlack . 61094) (noteheadCowellFifteenthNoteSeriesWhole . 61107) (noteheadCowellFifteenthNoteSeriesHalf . 61108) (noteheadCowellFifteenthNoteSeriesBlack . 61109) (noteheadCowellEleventhSeriesBlack . 61103) (noteheadCowellEleventhNoteSeriesWhole . 61101) (noteheadCowellEleventhNoteSeriesHalf . 61102) (noteheadClusterWholeTop . 57647) (noteheadClusterWholeMiddle . 57648) (noteheadClusterWholeBottom . 57649) (noteheadClusterWhole3rd . 57641) (noteheadClusterWhole2nd . 57637) (noteheadClusterSquareWhite . 57632) (noteheadClusterSquareBlack . 57633) (noteheadClusterRoundWhite . 57634) (noteheadClusterRoundBlack . 57635) (noteheadClusterQuarterTop . 57653) (noteheadClusterQuarterMiddle . 57654) (noteheadClusterQuarterBottom . 57655) (noteheadClusterQuarter3rd . 57643) (noteheadClusterQuarter2nd . 57639) (noteheadClusterHalfTop . 57650) (noteheadClusterHalfMiddle . 57651) (noteheadClusterHalfBottom . 57652) (noteheadClusterHalf3rd . 57642) (noteheadClusterHalf2nd . 57638) (noteheadClusterDoubleWholeTop . 57644) (noteheadClusterDoubleWholeMiddle . 57645) (noteheadClusterDoubleWholeBottom . 57646) (noteheadClusterDoubleWhole3rd . 57640) (noteheadClusterDoubleWhole2nd . 57636) (noteheadCircledXLarge . 57580) (noteheadCircledWholeLarge . 57578) (noteheadCircledWhole . 57574) (noteheadCircledHalfLarge . 57577) (noteheadCircledHalf . 57573) (noteheadCircledDoubleWholeLarge . 57579) (noteheadCircledDoubleWhole . 57575) (noteheadCircledBlackLarge . 57576) (noteheadCircledBlack . 57572) (noteheadCircleXWhole . 57521) (noteheadCircleXHalf . 57522) (noteheadCircleXDoubleWhole . 57520) (noteheadCircleX . 57523) (noteheadCircleSlash . 57591) (noteheadBlack . 57508) (noteTiWhole . 57686) (noteTiHalf . 57694) (noteTiBlack . 57702) (noteTeWhole . 61160) (noteTeHalf . 61169) (noteTeBlack . 61178) (noteSoWhole . 57684) (noteSoHalf . 57692) (noteSoBlack . 57700) (noteSiWhole . 57687) (noteSiHalf . 57695) (noteSiBlack . 57703) (noteShapeTriangleUpWhite . 57786) (noteShapeTriangleUpDoubleWhole . 60629) (noteShapeTriangleUpBlack . 57787) (noteShapeTriangleRoundWhite . 57790) (noteShapeTriangleRoundLeftWhite . 57802) (noteShapeTriangleRoundLeftDoubleWhole . 60637) (noteShapeTriangleRoundLeftBlack . 57803) (noteShapeTriangleRoundDoubleWhole . 60631) (noteShapeTriangleRoundBlack . 57791) (noteShapeTriangleRightWhite . 57780) (noteShapeTriangleRightDoubleWhole . 60626) (noteShapeTriangleRightBlack . 57781) (noteShapeTriangleLeftWhite . 57782) (noteShapeTriangleLeftDoubleWhole . 60627) (noteShapeTriangleLeftBlack . 57783) (noteShapeSquareWhite . 57778) (noteShapeSquareDoubleWhole . 60625) (noteShapeSquareBlack . 57779) (noteShapeRoundWhite . 57776) (noteShapeRoundDoubleWhole . 60624) (noteShapeRoundBlack . 57777) (noteShapeQuarterMoonWhite . 57794) (noteShapeQuarterMoonDoubleWhole . 60633) (noteShapeQuarterMoonBlack . 57795) (noteShapeMoonWhite . 57788) (noteShapeMoonLeftWhite . 57798) (noteShapeMoonLeftDoubleWhole . 60635) (noteShapeMoonLeftBlack . 57799) (noteShapeMoonDoubleWhole . 60630) (noteShapeMoonBlack . 57789) (noteShapeKeystoneWhite . 57792) (noteShapeKeystoneDoubleWhole . 60632) (noteShapeKeystoneBlack . 57793) (noteShapeIsoscelesTriangleWhite . 57796) (noteShapeIsoscelesTriangleDoubleWhole . 60634) (noteShapeIsoscelesTriangleBlack . 57797) (noteShapeDiamondWhite . 57784) (noteShapeDiamondDoubleWhole . 60628) (noteShapeDiamondBlack . 57785) (noteShapeArrowheadLeftWhite . 57800) (noteShapeArrowheadLeftDoubleWhole . 60636) (noteShapeArrowheadLeftBlack . 57801) (noteSeWhole . 61157) (noteSeHalf . 61166) (noteSeBlack . 61175) (noteRiWhole . 61153) (noteRiHalf . 61162) (noteRiBlack . 61171) (noteReWhole . 57681) (noteReHalf . 57689) (noteReBlack . 57697) (noteRaWhole . 61154) (noteRaHalf . 61163) (noteRaBlack . 61172) (noteMiWhole . 57682) (noteMiHalf . 57690) (noteMiBlack . 57698) (noteMeWhole . 61155) (noteMeHalf . 61164) (noteMeBlack . 61173) (noteLiWhole . 61158) (noteLiHalf . 61167) (noteLiBlack . 61176) (noteLeWhole . 61159) (noteLeHalf . 61168) (noteLeBlack . 61177) (noteLaWhole . 57685) (noteLaHalf . 57693) (noteLaBlack . 57701) (noteHWhole . 57725) (noteHSharpWhole . 57726) (noteHSharpHalf . 57749) (noteHSharpBlack . 57772) (noteHHalf . 57748) (noteHBlack . 57771) (noteGWhole . 57723) (noteGSharpWhole . 57724) (noteGSharpHalf . 57747) (noteGSharpBlack . 57770) (noteGHalf . 57746) (noteGFlatWhole . 57722) (noteGFlatHalf . 57745) (noteGFlatBlack . 57768) (noteGBlack . 57769) (noteFiWhole . 61156) (noteFiHalf . 61165) (noteFiBlack . 61174) (noteFaWhole . 57683) (noteFaHalf . 57691) (noteFaBlack . 57699) (noteFWhole . 57720) (noteFSharpWhole . 57721) (noteFSharpHalf . 57744) (noteFSharpBlack . 57767) (noteFHalf . 57743) (noteFFlatWhole . 57719) (noteFFlatHalf . 57742) (noteFFlatBlack . 57765) (noteFBlack . 57766) (noteEmptyWhole . 57773) (noteEmptyHalf . 57774) (noteEmptyBlack . 57775) (noteEWhole . 57717) (noteESharpWhole . 57718) (noteESharpHalf . 57741) (noteESharpBlack . 57764) (noteEHalf . 57740) (noteEFlatWhole . 57716) (noteEFlatHalf . 57739) (noteEFlatBlack . 57762) (noteEBlack . 57763) (noteDoWhole . 57680) (noteDoHalf . 57688) (noteDoBlack . 57696) (noteDiWhole . 61152) (noteDiHalf . 61161) (noteDiBlack . 61170) (noteDWhole . 57714) (noteDSharpWhole . 57715) (noteDSharpHalf . 57738) (noteDSharpBlack . 57761) (noteDHalf . 57737) (noteDFlatWhole . 57713) (noteDFlatHalf . 57736) (noteDFlatBlack . 57759) (noteDBlack . 57760) (noteCWhole . 57711) (noteCSharpWhole . 57712) (noteCSharpHalf . 57735) (noteCSharpBlack . 57758) (noteCHalf . 57734) (noteCFlatWhole . 57710) (noteCFlatHalf . 57733) (noteCFlatBlack . 57756) (noteCBlack . 57757) (noteBWhole . 57708) (noteBSharpWhole . 57709) (noteBSharpHalf . 57732) (noteBSharpBlack . 57755) (noteBHalf . 57731) (noteBFlatWhole . 57707) (noteBFlatHalf . 57730) (noteBFlatBlack . 57753) (noteBBlack . 57754) (noteAWhole . 57705) (noteASharpWhole . 57706) (noteASharpHalf . 57729) (noteASharpBlack . 57752) (noteAHalf . 57728) (noteAFlatWhole . 57704) (noteAFlatHalf . 57727) (noteAFlatBlack . 57750) (noteABlack . 57751) (flag8thUp . 57920) (flag8thDown . 57921) (flag64thUp . 57926) (flag64thDown . 57927) (flag512thUp . 57932) (flag512thDown . 57933) (flag32ndUp . 57924) (flag32ndDown . 57925) (flag256thUp . 57930) (flag256thDown . 57931) (flag16thUp . 57922) (flag16thDown . 57923) (flag128thUp . 57928) (flag128thDown . 57929) (flag1024thUp . 57934) (flag1024thDown . 57935)))
