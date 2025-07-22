      *-------------*  PROGRAMMA  BATCH  MVS COBOL2 DB2  *-------------*

       ID  DIVISION.
       PROGRAM-ID.                     AAACH07.

      ******************************************************************
      *                                                                *
      *  RICALCOLO MENSILE DEI CONTROVALORI DELLE REGISTRAZIONI DI     *
      *  PERDITA ('FONPER' E 'PERDIT') IN VALUTA ESTERA.               *
      *                                                                *
      *  CREAZIONE SULLE TABELLE CONTABILI DI :                        *
      *                                                                *
      *  A) UNA SCRITTURA DI STORNO DEI MOVIMENTI ORIGINALI DI         *
      *     PERDITA.                                                   *
      *                                                                *
      *  B) UNA SCRITTURA DI SALDO A ZERO COMPRENSIVO DELLA SCRITTURA  *
      *     ORIGINALE E DEL RELATIVO STORNO.                           *
      *                                                                *
      *  C) UNA SCRITTURA DI APERTURA DI FONDO PERDITA CON IL          *
      *     CONTROVALORE RICALCOLATO AL CAMBIO ULTIMO DEL MESE.        *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEOUT      ASSIGN TO UT-S-FILEOUT.
           SELECT STAMPA       ASSIGN TO UT-S-STAMPA.

      *--------------*
       DATA DIVISION.
      *--------------*

       FILE SECTION.

       FD  FILEOUT
           RECORDING MODE IS F
           BLOCK  CONTAINS 0 RECORDS.
       01  REC-FILEOUT         PIC X(400).

       FD  STAMPA
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RIGA                         PIC X(133).

      *------------------------*
       WORKING-STORAGE SECTION.
      *------------------------*
       01  FILLER                      PIC X(8) VALUE '**DCPARM'.
           COPY DCPARM.
       01  FILLER                      PIC X(8) VALUE '*DCDB2ER'.
           COPY DCDB2ER.
       01  FILLER                      PIC X(8) VALUE '*DCKYNUM'.
           COPY DCKYNUM.
       01  FILLER                      PIC X(8) VALUE '*DCWCAMB'.
           COPY DCWCAMB.
       01  FILLER                      PIC X(8) VALUE '*DCWDTEL'.
           COPY DCWDTEL.
       01  FILLER                      PIC X(8) VALUE '*DCWCHIU'.
           COPY DCWCHIU.
       01  FILLER                      PIC X(8) VALUE '*DCARR0*'.
           COPY DCARR0.
       01  FILLER                      PIC X(8) VALUE '*DCWCCOS'.
           COPY DCWCCOS.

       01  FILLER                      PIC X(8) VALUE 'TAB.CLIE'.
       01  TABCLIE.
         05  ELEM-TABCLIE OCCURS 30.
             10  TBCL-PROLOMER           PIC X(2).
             10  TBCL-CODLINEA           PIC X(2).
             10  TBCL-TIPOSTAG           PIC X(1).
             10  TBCL-SECSCELT           PIC X(1).
             10  TBCL-FLASTOCK           PIC X(1).
             10  TBCL-CENCOSTO           PIC X(5).
             10  TBCL-IMPMOVIM           PIC S9(11)V99 COMP-3.
             10  TBCL-CVLPRIMA           PIC S9(11)V99 COMP-3.
             10  TBCL-CVLDOPOO           PIC S9(11)V99 COMP-3.
             10  TBCL-CVLDIFFP           PIC  9(11)V99 COMP-3.
             10  TBCL-CVLDIFFN           PIC  9(11)V99 COMP-3.
             10  TBCL-CVLDIFFE           PIC S9(11)V99 COMP-3.

       01  FILLER                      PIC X(8) VALUE 'TAB.STAM'.
       01  TABSTAM.
         05  ELEM-TABSTAM OCCURS 3000.
             10  TBST-CODSOCIE           PIC XX.
             10  TBST-CODCLIEN           PIC X(5).
             10  TBST-SIGLADIV           PIC XXX.
             10  TBST-CENCOSTO           PIC X(5).
             10  TBST-IMPMOVIM           PIC S9(11)V99 COMP-3.
             10  TBST-CVLPRIMA           PIC S9(11)V99 COMP-3.
             10  TBST-CVLDOPOO           PIC S9(11)V99 COMP-3.
             10  TBST-CVLDIFFE           PIC S9(11)V99 COMP-3.
             10  TBST-CVLDIFFP           PIC S9(11)V99 COMP-3.
             10  TBST-CVLDIFFN           PIC S9(11)V99 COMP-3.

       01  FILLER                      PIC X(8) VALUE 'TAB.CECO'.
       01  TABCECO.
         05  ELEM-TABCECO OCCURS 300.
             10  TBCC-CODSOCIE           PIC XX.
             10  TBCC-CENCOSTO           PIC X(5).
             10  TBCC-CVLPRIMA           PIC S9(11)V99 COMP-3.
             10  TBCC-CVLDOPOO           PIC S9(11)V99 COMP-3.
             10  TBCC-CVLDIFFP           PIC S9(11)V99 COMP-3.
             10  TBCC-CVLDIFFN           PIC S9(11)V99 COMP-3.
             10  TBCC-CVLDIFFE           PIC S9(11)V99 COMP-3.

       01  FILLER                      PIC X(8) VALUE '*COMODI*'.
       01  COMODI.
           05  WS-REC-STAMPA.
               10 WS-ASA                 PIC X(01) VALUE SPACES.
               10 WS-RIG                 PIC X(132) VALUE SPACES.
           05  WS-SKEDA.
               07  WS-SKPAR.
                   10  SK-TIPOSKED       PIC X(2).
                   10  FILLER            PIC X.
                   10  SK-CODSOCIE       PIC X(2).
                   10  FILLER            PIC X.
                   10  SK-AGGIORNA       PIC X(2).
                   10  FILLER            PIC X(72).
           05 WS-PARTCONT              PIC X(6).
           05 WS-NRMOVCON              PIC X(6).
           05 WS-DTREGMOV              PIC X(8).
           05 WS-DTSCADEN              PIC X(8).
           05 WS-OLDSCADE              PIC X(8).
           05 WS-CDMOVCON              PIC X.
           05 WS-CTSTFOND              PIC X(6).
           05 WS-CTCASSAN              PIC X(6).
           05 WS-NRMOVRIF              PIC X(6).
           05 WS-NRMOVRI1              PIC X(6).
           05 WS-NRMOVRI2              PIC X(6).
           05 WS-DATAGIOR              PIC X(8).
           05 WS-MINIDATA              PIC X(8).
           05 WS-MAXIDATA              PIC X(8).
           05 WS-DTCAMBIO              PIC X(8).
           05 WS-SALVELEM              PIC X(49).
           05 WS-INDSEGNO              PIC X.
           05 WS-SOCISCOP              PIC XX.
           05 WS-CLIESCOP              PIC X(5).
           05 WS-CDMOSCOP              PIC X.
           05 WS-DTRESCOP              PIC X(8).
           05 WS-NRMOSCOP              PIC X.
           05 WS-DTSCSCOP              PIC X(8).
           05 WS-OLDSSCOP              PIC X(8).
           05 WS-INDSSCOP              PIC X.
           05 WS-INSCOCON              PIC S9(11)V99  COMP-3.
           05 WS-COMDIFFE              PIC S9(11)V99  COMP-3.
           05 WS-CVLSCOPE              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TCVSCOPE              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLMOVIM              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-PARCVLPR              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-PARCVLDO              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-PARIMPOR              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLSCOLD              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOCVLDFP              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOCVLDFN              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVSCANEW              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-DIFFSCOP              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-COMODIFF              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLDIFFE              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLDIFFP              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLDIFFN              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TCVLPRIM              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TCVLDOPO              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TIMPORTO              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOCVLDOP              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOCVLPRI              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOCVLDIP              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOCVLDIN              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-IMPXCONV              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-IVAXCONV              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOTCVLDC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOTCIVDC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLDIFDC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLDIVDC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-DIFFEREN              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-DIFFERCC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CRIGSTAM              PIC S9(5)     COMP-3 VALUE +60.
           05 WS-DIFFEDEC              PIC 9(11)V99.
           05 WS-DIFFEXXX REDEFINES WS-DIFFEDEC PIC 9(13).
           05 WS-DIFFEYYY REDEFINES WS-DIFFEDEC.
              10 WS-DIFFEINT           PIC 9(11).
              10 FILLER                PIC 99.
           05 WS-CODDIVRI              PIC XX.
           05 WS-CODDIVSO              PIC XX.
           05 WS-CODDIVIS              PIC XX.
           05 WS-SIGDVGTM              PIC XXX.
           05 WS-DECIDIVI              PIC X.
           05 WS-CVLDIFED              PIC Z(10)9,99-.
           05 WS-CVLMOVED              PIC Z(10)9,99.
           05 WS-CVLPRIED              PIC Z(7)9,99-.
           05 WS-CVLDOPED              PIC Z(7)9,99-.
           05 WS-IMPORTED              PIC Z(7)9,99-.
           05 WS-PEREDITT              PIC Z(7)9,99-.
           05 WS-CVLDFPED              PIC Z(7)9,99.
           05 WS-CVLDFPEX REDEFINES WS-CVLDFPED PIC X(11).
           05 WS-CVLDFNED              PIC Z(7)9,99.
           05 WS-CVLDFNEX REDEFINES WS-CVLDFNED PIC X(11).
           05 WS-INDITABE              PIC S9(5).
           05 WS-INDICLIE              PIC S9(5).
           05 WS-INDICMAX              PIC S9(5).
           05 WS-INDITAB1              PIC S9(5).
           05 WS-CODSOCIE              PIC XX.
           05 WS-NOMSOCIE              PIC X(23).
           05 WS-SIGLADIV              PIC XXX.
           05 WS-CODCLIEN              PIC X(5).
           05 WS-CENCOSTO              PIC X(5).
           05 WS-SWORDINA              PIC XX                VALUE 'SI'.
           05 WS-CENESONO              PIC XX                VALUE 'NO'.
           05 WS-CODSOCST              PIC XX.
           05 WS-NOMSOCST              PIC X(23).
           05 WS-TOTALE                PIC S9(11)V99         VALUE +0.
           05 WS-CVLTOTFP              PIC S9(11)V99  COMP-3 VALUE 0.
           05 WS-CVLTOTFN              PIC S9(11)V99  COMP-3 VALUE 0.
           05 WS-CVLTOTFP-S            PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLTOTFN-S            PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-RIGATOT.
              10 FILLER                PIC X(24) VALUE SPACES.
              10 FILLER                PIC X(10) VALUE '* TOTALE *'.
              10 FILLER                PIC X(14) VALUE SPACES.
              10 WS-CVLTOPRI           PIC Z(7)9,99-.
              10 FILLER                PIC X     VALUE SPACES.
              10 WS-CVLTODOP           PIC Z(7)9,99-.
              10 FILLER                PIC X     VALUE SPACES.
              10 WS-CVLTODFP           PIC Z(7)9,99-.
              10 FILLER                PIC X     VALUE SPACES.
              10 WS-CVLTODFN           PIC Z(7)9,99.
              10 FILLER                PIC XX    VALUE SPACES.
              10 WS-CVLTODIF           PIC Z(7)9,99-.
           05 WS-CVLDIFSP              PIC ZZ.ZZZ.ZZZ.ZZ9,99+.
           05 WS-CVLDIFSP-X REDEFINES WS-CVLDIFSP PIC X(18).
           05 WS-CVLDIFSM              PIC ZZ.ZZZ.ZZZ.ZZ9,99+.
           05 WS-CVLDIFSM-X REDEFINES WS-CVLDIFSM PIC X(18).

      *------------------------------ DANAGRAF.TLINEAVEN
       01  FILLER                      PIC X(8) VALUE 'FDANA497'.
           EXEC SQL INCLUDE FDANA497 END-EXEC.

      *------------------------------ DANAGRAF.TSOCIETA
       01  FILLER                      PIC X(8) VALUE 'FDANA531'.
           EXEC SQL INCLUDE FDANA531 END-EXEC.

      *------------------------------ DANAGRAF.TCLIENTI
       01  FILLER                      PIC X(8) VALUE 'FDANA901'.
           EXEC SQL INCLUDE FDANA901 END-EXEC.

      *------------------------------ DANAGRAF.TDIVISE
       01  FILLER                      PIC X(8) VALUE 'FDANA963'.
           EXEC SQL INCLUDE FDANA963 END-EXEC.

      *------------------------------ DCONTCLI.TMOVIMENTI
       01  FILLER                      PIC X(8) VALUE 'FDCLI852'.
           EXEC SQL INCLUDE FDCLI852 END-EXEC.

      *------------------------------ DCONTCLI.TSICOPLNEW
       01  FILLER                      PIC X(8) VALUE 'FDCLI200'.
           EXEC SQL INCLUDE FDCLI200 END-EXEC.

      *------------------------------ DCONTCLI.TMOVISCAD
       01  FILLER                      PIC X(8) VALUE 'FDCLI855'.
           EXEC SQL INCLUDE FDCLI855 END-EXEC.

      *------------------------------ DCONTCLI.TMOVIDCE
       01  FILLER                      PIC X(8) VALUE 'FDCLI858'.
           EXEC SQL INCLUDE FDCLI858 END-EXEC.

      *------------------------------ DCONTCLI.TCONTIDCE
       01  FILLER                      PIC X(8) VALUE 'FDCLI861'.
           EXEC SQL INCLUDE FDCLI861 END-EXEC.

      *   ----------------------------------------
           EXEC SQL DECLARE PMOVSCA CURSOR FOR
                    SELECT S.CODSOCIE,S.CODDIVIS,S.CODCLIEN,
                           S.CDMOVCON,S.DTREGMOV,S.NRMOVCON,
                           S.CVLMOVIM,S.IMPMOVIM,S.DTSCADEN,
                           S.OLDSCADE,S.IMPORIVA,S.INDSEGNO,
                           S.CVLXXIVA,D.SIGLADIV,M.NRMOVRIF,
                           M.CENCOSTO,P.CONTPROV
                      FROM DCONTCLI.TMOVISCAD  S,
                           DCONTCLI.TMOVIMENTI M,
                           DCONTCLI.TMOVIDCE   E,
                           DCONTCLI.TCONTIDCE  P,
                           DANAGRAF.TSOCIETA   T,
                           DANAGRAF.TDIVISE    D
                     WHERE S.CODSOCIE = :C855-CODSOCIE
                       AND S.CDMOVCON = 'D'
                       AND NOT S.DTREGMOV > :WS-MAXIDATA
                       AND S.FLAGSALD <> 'S'
                       AND S.FLAGSALD <> 'A'
                       AND S.CODSOCIE =  M.CODSOCIE
                       AND S.CDMOVCON =  M.CDMOVCON
                       AND S.NRMOVCON =  M.NRMOVCON
                       AND S.DTREGMOV =  M.DTREGMOV
                       AND S.CODSOCIE =  E.CODSOCIE
                       AND S.CDMOVCON =  E.CDMOVCON
                       AND S.NRMOVCON =  E.NRMOVCON
                       AND S.DTREGMOV =  E.DTREGMOV
                       AND S.DTSCADEN =  E.DTSCADEN
                       AND S.OLDSCADE =  E.OLDSCADE
                       AND S.CODSOCIE =  T.CODSOCIE
                       AND S.CODDIVIS <> T.CODDIVIS
                       AND NOT (S.CODDIVIS = '75' AND
                                T.CODPAESE = '068')
                       AND T.CODPIANO =  P.CODPIANO
                       AND E.CONTODCE =  P.CONTODCE
                       AND P.CONTPROV IN ('FONPER','PERDIT')
                       AND S.CODDIVIS =  D.CODDIVIS
                       AND D.CODDIVRI =  T.CODDIVRI
                     ORDER BY 1 , 3 , 2 , 4 , 6
           END-EXEC.

           EXEC SQL DECLARE PSICOPL CURSOR FOR
                    SELECT PROLOMER,INSCOCON
                          ,CODLINEA,TIPOSTAG
                          ,SECSCELT,FLASTOCK
                      FROM DCONTCLI.TSICOPLNEW
                     WHERE CODSOCIE=:C200-CODSOCIE
                       AND CODCLIEN=:C200-CODCLIEN
                       AND AARIFERI=:C200-AARIFERI
                       AND INSCOCON>0
           END-EXEC.

           EXEC SQL DECLARE PCVLSCO CURSOR FOR
                    SELECT DISTINCT
                           S.CODSOCIE,S.CODCLIEN,S.CDMOVCON,
                           S.DTREGMOV,S.NRMOVCON,S.DTSCADEN,
                           S.OLDSCADE,S.INDSEGNO,
                           CASE WHEN S.INDSEGNO = '+'
                                THEN S.CVLMOVIM
                           ELSE
                                     S.CVLMOVIM * -1
                           END
                      FROM DCONTCLI.TMOVISCAD  S,
                           DCONTCLI.TMOVIDCE   E,
                           DCONTCLI.TCONTIDCE  P,
                           DANAGRAF.TSOCIETA   T
                     WHERE S.FLAGSTOR     =  ' '
                       AND S.FLAGSALD     <> 'S'
                       AND S.FLAGSALD     <> 'A'
                       AND S.CODSOCIE     = :C855-CODSOCIE
                       AND S.CODCLIEN     = :C855-CODCLIEN
                       AND NOT S.DTREGMOV > :WS-MAXIDATA
                       AND S.CODSOCIE =  E.CODSOCIE
                       AND S.CDMOVCON =  E.CDMOVCON
                       AND S.NRMOVCON =  E.NRMOVCON
                       AND S.DTREGMOV =  E.DTREGMOV
                       AND S.DTSCADEN =  E.DTSCADEN
                       AND S.OLDSCADE =  E.OLDSCADE
                       AND S.CODSOCIE =  T.CODSOCIE
                       AND T.CODPIANO =  P.CODPIANO
                       AND E.CONTODCE =  P.CONTODCE
                       AND NOT P.CONTPROV IN ('FONPER','PERDIT')
                    GROUP BY S.CODSOCIE,S.CODCLIEN,S.CDMOVCON,
                             S.DTREGMOV,S.NRMOVCON,S.DTSCADEN,
                             S.OLDSCADE,S.INDSEGNO,S.CVLMOVIM
           END-EXEC.

           EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           MOVE 'AAACH07'              TO PPNAME
                                          WCOM-PROGNAME.

           EXEC SQL WHENEVER SQLERROR GOTO Z00-ERRORE END-EXEC.

           PERFORM A00-LETTURA-SKEDA
              THRU A00-EX.

           OPEN OUTPUT FILEOUT STAMPA.

           PERFORM A05-CERCA-DTCAMBIO
              THRU A05-EX.

           INITIALIZE TABSTAM
                      TABCECO.
           MOVE 'MAIN'                 TO WCOM-CODABEND.
           MOVE 'OPEN'                 TO WCOM-FILEFUNZ.
           MOVE 'PMOVSCA'              TO WCOM-FILENOME.
           EXEC SQL OPEN  PMOVSCA    END-EXEC.

           PERFORM L00-FETCH-PMOVSCA
              THRU L00-EX.

           PERFORM B00-ELABORA-PMOVSCA
              THRU B00-EX
                   UNTIL SQLCODE = 100.

           MOVE SPACES TO WS-CODSOCIE.

           PERFORM B10-STAMPA-TABELLA
              THRU B10-EX.

           MOVE 'MAIN'                 TO WCOM-CODABEND.
           MOVE 'CLOSE'                TO WCOM-FILEFUNZ.
           MOVE 'PMOVSCA'              TO WCOM-FILENOME.
           MOVE SPACES                 TO WCOM-FILECTRL

           EXEC SQL
                CLOSE PMOVSCA
           END-EXEC.

           DISPLAY 'TOTALE ' WS-TOTALE

           IF SK-AGGIORNA = 'NO'
              EXEC SQL
                   ROLLBACK
              END-EXEC.

           MOVE 'P'                    TO PPROUTIN.
           PERFORM Z10-READPRIN
              THRU Z10-EX.

           CLOSE FILEOUT STAMPA.

           MOVE ZEROES                 TO RETURN-CODE.
CSIins     CALL "CCF_SET_RETCODE" USING RETURN-CODE
           STOP RUN.

      *---------------------------------------------------------------*
       A00-LETTURA-SKEDA.
      *---------------------------------------------------------------*

           MOVE 'S'                    TO PPROUTIN.
           PERFORM Z10-READPRIN
              THRU Z10-EX.

           IF  PPSTAT NOT = '****'
               IF  PPSTAT = 'NOSK'
                   MOVE 'NOSK'         TO WCOM-CODABEND
                   MOVE 'LETTURA'      TO WCOM-FILEFUNZ
                   MOVE 'SKEDA-PARAMETRO' TO WCOM-FILENOME
                   MOVE  WS-SKEDA      TO WCOM-FILECTRL

                   MOVE '**** MANCA SKEDA PARAMETRO ****'
                                       TO WCOM-MESSAGES
                   PERFORM Z00-ERRORE
                      THRU Z00-EX
               ELSE
                   MOVE 'SKER'         TO WCOM-CODABEND
                   MOVE 'LETTURA'      TO WCOM-FILEFUNZ
                   MOVE 'SKEDA'        TO WCOM-FILENOME
                   MOVE  WS-SKEDA      TO WCOM-FILECTRL
                   MOVE  '** ERRORE IN LETTURA SK-PARAM *'
                                       TO WCOM-MESSAGES
                   PERFORM Z00-ERRORE
                      THRU Z00-EX.

           MOVE PPSK1                  TO  WS-SKPAR.

           IF  SK-TIPOSKED NOT EQUAL 'AN'
               MOVE 'SKER'             TO WCOM-CODABEND
               MOVE 'LETTURA'          TO WCOM-FILEFUNZ
               MOVE 'SKEDA-PARAMETRO'  TO WCOM-FILENOME
               MOVE  WS-SKEDA          TO WCOM-FILECTRL
               PERFORM Z00-ERRORE
                  THRU Z00-EX.

            MOVE SK-CODSOCIE           TO A531-CODSOCIE.
            EXEC SQL
                 SELECT A.CODDIVIS,A.CODDIVRI,A.NOMSOCIE,
                        A.SOCIEGTM,A.CODPIANO,B.SIGLADIV,
                        B.DECIDIVI
                   INTO :A531-CODDIVIS,:A531-CODDIVRI,:A531-NOMSOCIE,
                        :A531-SOCIEGTM,:A531-CODPIANO,:WS-SIGDVGTM,
                        :WS-DECIDIVI
                   FROM DANAGRAF.TSOCIETA A,
                        DANAGRAF.TDIVISE  B
                  WHERE A.CODSOCIE = :A531-CODSOCIE
                    AND A.CODDIVIS = B.CODDIVIS
                    AND B.CODDIVRI = A.CODDIVRI
            END-EXEC.

            IF SQLCODE NOT = ZERO
               MOVE 'A00-'             TO WCOM-CODABEND
               MOVE 'SELECT '          TO WCOM-FILEFUNZ
               MOVE 'SOCIETA'          TO WCOM-FILENOME
               MOVE A531-CODSOCIE      TO WCOM-FILECTRL
               PERFORM Z00-ERRORE
                  THRU Z00-EX.

           IF  SK-AGGIORNA NOT EQUAL 'SI' AND 'NO'
               MOVE 'SKAG'             TO WCOM-CODABEND
               MOVE 'AGGIORN'          TO WCOM-FILEFUNZ
               MOVE 'SKEDA-PARAMETRO'  TO WCOM-FILENOME
               MOVE  WS-SKEDA          TO WCOM-FILECTRL
               PERFORM Z00-ERRORE
                  THRU Z00-EX.

           MOVE SK-CODSOCIE            TO C855-CODSOCIE.
           MOVE SPACES                 TO DCWDTEL.
           MOVE 'R'                    TO DTEL-TIPORICH.
           MOVE 'M'                    TO DTEL-DATATIPO.
           MOVE 'AAACH07'              TO DTEL-SIGLDATA.
           MOVE 'SCADTEL'              TO PPCALL.
           CALL PPCALL                 USING DCPARM  DCWDTEL.

           IF  DTEL-RETCOD NOT = ZEROES
               MOVE DTEL-RETCOD        TO WCOM-CODABEND
               MOVE 'SELECT'           TO WCOM-FILEFUNZ
               MOVE 'SCADTEL'          TO WCOM-FILENOME
               MOVE 'DATEDAT'          TO WCOM-FILECTRL
               PERFORM Z00-ERRORE
                  THRU Z00-EX.

           MOVE 'U'            TO PPROUTIN.
           MOVE 'D'            TO PPTPFILE.
           MOVE 'R'            TO PPTPOPER.
           MOVE 'SCADTEL'      TO PPNMFILE.
           PERFORM Z10-READPRIN
              THRU Z10-EX.

           MOVE DTEL-DTINELAB          TO WS-MINIDATA.
           MOVE DTEL-DTFIELAB          TO WS-MAXIDATA.

           MOVE 'G'                    TO DTEL-DATATIPO.
           MOVE 'AAACH07'              TO DTEL-SIGLDATA.
           MOVE 'SCADTEL'              TO PPCALL.
           CALL PPCALL                 USING DCPARM  DCWDTEL.

           IF  DTEL-RETCOD NOT = ZEROES
               MOVE DTEL-RETCOD        TO WCOM-CODABEND
               MOVE 'SELECT'           TO WCOM-FILEFUNZ
               MOVE 'SCADTEL'          TO WCOM-FILENOME
               MOVE 'DATEDAT'          TO WCOM-FILECTRL
               PERFORM Z00-ERRORE
                  THRU Z00-EX.

           MOVE DTEL-DTINELAB          TO WS-DATAGIOR.
           MOVE 'FONPER'               TO C861-CONTPROV.
           MOVE A531-CODPIANO          TO C861-CODPIANO.
           PERFORM L05-LEGGI-CONTIDCE
              THRU L05-EX.

           IF SQLCODE NOT = +0
              PERFORM M00-MESSCONTO
                 THRU M00-EX.

           MOVE 'STFOND'               TO C861-CONTPROV.
           MOVE A531-CODPIANO          TO C861-CODPIANO.
           PERFORM L05-LEGGI-CONTIDCE
              THRU L05-EX.

           IF SQLCODE NOT = +0
              PERFORM M00-MESSCONTO
                 THRU M00-EX.

           MOVE C861-CONTODCE          TO WS-CTSTFOND.
           MOVE 'CASSAN'               TO C861-CONTPROV.
           MOVE A531-CODPIANO          TO C861-CODPIANO.
           PERFORM L05-LEGGI-CONTIDCE
              THRU L05-EX.

           IF SQLCODE NOT = +0
              PERFORM M00-MESSCONTO
                 THRU M00-EX.

           MOVE C861-CONTODCE          TO WS-CTCASSAN.

       A00-EX. EXIT.

      *--------------------------------------------------------------*
       A05-CERCA-DTCAMBIO.
      *--------------------------------------------------------------*

           MOVE 'A05-'        TO WCOM-CODABEND.
           MOVE 'SELECT'      TO WCOM-FILEFUNZ.
           MOVE 'CAMBIST'     TO WCOM-FILENOME.
           MOVE A531-CODDIVRI TO WCOM-FILECTRL.

           EXEC SQL
                SELECT MAX(DTCAMBIO)
                  INTO :WS-DTCAMBIO
                  FROM DANAGRAF.TCAMBISTOR
                WHERE CODDIVRI = :A531-CODDIVRI
                  AND DTCAMBIO BETWEEN :WS-MINIDATA
                                   AND :WS-MAXIDATA
           END-EXEC.

           IF SQLCODE NOT = +0
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

           MOVE 'U'            TO PPROUTIN.
           MOVE 'D'            TO PPTPFILE.
           MOVE 'R'            TO PPTPOPER.
           MOVE 'CAMBIST'      TO PPNMFILE.
           PERFORM Z10-READPRIN
              THRU Z10-EX.

           STRING 'DATA CAMBIO : ' WS-DTCAMBIO
                  DELIMITED BY SIZE INTO PPMESSAG
           MOVE 'M' TO PPROUTIN
           PERFORM Z10-READPRIN
              THRU Z10-EX.

       A05-EX. EXIT.

      *--------------------------------------------------------------*
       B00-ELABORA-PMOVSCA.
      *--------------------------------------------------------------*

           MOVE C855-IMPMOVIM TO WS-IMPXCONV.
           MOVE C855-IMPORIVA TO WS-IVAXCONV.
           MOVE C855-CVLMOVIM TO WS-CVLSCOLD.
           MOVE A531-CODDIVRI TO WS-CODDIVRI.
           MOVE A531-CODDIVIS TO WS-CODDIVSO.
           MOVE C855-CODDIVIS TO WS-CODDIVIS.
           PERFORM C00-CHIAMA-SCBCB10
              THRU C00-EX.
           MOVE CAMB-DECICAMB TO ARR0-DECIDIVI
           MOVE CAMB-CONTROVA(1) TO ARR0-IMPORTOX.
           PERFORM C05-ARROTONDA-IMP
              THRU C05-EX.
           MOVE ARR0-IMPORTOX    TO C855-CVLMOVIM
                                    WS-CVSCANEW.
           MOVE CAMB-DECICAMB TO ARR0-DECIDIVI
           MOVE CAMB-CONTROVA(2) TO ARR0-IMPORTOX.
           PERFORM C05-ARROTONDA-IMP
              THRU C05-EX.
           MOVE ARR0-IMPORTOX    TO C855-CVLXXIVA.
           COMPUTE WS-CVLDIFFE = WS-CVLSCOLD   - C855-CVLMOVIM.

           IF WS-CVLDIFFE NOT = ZEROES
              PERFORM C10-ELABORA-DIFFE
                 THRU C10-EX.

           PERFORM L00-FETCH-PMOVSCA
              THRU L00-EX.

       B00-EX. EXIT.

      *--------------------------------------------------------------*
       B10-STAMPA-TABELLA.
      *--------------------------------------------------------------*

           PERFORM VARYING WS-INDITABE FROM 1 BY 1
             UNTIL WS-INDITABE GREATER 3000 OR
                   TBST-CODSOCIE (WS-INDITABE) NOT GREATER SPACES

                   IF WS-CRIGSTAM > 57 OR
                      WS-CODSOCIE NOT = TBST-CODSOCIE (WS-INDITABE)
                      PERFORM C20-TESTATA
                         THRU C20-EX
                   END-IF
                   COMPUTE TBST-CVLDIFFE (WS-INDITABE) =
                           TBST-CVLDIFFP (WS-INDITABE) -
                           TBST-CVLDIFFN (WS-INDITABE)
                   COMPUTE WS-DIFFEREN = WS-DIFFEREN +
                           TBST-CVLDIFFE (WS-INDITABE)
                   IF TBST-CVLDIFFE (WS-INDITABE) NOT EQUAL ZEROES
                      PERFORM C25-DETTAGLIO
                         THRU C25-EX
                   END-IF
           END-PERFORM.

           PERFORM C30-TOTALI-SOCIETA
              THRU C30-EX.

       B10-EX. EXIT.

      *--------------------------------------------------------------*
       C00-CHIAMA-SCBCB10.
      *--------------------------------------------------------------*

           MOVE SPACES          TO DCWCAMB.
           MOVE WS-DTCAMBIO     TO CAMB-DTCAMBIO.
           MOVE WS-CODDIVSO     TO CAMB-CODCAMBI.
           MOVE WS-CODDIVRI     TO CAMB-CODDRIFE.
           MOVE WS-CODDIVIS     TO CAMB-CODDIVIS.
           MOVE WS-IMPXCONV     TO CAMB-IMPORTOO(1).
           MOVE WS-IVAXCONV     TO CAMB-IMPORTOO(2).
           MOVE 'SCBCB10'       TO PPCALL.
           CALL PPCALL          USING DCPARM DCWCAMB.

           IF CAMB-RETCOD NOT = '****'
              MOVE 'CALL  '      TO WCOM-FILEFUNZ
              MOVE 'SCBCB10  '   TO WCOM-FILENOME
              MOVE 'CAMB'        TO WCOM-CODABEND
              STRING CAMB-DTCAMBIO ' ' CAMB-CODCAMBI ' '
                     CAMB-CODDRIFE ' ' CAMB-CODDIVIS ' '
                     DELIMITED BY SIZE INTO WCOM-FILECTRL
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

           MOVE 'U'            TO PPROUTIN.
           MOVE 'D'            TO PPTPFILE.
           MOVE 'R'            TO PPTPOPER.
           MOVE 'SCBCB10'      TO PPNMFILE.
           PERFORM Z10-READPRIN
              THRU Z10-EX.

       C00-EX. EXIT.

      *--------------------------------------------------------------*
       C05-ARROTONDA-IMP.
      *--------------------------------------------------------------*

           COPY SRARR0.

       C05-EX. EXIT.

      *--------------------------------------------------------------*
       C10-ELABORA-DIFFE.
      *--------------------------------------------------------------*

           IF C861-CONTPROV = 'FONPER'
ELIO  *       IF SK-AGGIORNA = 'SI'
                 PERFORM E00-CERCA-CVLSCOPE
                    THRU E00-EX
                 IF WS-COMODIFF NOT GREATER 1 AND NOT LESS -1
                    MOVE WS-DIFFSCOP TO WS-CVLDIFFE
                    MOVE WS-TCVSCOPE TO WS-CVLMOVIM
                 ELSE
                    PERFORM M05-MESSDIFFE
                       THRU M05-EX
                    MOVE WS-CVSCANEW TO WS-CVLMOVIM
                 END-IF
ELIO  *       END-IF
              PERFORM D00-SISTEMA-TABEL
                 THRU D00-EX
              INITIALIZE                DCKYNUM
              MOVE C855-CODSOCIE     TO DCKY-CODSOCIE
              MOVE C855-CODCLIEN     TO DCKY-CODCLIEN
              MOVE 'C'               TO DCKY-TIPOPROC
              MOVE ' '               TO DCKY-TIPOMOVI
              MOVE 'S'               TO DCKY-CODIMOVI
              PERFORM L20-SCBNUM5
                 THRU L20-EX
              MOVE DCKY-NUMEAUTO (4:6) TO WS-PARTCONT
              PERFORM U00-UPDATE-MOVISCAD
                 THRU U00-EX
              MOVE C855-NRMOVCON       TO WS-NRMOVCON
              MOVE C855-DTREGMOV       TO WS-DTREGMOV
              MOVE C855-DTSCADEN       TO WS-DTSCADEN
              MOVE C855-OLDSCADE       TO WS-OLDSCADE
              MOVE C855-CDMOVCON       TO WS-CDMOVCON
              MOVE C852-NRMOVRIF       TO C855-NRMOVCON
                                          WS-NRMOVRIF
              PERFORM U00-UPDATE-MOVISCAD
                 THRU U00-EX
              PERFORM D05-STORNO-ORIGINALE
                 THRU D05-EX
              PERFORM D10-SALDO-A-ZERO
                 THRU D10-EX
              PERFORM D15-NUOVA-SCRITTURA
                 THRU D15-EX.

       C10-EX. EXIT.

      *--------------------------------------------------------------*
       C20-TESTATA.
      *--------------------------------------------------------------*

           MOVE TBST-CODSOCIE (WS-INDITABE) TO WS-CODSOCIE.
           MOVE '1'    TO WS-ASA
           STRING 'DIVISIONE - T - '
                  'SOCIETA'' ' WS-CODSOCIE ' ' WS-NOMSOCIE ' '
                  'RIEPILOGO DIFFERENZE CAMBI SU PERDITE AL '
                  WS-MAXIDATA (7:2) '/' WS-MAXIDATA (5:2) '/'
                  WS-MAXIDATA (1:4)
                  DELIMITED BY SIZE INTO WS-RIG.
           PERFORM W00-WRITE THRU W00-EX.

           PERFORM W00-WRITE THRU W00-EX 2 TIMES.

           STRING 'CLIENTE '
                  '--- RAGIONE SOCIALE -- '
                  'DIV '
                  '--IMPORTO--- '
                  '-CTVL.PRIMA- '
                  '-CTVL. DOPO- '
                  '----UTILE--- '
                  '---PERDITA-- '
                  'DIFF. CAMBIO '
                  'C/COSTO'
                  DELIMITED BY SIZE INTO WS-RIG.

           PERFORM W00-WRITE THRU W00-EX.

           PERFORM W00-WRITE THRU W00-EX.
           MOVE 7 TO WS-CRIGSTAM.

       C20-EX. EXIT.

      *--------------------------------------------------------------*
       C25-DETTAGLIO.
      *--------------------------------------------------------------*

           MOVE 'SI' TO WS-CENESONO.

           IF TBST-CODCLIEN (WS-INDITABE) NOT = A901-CODCLIEN
              MOVE TBST-CODCLIEN (WS-INDITABE) TO A901-CODCLIEN
              PERFORM L10-LEGGI-CLIENTE
                 THRU L10-EX.

           COMPUTE WS-TOCVLPRI = WS-TOCVLPRI +
                   TBST-CVLPRIMA (WS-INDITABE).
           COMPUTE WS-TOCVLDFP = WS-TOCVLDFP +
                   TBST-CVLDIFFP (WS-INDITABE).
           COMPUTE WS-TOCVLDFN = WS-TOCVLDFN +
                   TBST-CVLDIFFN (WS-INDITABE).
           COMPUTE WS-TOCVLDOP = WS-TOCVLDOP +
                   TBST-CVLDOPOO (WS-INDITABE).
           MOVE   TBST-CVLPRIMA (WS-INDITABE) TO WS-CVLPRIED.
           MOVE   TBST-CVLDOPOO (WS-INDITABE) TO WS-CVLDOPED.
           MOVE   TBST-IMPMOVIM (WS-INDITABE) TO WS-IMPORTED.
           MOVE   SPACES                      TO WS-CVLDFPEX
                                                 WS-CVLDFNEX.
           MOVE   TBST-CVLDIFFE (WS-INDITABE) TO WS-PEREDITT.

           IF TBST-CVLDIFFN (WS-INDITABE) NOT = ZEROES
              MOVE TBST-CVLDIFFN(WS-INDITABE) TO WS-CVLDFNED.

           IF TBST-CVLDIFFP (WS-INDITABE) NOT = ZEROES
              MOVE TBST-CVLDIFFP(WS-INDITABE) TO WS-CVLDFPED.

           STRING TBST-CODCLIEN (WS-INDITABE) ' '
                  A901-RAGFATTU ' '
                  TBST-SIGLADIV (WS-INDITABE) ' '
                  WS-IMPORTED                 ' '
                  WS-CVLPRIED                 ' '
                  WS-CVLDOPED                 ' '
                  WS-CVLDFPED                 '  '
                  WS-CVLDFNED                 '  '
                  WS-PEREDITT                 ' '
                  TBST-CENCOSTO (WS-INDITABE)
                  DELIMITED BY SIZE INTO WS-RIG.

           PERFORM W00-WRITE THRU W00-EX.
           COMPUTE WS-CRIGSTAM = WS-CRIGSTAM + 1.

       C25-EX. EXIT.

      *--------------------------------------------------------------*
       C30-TOTALI-SOCIETA.
      *--------------------------------------------------------------*

           IF WS-CENESONO = 'SI'
              PERFORM W00-WRITE THRU W00-EX 2 TIMES
              MOVE WS-DIFFEREN            TO WS-CVLTODIF
              MOVE WS-TOCVLPRI            TO WS-CVLTOPRI
              MOVE WS-TOCVLDOP            TO WS-CVLTODOP
              MOVE WS-TOCVLDFP            TO WS-CVLTODFP
              MOVE WS-TOCVLDFN            TO WS-CVLTODFN
              MOVE WS-RIGATOT             TO WS-RIG
              PERFORM W00-WRITE THRU W00-EX

              PERFORM D20-ORDITABE
                 THRU D20-EX UNTIL WS-SWORDINA = 'NO'
              PERFORM D25-STAMPA-TABCENCO
                 THRU D25-EX
              MOVE 1 TO WS-INDITABE
              PERFORM D30-CREA-GTM
                 THRU D30-EX
                UNTIL TBST-CODCLIEN (WS-INDITABE) = SPACES.

       C30-EX. EXIT.

      *--------------------------------------------------------------*
       D00-SISTEMA-TABEL.
      *--------------------------------------------------------------*

           MOVE ZEROES TO WS-CVLDIFFP
                          WS-CVLDIFFN.

           IF WS-CVLDIFFE GREATER ZEROES
              MOVE WS-CVLDIFFE TO WS-CVLDIFFP
           ELSE
              MOVE WS-CVLDIFFE TO WS-CVLDIFFN.

           PERFORM E05-RIPARTIZ-CENCOSTO
              THRU E05-EX.

           MOVE A531-SOCIEGTM TO WS-CODSOCIE.
           MOVE A531-NOMSOCIE TO WS-NOMSOCIE.
           MOVE A963-SIGLADIV TO WS-SIGLADIV.
           MOVE C855-CODCLIEN TO WS-CODCLIEN.
           PERFORM E10-AGG-TABESTAMPA
              THRU E10-EX
           VARYING WS-INDICLIE FROM 1 BY 1
             UNTIL TBCL-CENCOSTO(WS-INDICLIE) = SPACES.

       D00-EX. EXIT.

      *--------------------------------------------------------------*
       D05-STORNO-ORIGINALE.
      *--------------------------------------------------------------*

           PERFORM E15-CREA-MOVIMENTI
              THRU E15-EX.

           PERFORM E20-CREA-MOVISCAD
              THRU E20-EX.

           PERFORM E25-CREA-MOVIDCE
              THRU E25-EX.

       D05-EX. EXIT.

      *--------------------------------------------------------------*
       D10-SALDO-A-ZERO.
      *--------------------------------------------------------------*

           MOVE SK-CODSOCIE   TO C852-CODSOCIE.
           MOVE WS-CDMOVCON   TO C852-CDMOVCON.
           MOVE WS-DTREGMOV   TO C852-DTREGMOV.
           MOVE C855-CODCLIEN TO C852-CODCLIEN.
           MOVE WS-NRMOVCON TO C852-NRMOVCON.
           PERFORM L30-LEGGI-MOVIMENTI
              THRU L30-EX.
           MOVE 'S'         TO C852-CDMOVCON.
           MOVE 'X'         TO C852-CAUSACON.
           MOVE SPACES      TO C852-NRMOVRIF
           MOVE 'S'         TO C852-FLEXECON.
           MOVE WS-PARTCONT TO C852-NRMOVCON.
           MOVE WS-MAXIDATA TO C852-DTREGMOV
                               C852-DTULTVAR
                               C852-DATAVALU
                               C852-DTCONTAB
           MOVE 'AACH07'    TO C852-CODUTVAR
                               C852-CODUTCRE.
           PERFORM I00-INSERT-MOVIMENTI
              THRU I00-EX.
           MOVE WS-CDMOVCON   TO C855-CDMOVCON.
           MOVE WS-DTREGMOV   TO C855-DTREGMOV.
           MOVE WS-NRMOVCON   TO C855-NRMOVCON.
           MOVE WS-DTSCADEN   TO C855-DTSCADEN.
           MOVE WS-OLDSCADE   TO C855-OLDSCADE.
           PERFORM L35-LEGGI-MOVISCAD
              THRU L35-EX.
           MOVE 'S'           TO C855-FLEXECON.
           MOVE 'S'           TO C855-CDMOVCON.
           MOVE 'AACH07'      TO C855-CODUTVAR.
           MOVE C855-DTSCADEN TO C855-SCADORIG
           MOVE SPACES        TO C855-OLDSCADE.
           MOVE WS-MAXIDATA   TO C855-DTREGMOV
                                 C855-DTINCASS
                                 C855-DTULTVAR.
           MOVE WS-PARTCONT   TO C855-NRMOVCON
                                 C855-PARTCONT.
           MOVE '**N'         TO C855-BANCACON
           MOVE 'ZZ'          TO C855-CAUSBANC.
           MOVE 'Y'           TO C855-CDSELMOV.
           MOVE '-'           TO C855-INDSEGNO.
           MOVE 'X'           TO C855-CDSELTIP.
           MOVE 'S'           TO C855-FLAGSALD.
           MOVE ZEROES        TO C855-IMPMOVIM
                                 C855-CVLMOVIM
                                 C855-IMPORIVA
                                 C855-CVLXXIVA.
           PERFORM I05-INSERT-MOVISCAD
              THRU I05-EX.
           MOVE SK-CODSOCIE   TO C858-CODSOCIE.
           MOVE WS-NRMOVCON   TO C858-NRMOVCON.
           MOVE WS-CDMOVCON   TO C858-CDMOVCON.
           MOVE WS-DTREGMOV   TO C858-DTREGMOV.
           MOVE WS-NRMOVCON   TO C858-NRMOVCON.
           MOVE WS-DTSCADEN   TO C858-DTSCADEN.
           MOVE WS-OLDSCADE   TO C858-OLDSCADE.
           PERFORM L40-LEGGI-MOVIDCE
              THRU L40-EX.
           MOVE WS-MAXIDATA   TO C858-DTREGMOV
                                 C858-DTULTVAR.
           MOVE SPACES        TO C855-OLDSCADE.
           MOVE 'AACH07'      TO C858-CODUTVAR.
           MOVE WS-PARTCONT   TO C858-NRMOVCON.
           MOVE 'S'           TO C858-CDMOVCON.
           MOVE 'S'           TO C858-FLEXECON.
           MOVE '-'           TO C858-INDSEGNO.
           MOVE ZEROES        TO C858-IMPARMOV
                                 C858-CVLMOVIM
                                 C858-IMPADIVA
                                 C858-IMPORIVA
                                 C858-CVLXXIVA
                                 C858-CVLADIVA.
           MOVE WS-CTCASSAN   TO C858-CONTODCE.
           PERFORM I10-INSERT-MOVIDCE
              THRU I10-EX.

       D10-EX. EXIT.

      *--------------------------------------------------------------*
       D15-NUOVA-SCRITTURA.
      *--------------------------------------------------------------*

           MOVE WS-MAXIDATA TO C855-DTREGMOV.
           PERFORM E15-CREA-MOVIMENTI
              THRU E15-EX.

           PERFORM E30-CREA-MOVISCANEW
              THRU E30-EX.

           PERFORM E35-CREA-MOVIDCENEW
              THRU E35-EX.

       D15-EX. EXIT.

      *--------------------------------------------------------------*
       D20-ORDITABE.
      *--------------------------------------------------------------*

           MOVE 'NO' TO WS-SWORDINA.
           PERFORM VARYING WS-INDITABE FROM 1 BY 1
             UNTIL TBCC-CODSOCIE (WS-INDITABE + 1) = SPACES OR
                   WS-INDITABE GREATER 300
             IF TBCC-CENCOSTO (WS-INDITABE + 1) LESS
                TBCC-CENCOSTO (WS-INDITABE)
                MOVE 'SI' TO WS-SWORDINA
                MOVE ELEM-TABCECO (WS-INDITABE + 1) TO WS-SALVELEM
                MOVE ELEM-TABCECO (WS-INDITABE) TO
                     ELEM-TABCECO (WS-INDITABE + 1)
                MOVE WS-SALVELEM TO ELEM-TABCECO (WS-INDITABE)
             END-IF
           END-PERFORM.

       D20-EX. EXIT.

      *--------------------------------------------------------------*
       D25-STAMPA-TABCENCO.
      *--------------------------------------------------------------*

           PERFORM VARYING WS-INDITABE FROM 1 BY 1
             UNTIL TBCC-CENCOSTO(WS-INDITABE) = SPACES
                   COMPUTE TBCC-CVLDIFFE (WS-INDITABE) =
                           TBCC-CVLDIFFP (WS-INDITABE) -
                           TBCC-CVLDIFFN (WS-INDITABE)
           END-PERFORM.

           MOVE ZEROES TO WS-TOCVLPRI
                          WS-TOCVLDOP
                          WS-TOCVLDFP
                          WS-TOCVLDFN
                          WS-DIFFERCC.

           MOVE 60 TO WS-CRIGSTAM.
           PERFORM F10-STAMPA-RIGA-CENCOSTO
              THRU F10-EX
           VARYING WS-INDITABE FROM 1 BY 1
             UNTIL WS-INDITABE GREATER 300 OR
                   TBCC-CENCOSTO(WS-INDITABE) NOT GREATER SPACES.

           IF WS-CRIGSTAM > 57
              PERFORM G00-TESTATA-CENCOSTO
                 THRU G00-EX.


           PERFORM W00-WRITE THRU W00-EX.
           MOVE WS-TOCVLPRI            TO WS-CVLTOPRI.
           MOVE WS-DIFFERCC            TO WS-CVLTODIF.
           MOVE WS-TOCVLDOP            TO WS-CVLTODOP.
           MOVE WS-TOCVLDFP            TO WS-CVLTODFP.
           MOVE WS-TOCVLDFN            TO WS-CVLTODFN.
           STRING 'TOTALE' WS-CVLTOPRI ' ' WS-CVLTODOP ' '
                  WS-CVLTODFP ' ' WS-CVLTODFN '  '
                  WS-CVLTODIF ' ***'
                  DELIMITED BY SIZE INTO WS-RIG.
           PERFORM W00-WRITE THRU W00-EX.

       D25-EX. EXIT.

      *--------------------------------------------------------------*
       D30-CREA-GTM.
      *--------------------------------------------------------------*

           INITIALIZE                             TABCLIE.
           MOVE TBST-CODCLIEN(WS-INDITABE) TO WS-CODCLIEN.
           MOVE TBST-SIGLADIV(WS-INDITABE) TO WS-SIGLADIV.
           MOVE 1                          TO WS-INDICLIE.
           MOVE ZEROES                     TO WS-DIFFEREN.

           PERFORM VARYING WS-INDITABE FROM WS-INDITABE BY 1
             UNTIL TBST-CODCLIEN(WS-INDITABE) NOT = WS-CODCLIEN OR
                   TBST-SIGLADIV(WS-INDITABE) NOT = WS-SIGLADIV
              MOVE TBST-CENCOSTO(WS-INDITABE) TO
                   TBCL-CENCOSTO(WS-INDICLIE)
              MOVE TBST-CVLDIFFE(WS-INDITABE) TO
                   TBCL-CVLDIFFE(WS-INDICLIE)
              COMPUTE WS-DIFFEREN = WS-DIFFEREN +
                      TBST-CVLDIFFE(WS-INDITABE)
              COMPUTE WS-INDICLIE = WS-INDICLIE + 1
           END-PERFORM.

           IF WS-DIFFEREN NOT = ZEROES
              PERFORM E40-SCARICA-TABECLIE
                 THRU E40-EX.

       D30-EX. EXIT.

      *--------------------------------------------------------------*
       E00-CERCA-CVLSCOPE.
      *--------------------------------------------------------------*

           MOVE ZEROES TO WS-TCVSCOPE.

           EXEC SQL
                OPEN PCVLSCO
           END-EXEC.

           PERFORM L55-FETCH-SUMSCOPE
              THRU L55-EX.

           PERFORM UNTIL SQLCODE = +100
                   COMPUTE WS-TCVSCOPE = WS-TCVSCOPE + WS-CVLSCOPE
                   PERFORM L55-FETCH-SUMSCOPE
                      THRU L55-EX
           END-PERFORM.

           EXEC SQL
                CLOSE PCVLSCO
           END-EXEC.

           COMPUTE WS-DIFFSCOP = WS-CVLSCOLD - WS-TCVSCOPE.
           COMPUTE WS-COMODIFF = WS-TCVSCOPE - C855-CVLMOVIM.

       E00-EX. EXIT.

      *--------------------------------------------------------------*
       E05-RIPARTIZ-CENCOSTO.
      *--------------------------------------------------------------*

           INITIALIZE                TABCLIE.
           MOVE C855-CODCLIEN TO C200-CODCLIEN.
           MOVE C855-CODSOCIE TO C200-CODSOCIE.
           PERFORM L15-LEGGI-AARIFEMAX
              THRU L15-EX.

           IF C200-AARIFERI NOT GREATER SPACES
              PERFORM F15-CENCOS-MOVIM
                 THRU F15-EX
           ELSE
              PERFORM F20-CENCOS-SICOPL
                 THRU F20-EX.

           PERFORM VARYING WS-INDICLIE FROM 1 BY 1
                     UNTIL TBCL-CENCOSTO (WS-INDICLIE) = SPACES
                     PERFORM VARYING WS-INDITABE FROM 1 BY 1
                          UNTIL TBCC-CODSOCIE (WS-INDITABE) = SPACES OR
                                WS-INDITABE GREATER 300              OR
                                TBCL-CENCOSTO (WS-INDICLIE) =
                                TBCC-CENCOSTO (WS-INDITABE)
                     END-PERFORM
                     IF WS-INDITABE GREATER 300
                        MOVE 'RENATO : TABELLA TABCECO INSUFFICIENTE '
                             TO PPMESSAG
                        MOVE 'M' TO PPROUTIN
                        PERFORM Z10-READPRIN
                           THRU Z10-EX
                        MOVE 'TBCC' TO WCOM-CODABEND
                        PERFORM Z00-ERRORE
                           THRU Z00-EX
                     ELSE
                        PERFORM F30-SOMMA-IN-TABELLA
                           THRU F30-EX
                     END-IF
           END-PERFORM.

       E05-EX. EXIT.

      *--------------------------------------------------------------*
       E10-AGG-TABESTAMPA.
      *--------------------------------------------------------------*

           PERFORM VARYING WS-INDITABE FROM 1 BY 1
                     UNTIL TBST-CODSOCIE (WS-INDITABE) = SPACES OR
                           WS-INDITABE GREATER 3000             OR
                          (TBST-CODSOCIE (WS-INDITABE) = WS-CODSOCIE
                       AND TBST-CODCLIEN (WS-INDITABE) = WS-CODCLIEN
                       AND TBST-CENCOSTO (WS-INDITABE) =
                           TBCL-CENCOSTO (WS-INDICLIE)
                       AND TBST-SIGLADIV (WS-INDITABE) = WS-SIGLADIV)
           END-PERFORM.

           IF WS-INDITABE GREATER 3000
              MOVE 'RENATO : TABELLA TABSTAM INSUFFICIENTE '
                   TO PPMESSAG
              MOVE 'M' TO PPROUTIN
              PERFORM Z10-READPRIN
                 THRU Z10-EX
              MOVE 'TBST' TO WCOM-CODABEND
              PERFORM Z00-ERRORE
                 THRU Z00-EX
           ELSE
              MOVE WS-CODSOCIE   TO TBST-CODSOCIE (WS-INDITABE)
              MOVE WS-CODCLIEN   TO TBST-CODCLIEN (WS-INDITABE)
              MOVE WS-SIGLADIV   TO TBST-SIGLADIV (WS-INDITABE)
              MOVE TBCL-CENCOSTO(WS-INDICLIE) TO
                   TBST-CENCOSTO(WS-INDITABE)
              COMPUTE TBST-CVLPRIMA (WS-INDITABE) =
                      TBST-CVLPRIMA (WS-INDITABE) +
                      TBCL-CVLPRIMA (WS-INDICLIE)
              COMPUTE TBST-CVLDOPOO (WS-INDITABE) =
                      TBST-CVLDOPOO (WS-INDITABE) +
                      TBCL-CVLDOPOO (WS-INDICLIE)
              COMPUTE TBST-IMPMOVIM (WS-INDITABE) =
                      TBST-IMPMOVIM (WS-INDITABE) +
                      TBCL-IMPMOVIM (WS-INDICLIE)
              COMPUTE TBST-CVLDIFFP (WS-INDITABE) =
                      TBST-CVLDIFFP (WS-INDITABE) +
                      TBCL-CVLDIFFP (WS-INDICLIE)
              COMPUTE TBST-CVLDIFFN (WS-INDITABE) =
                      TBST-CVLDIFFN (WS-INDITABE) +
                      TBCL-CVLDIFFN (WS-INDICLIE).

       E10-EX. EXIT.

      *--------------------------------------------------------------*
       E15-CREA-MOVIMENTI.
      *--------------------------------------------------------------*

           MOVE SK-CODSOCIE   TO C852-CODSOCIE.
           MOVE WS-CDMOVCON   TO C852-CDMOVCON.
           MOVE WS-DTREGMOV   TO C852-DTREGMOV.
           MOVE C855-CODCLIEN TO C852-CODCLIEN.
           MOVE WS-NRMOVCON   TO C852-NRMOVCON.
           PERFORM L30-LEGGI-MOVIMENTI
              THRU L30-EX.
           INITIALIZE                DCKYNUM.
           MOVE C852-CODSOCIE     TO DCKY-CODSOCIE.
           MOVE C852-CODCLIEN     TO DCKY-CODCLIEN.
           MOVE 'C'               TO DCKY-TIPOPROC.
           MOVE ' '               TO DCKY-TIPOMOVI.
           MOVE 'D'               TO DCKY-CODIMOVI.
           PERFORM L20-SCBNUM5
              THRU L20-EX.
           MOVE DCKY-NUMEAUTO (4:6) TO WS-NRMOVRI1.
           INITIALIZE                DCKYNUM.
           MOVE C852-CODSOCIE     TO DCKY-CODSOCIE.
           MOVE C852-CODCLIEN     TO DCKY-CODCLIEN.
           MOVE 'C'               TO DCKY-TIPOPROC.
           MOVE ' '               TO DCKY-TIPOMOVI.
           MOVE 'D'               TO DCKY-CODIMOVI.
           PERFORM L20-SCBNUM5
              THRU L20-EX.
           MOVE DCKY-NUMEAUTO (4:6) TO WS-NRMOVRI2.

           MOVE 'S' TO C852-FLEXECON.
           MOVE WS-NRMOVRI1 TO C852-NRMOVCON.
           MOVE WS-NRMOVRI2 TO C852-NRMOVRIF.
           MOVE WS-MAXIDATA TO C852-DTREGMOV
                               C852-DTULTVAR.
           MOVE 'AACH07'    TO C852-CODUTVAR
                               C852-CODUTCRE.
           MOVE 'X'         TO C852-CAUSACON.
           PERFORM I00-INSERT-MOVIMENTI
              THRU I00-EX.
           MOVE WS-NRMOVRI2 TO C852-NRMOVCON.
           MOVE WS-NRMOVRI1 TO C852-NRMOVRIF.
           PERFORM I00-INSERT-MOVIMENTI
              THRU I00-EX.

       E15-EX. EXIT.

      *--------------------------------------------------------------*
       E20-CREA-MOVISCAD.
      *--------------------------------------------------------------*

           MOVE WS-NRMOVCON TO C855-NRMOVCON.
           MOVE WS-CDMOVCON TO C855-CDMOVCON.
           MOVE WS-DTREGMOV TO C855-DTREGMOV.
           MOVE WS-DTSCADEN TO C855-DTSCADEN.
           MOVE WS-OLDSCADE TO C855-OLDSCADE.
           PERFORM L35-LEGGI-MOVISCAD
              THRU L35-EX.
           MOVE WS-MAXIDATA TO C855-DTREGMOV
                               C855-DTULTVAR.
           MOVE 'AACH07'    TO C855-CODUTVAR.
           MOVE WS-NRMOVRI1 TO C855-NRMOVCON.
           MOVE WS-PARTCONT TO C855-PARTCONT.
           MOVE 'S'         TO C855-FLAGSALD.
           MOVE 'S'         TO C855-FLEXECON.
           MOVE 'X'         TO C855-CDSELTIP.
           MOVE 'J'         TO C855-CDSELMOV.
           STRING 'RIC.FONDO SVAL.CR.' WS-MAXIDATA
                  DELIMITED BY SIZE INTO C855-NOTENOTE.

           IF C855-INDSEGNO = '-'
              MOVE '+' TO C855-INDSEGNO
           ELSE
              MOVE '-' TO C855-INDSEGNO.

           PERFORM I05-INSERT-MOVISCAD
              THRU I05-EX.
           MOVE WS-NRMOVRIF TO C855-NRMOVCON.
           MOVE WS-CDMOVCON TO C855-CDMOVCON.
           MOVE WS-DTREGMOV TO C855-DTREGMOV.
           MOVE WS-OLDSCADE TO C855-OLDSCADE.
           MOVE WS-DTSCADEN TO C855-DTSCADEN.
           PERFORM L35-LEGGI-MOVISCAD
              THRU L35-EX.
           MOVE WS-MAXIDATA TO C855-DTREGMOV
                               C855-DTULTVAR.
           MOVE 'AACH07'    TO C855-CODUTVAR.
           MOVE WS-NRMOVRI2 TO C855-NRMOVCON.
           MOVE WS-PARTCONT TO C855-PARTCONT.
           MOVE 'S'         TO C855-FLAGSALD.
           MOVE 'S'         TO C855-FLEXECON.
           MOVE 'X'         TO C855-CDSELTIP.
           MOVE 'J'         TO C855-CDSELMOV.
           STRING 'RIC.FONDO SVAL.CR.' WS-MAXIDATA
                  DELIMITED BY SIZE INTO C855-NOTENOTE.

           IF C855-INDSEGNO = '-'
              MOVE '+' TO C855-INDSEGNO
           ELSE
              MOVE '-' TO C855-INDSEGNO.

           PERFORM I05-INSERT-MOVISCAD
              THRU I05-EX.

       E20-EX. EXIT.

      *--------------------------------------------------------------*
       E25-CREA-MOVIDCE.
      *--------------------------------------------------------------*

           MOVE SK-CODSOCIE   TO C858-CODSOCIE.
           MOVE WS-NRMOVCON   TO C858-NRMOVCON.
           MOVE WS-CDMOVCON   TO C858-CDMOVCON.
           MOVE WS-DTREGMOV   TO C858-DTREGMOV.
           MOVE WS-NRMOVCON   TO C858-NRMOVCON.
           MOVE WS-DTSCADEN   TO C858-DTSCADEN.
           MOVE WS-OLDSCADE   TO C858-OLDSCADE.
           PERFORM L40-LEGGI-MOVIDCE
              THRU L40-EX.
           MOVE WS-MAXIDATA TO C858-DTREGMOV
                               C858-DTULTVAR.
           MOVE 'AACH07'    TO C858-CODUTVAR.
           MOVE WS-NRMOVRI1 TO C858-NRMOVCON.
           MOVE 'S'         TO C858-FLEXECON.

           IF C858-INDSEGNO = '-'
              MOVE '+' TO C858-INDSEGNO
           ELSE
              MOVE '-' TO C858-INDSEGNO.

           PERFORM I10-INSERT-MOVIDCE
              THRU I10-EX.
           MOVE SK-CODSOCIE TO C858-CODSOCIE.
           MOVE WS-NRMOVRIF TO C858-NRMOVCON.
           MOVE WS-CDMOVCON TO C858-CDMOVCON.
           MOVE WS-DTREGMOV TO C858-DTREGMOV.
           MOVE WS-OLDSCADE TO C858-OLDSCADE.
           MOVE WS-DTSCADEN TO C858-DTSCADEN.
           PERFORM L40-LEGGI-MOVIDCE
              THRU L40-EX.
           MOVE WS-MAXIDATA TO C858-DTREGMOV
                               C858-DTULTVAR.
           MOVE 'AACH07'    TO C858-CODUTVAR.
           MOVE WS-NRMOVRI2 TO C858-NRMOVCON.
           MOVE WS-CTSTFOND TO C858-CONTODCE.
           MOVE 'S'         TO C858-FLEXECON.

           IF C858-INDSEGNO = '-'
              MOVE '+' TO C858-INDSEGNO
           ELSE
              MOVE '-' TO C858-INDSEGNO.

           PERFORM I10-INSERT-MOVIDCE
              THRU I10-EX.

       E25-EX. EXIT.

      *--------------------------------------------------------------*
       E30-CREA-MOVISCANEW.
      *--------------------------------------------------------------*

           MOVE WS-NRMOVCON TO C855-NRMOVCON.
           MOVE WS-DTREGMOV TO C855-DTREGMOV.
           MOVE WS-CDMOVCON TO C855-CDMOVCON.
           MOVE WS-DTSCADEN TO C855-DTSCADEN.
           MOVE WS-OLDSCADE TO C855-OLDSCADE.
           PERFORM L35-LEGGI-MOVISCAD
              THRU L35-EX.
           MOVE WS-MAXIDATA TO C855-DTREGMOV
                               C855-DTULTVAR.
           MOVE 'AACH07'    TO C855-CODUTVAR.
           MOVE WS-NRMOVRI1 TO C855-NRMOVCON.
           MOVE WS-CVLMOVIM TO C855-CVLMOVIM.
           MOVE SPACES      TO C855-PARTCONT.
           MOVE ' '         TO C855-FLAGSALD.
           MOVE 'S'         TO C855-FLEXECON.
           MOVE 'I'         TO C855-CDSELTIP.
           MOVE 'D'         TO C855-CDSELMOV.

           PERFORM I05-INSERT-MOVISCAD
              THRU I05-EX.
           MOVE WS-NRMOVRIF TO C855-NRMOVCON.
           MOVE WS-CDMOVCON TO C855-CDMOVCON.
           MOVE WS-DTREGMOV TO C855-DTREGMOV.
           MOVE WS-OLDSCADE TO C855-OLDSCADE.
           MOVE WS-DTSCADEN TO C855-DTSCADEN.
           PERFORM L35-LEGGI-MOVISCAD
              THRU L35-EX.
           MOVE WS-MAXIDATA TO C855-DTREGMOV
                               C855-DTULTVAR.
           MOVE 'AACH07'    TO C855-CODUTVAR.
           MOVE WS-NRMOVRI2 TO C855-NRMOVCON.
           MOVE WS-CVLMOVIM TO C855-CVLMOVIM.
           MOVE SPACES      TO C855-PARTCONT.
           MOVE ' '         TO C855-FLAGSALD.
           MOVE 'S'         TO C855-FLEXECON.
           MOVE 'I'         TO C855-CDSELTIP.
           MOVE 'J'         TO C855-CDSELMOV.

           PERFORM I05-INSERT-MOVISCAD
              THRU I05-EX.

       E30-EX. EXIT.

      *--------------------------------------------------------------*
       E35-CREA-MOVIDCENEW.
      *--------------------------------------------------------------*

           MOVE SK-CODSOCIE TO C858-CODSOCIE.
           MOVE WS-NRMOVCON TO C858-NRMOVCON.
           MOVE WS-CDMOVCON TO C858-CDMOVCON.
           MOVE WS-DTREGMOV TO C858-DTREGMOV.
           MOVE WS-NRMOVCON TO C858-NRMOVCON.
           MOVE WS-DTSCADEN TO C858-DTSCADEN.
           MOVE WS-OLDSCADE TO C858-OLDSCADE.
           PERFORM L40-LEGGI-MOVIDCE
              THRU L40-EX.
           MOVE C858-DTREGMOV TO WS-DTREGMOV.
           MOVE C858-DTSCADEN TO WS-DTSCADEN.
           MOVE C858-OLDSCADE TO WS-OLDSCADE.
           MOVE C858-CDMOVCON TO WS-CDMOVCON.
           MOVE WS-MAXIDATA TO C858-DTREGMOV
                               C858-DTULTVAR.
           MOVE 'AACH07'    TO C858-CODUTVAR.
           MOVE WS-NRMOVRI1 TO C858-NRMOVCON.
           MOVE WS-CVLMOVIM TO C858-CVLMOVIM.
           MOVE 'S'         TO C858-FLEXECON.

           PERFORM I10-INSERT-MOVIDCE
              THRU I10-EX.
           MOVE SK-CODSOCIE TO C858-CODSOCIE.
           MOVE WS-NRMOVRIF TO C858-NRMOVCON.
           MOVE WS-CDMOVCON TO C858-CDMOVCON.
           MOVE WS-DTREGMOV TO C858-DTREGMOV.
           MOVE WS-OLDSCADE TO C858-OLDSCADE.
           MOVE WS-DTSCADEN TO C858-DTSCADEN.
           PERFORM L40-LEGGI-MOVIDCE
              THRU L40-EX.
           MOVE WS-MAXIDATA TO C858-DTREGMOV
                               C858-DTULTVAR.
           MOVE 'AACH07'    TO C858-CODUTVAR.
           MOVE WS-NRMOVRI2 TO C858-NRMOVCON.
           MOVE WS-CVLMOVIM TO C858-CVLMOVIM.
           MOVE 'S'         TO C858-FLEXECON.
           PERFORM I10-INSERT-MOVIDCE
              THRU I10-EX.

       E35-EX. EXIT.

      *--------------------------------------------------------------*
       E40-SCARICA-TABECLIE.
      *--------------------------------------------------------------*

           INITIALIZE FINA-RECORDXX.
           MOVE A531-SOCIEGTM TO FINA-CODSOCIE.
           MOVE 'T'           TO FINA-DIVISION.
           IF FINA-CODSOCIE = '22' OR 'BS'
              MOVE 'A'        TO FINA-DIVISION.
           IF FINA-CODSOCIE = 'EG' OR 'DQ'
              MOVE 'V'        TO FINA-DIVISION.

           MOVE 'C'           TO FINA-CODICECL.
           MOVE 'S'           TO FINA-ACQUVEND
                                 FINA-TIPODOCU.
           MOVE ZEROES      TO FINA-NRDOCUME
           MOVE WS-CODCLIEN TO FINA-CLIECONS.
           MOVE WS-MAXIDATA (3:2)   TO FINA-DTDOCUAA.
           MOVE WS-MAXIDATA (5:2)   TO FINA-DTDOCUMM.
           MOVE WS-MAXIDATA (7:2)   TO FINA-DTDOCUGG.
           MOVE FINA-DTDOCUAA (2:1) TO FINA-COMPETAA.
           MOVE '13'                TO FINA-COMPETMM.
           MOVE '1'                 TO FINA-TIPORECO.
           MOVE 'E'                 TO FINA-ESTEINTE.
           MOVE 'SR  '              TO FINA-CODIMOVI.
           MOVE 'SVAL'              TO FINA-CODIMOVI.
           MOVE WS-SIGLADIV         TO FINA-SIGLADIV.
           MOVE WS-DIFFEREN         TO WS-DIFFEDEC.

           IF WS-DIFFEREN LESS ZERO
              MOVE '-'              TO FINA-INDSEGN1
           ELSE
              MOVE '+'                 TO FINA-INDSEGN1.

           IF WS-DECIDIVI = '0'
              MOVE WS-DIFFEINT      TO FINA-CVLMOVIM
           ELSE
              MOVE WS-DIFFEXXX      TO FINA-CVLMOVIM.

           STRING 'RICALC. CREDITI '  SK-CODSOCIE ' ' WS-MAXIDATA
                  DELIMITED BY SIZE INTO FINA-RAGFATTU.
           MOVE 'XXX'                 TO FINA-INDFATTU
                                         FINA-CITFATTU.

           MOVE FINA-CLIECONS       TO A901-CODCLIEN
           PERFORM L10-LEGGI-CLIENTE
              THRU L10-EX.
           MOVE A901-PAESFATT       TO FINA-PAESCLIE.
           IF FINA-PAESCLIE = '390'
              MOVE '388'            TO FINA-PAESCLIE.
           IF FINA-PAESCLIE = '000'
              MOVE '005'            TO FINA-PAESCLIE.

           PERFORM S00-SCRIVI-FILEOUT
              THRU S00-EX.
           INITIALIZE FINA-RECORD02.
           MOVE '2'                 TO FINA-TIPORECO.
           MOVE 'CLI.TESS.DIFF.FM'  TO FINA-DESCRIZ2.
           MOVE 'DIFCAM'            TO FINA-CONTODCE.
           PERFORM F00-CREA-RECORD-DUE
              THRU F00-EX
           VARYING WS-INDICLIE FROM 1 BY 1
             UNTIL WS-INDICLIE GREATER 30 OR
                   TBCL-CENCOSTO(WS-INDICLIE) NOT GREATER SPACES.

       E40-EX. EXIT.

      *--------------------------------------------------------------*
       F00-CREA-RECORD-DUE.
      *--------------------------------------------------------------*

           IF TBCL-CVLDIFFE (WS-INDICLIE) NOT EQUAL ZEROES
              MOVE TBCL-CENCOSTO (WS-INDICLIE) TO FINA-CENCOSTO
CLA           IF SK-CODSOCIE = 'BS'
                 MOVE 'AWSAL'                  TO FINA-CENCOSTO
CLA           END-IF
              MOVE TBCL-CVLDIFFE (WS-INDICLIE) TO WS-DIFFERCC
              MOVE WS-DIFFERCC TO WS-DIFFEDEC
                                  WS-CVLDIFED
              IF WS-DECIDIVI = '0'
                 MOVE WS-DIFFEINT      TO FINA-CVLDIVIS
                 MOVE ZEROES           TO FINA-IMPODIVI
              ELSE
                 MOVE WS-DIFFEXXX      TO FINA-CVLDIVIS
                 MOVE ZEROES           TO FINA-IMPODIVI
              END-IF
              IF WS-DIFFERCC LESS ZERO
                 MOVE '+'           TO FINA-INDSEGN2
              ELSE
                 MOVE '-'           TO FINA-INDSEGN2
              END-IF
              PERFORM S00-SCRIVI-FILEOUT
                 THRU S00-EX.

       F00-EX. EXIT.

      *--------------------------------------------------------------*
       F10-STAMPA-RIGA-CENCOSTO.
      *--------------------------------------------------------------*

           IF WS-CRIGSTAM > 57
              PERFORM G00-TESTATA-CENCOSTO
                 THRU G00-EX.

           COMPUTE WS-TOCVLPRI = WS-TOCVLPRI +
                                 TBCC-CVLPRIMA (WS-INDITABE).
           COMPUTE WS-TOCVLDOP = WS-TOCVLDOP +
                                 TBCC-CVLDOPOO (WS-INDITABE).
           COMPUTE WS-TOCVLDFN = WS-TOCVLDFN +
                                 TBCC-CVLDIFFN (WS-INDITABE).
           COMPUTE WS-TOCVLDFP = WS-TOCVLDFP +
                                 TBCC-CVLDIFFP (WS-INDITABE).
           COMPUTE WS-DIFFERCC = WS-DIFFERCC +
                                 TBCC-CVLDIFFE (WS-INDITABE).
           MOVE   TBCC-CVLPRIMA (WS-INDITABE) TO WS-CVLPRIED.
           MOVE   TBCC-CVLDOPOO (WS-INDITABE) TO WS-CVLDOPED.
           MOVE   TBCC-CVLDIFFE (WS-INDITABE) TO WS-PEREDITT.
           MOVE   SPACES                      TO WS-CVLDFPEX
                                                 WS-CVLDFNEX.

           IF TBCC-CVLDIFFN (WS-INDITABE) NOT = ZEROES
              MOVE TBCC-CVLDIFFN(WS-INDITABE) TO WS-CVLDFNED.

           IF TBCC-CVLDIFFP (WS-INDITABE) NOT = ZEROES
              MOVE TBCC-CVLDIFFP(WS-INDITABE) TO WS-CVLDFPED.

           STRING TBCC-CENCOSTO(WS-INDITABE)  ' '
                  WS-CVLPRIED                 ' '
                  WS-CVLDOPED                 ' '
                  WS-CVLDFPED                 '  '
                  WS-CVLDFNED                 '  '
                  WS-PEREDITT                 ' '
                  DELIMITED BY SIZE INTO WS-RIG.

           PERFORM W00-WRITE THRU W00-EX.

           COMPUTE WS-CRIGSTAM = WS-CRIGSTAM + 1.

       F10-EX. EXIT.

      *--------------------------------------------------------------*
       F15-CENCOS-MOVIM.
      *--------------------------------------------------------------*

           MOVE C852-CENCOSTO TO WS-CENCOSTO.

           IF WS-CENCOSTO NOT GREATER SPACES
              PERFORM L25-CERCA-CENCOSTO
                 THRU L25-EX.

           IF WS-CENCOSTO NOT GREATER SPACES
              STRING 'RENATO : NON TROVO C/COSTO '
                     WS-CODSOCIE ' ' WS-CODCLIEN ' ' C855-CDMOVCON ' '
                     C855-DTREGMOV ' ' C855-NRMOVCON ' '
                     DELIMITED BY SIZE INTO PPMESSAG
              MOVE 'M' TO PPROUTIN
              PERFORM Z10-READPRIN
                 THRU Z10-EX
              MOVE 'NOCC' TO WCOM-CODABEND
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

           MOVE WS-CENCOSTO   TO TBCL-CENCOSTO(1).
           PERFORM G10-UNCENCOSTO
              THRU G10-EX.

       F15-EX. EXIT.

      *--------------------------------------------------------------*
       F20-CENCOS-SICOPL.
      *--------------------------------------------------------------*

           PERFORM L45-LEGGI-SICONOOK
              THRU L45-EX.

           IF SQLCODE NOT = +100
              PERFORM L50-LEGGI-SICOMAXAN
                 THRU L50-EX
              MOVE CCOS-CENCOCOM TO TBCL-CENCOSTO(1)
              PERFORM G10-UNCENCOSTO
                 THRU G10-EX
           ELSE
              PERFORM G15-APPLICA-INSCOCON
                 THRU G15-EX.

       F20-EX. EXIT.

      *--------------------------------------------------------------*
       F30-SOMMA-IN-TABELLA.
      *--------------------------------------------------------------*

           MOVE WS-CODSOCIE TO TBCC-CODSOCIE (WS-INDITABE).
           MOVE TBCL-CENCOSTO (WS-INDICLIE) TO
                TBCC-CENCOSTO (WS-INDITABE).
           COMPUTE TBCC-CVLPRIMA (WS-INDITABE) =
                   TBCC-CVLPRIMA (WS-INDITABE) +
                   TBCL-CVLPRIMA (WS-INDICLIE).
           COMPUTE TBCC-CVLDOPOO (WS-INDITABE) =
                   TBCC-CVLDOPOO (WS-INDITABE) +
                   TBCL-CVLDOPOO (WS-INDICLIE).
           COMPUTE TBCC-CVLDIFFP (WS-INDITABE) =
                   TBCC-CVLDIFFP (WS-INDITABE) +
                   TBCL-CVLDIFFP (WS-INDICLIE).
           COMPUTE TBCC-CVLDIFFN (WS-INDITABE) =
                   TBCC-CVLDIFFN (WS-INDITABE) +
                   TBCL-CVLDIFFN (WS-INDICLIE).

       F30-EX. EXIT.

      *--------------------------------------------------------------*
       G00-TESTATA-CENCOSTO.
      *--------------------------------------------------------------*
           MOVE '1' TO WS-ASA
           STRING 'DIVISIONE - T - '
                  'SOCIETA'' ' WS-CODSOCIE ' ' WS-NOMSOCIE ' '
                  'RIEP. DIFFERENZE CAMBI SU PERDITE AL '
                  WS-MAXIDATA (7:2) '/' WS-MAXIDATA (5:2) '/'
                  WS-MAXIDATA (1:4) ' PER CENTRO DI COSTO'
                  DELIMITED BY SIZE INTO WS-RIG.
           PERFORM W00-WRITE THRU W00-EX.

           PERFORM W00-WRITE THRU W00-EX.

           STRING 'C/COS '
                  '-CTVL.PRIMA- '
                  '-CTVL. DOPO- '
                  '----UTILE--- '
                  '---PERDITA-- '
                  'DIFF. CAMBIO '
                  DELIMITED BY SIZE INTO WS-RIG.
           PERFORM W00-WRITE THRU W00-EX.

           PERFORM W00-WRITE THRU W00-EX.
           MOVE 5      TO WS-CRIGSTAM.

       G00-EX. EXIT.

      *--------------------------------------------------------------*
       G10-UNCENCOSTO.
      *--------------------------------------------------------------*
           IF C855-INDSEGNO = '+'
              COMPUTE TBCL-CVLPRIMA (1) =
                      TBCL-CVLPRIMA (1) + WS-CVLSCOLD
              COMPUTE TBCL-CVLDOPOO (1) =
                      TBCL-CVLDOPOO (1) + WS-CVLMOVIM
              COMPUTE TBCL-IMPMOVIM (1) =
                      TBCL-IMPMOVIM (1) + C855-IMPMOVIM
           ELSE
              COMPUTE TBCL-CVLPRIMA (1) =
                      TBCL-CVLPRIMA (1) - WS-CVLSCOLD
              COMPUTE TBCL-CVLDOPOO (1) =
                      TBCL-CVLDOPOO (1) - WS-CVLMOVIM
              COMPUTE TBCL-IMPMOVIM (1) =
                      TBCL-IMPMOVIM (1) - C855-IMPMOVIM.

           MOVE WS-CVLDIFFN TO TBCL-CVLDIFFN (1).
           MOVE WS-CVLDIFFP TO TBCL-CVLDIFFP (1).

       G10-EX. EXIT.

      *--------------------------------------------------------------*
       G15-APPLICA-INSCOCON.
      *--------------------------------------------------------------*

           MOVE ZEROES TO WS-TCVLPRIM
                          WS-TCVLDOPO
                          WS-TIMPORTO.

           EXEC SQL
                OPEN PSICOPL
           END-EXEC.

           PERFORM L60-FETCH-PSICOPL
              THRU L60-EX.

           IF SQLCODE = +0
              MOVE ZEROES TO WS-INSCOCON
              PERFORM H00-ESEGUI-CALCOPER
                 THRU H00-EX
              VARYING WS-INDICLIE FROM 1 BY 1
                UNTIL SQLCODE = +100 OR
                      WS-INDICLIE > 30.

           IF WS-INDICLIE > 30
              MOVE 'RENATO : TABELLA TABCLIE INSUFFICIENTE '
                   TO PPMESSAG
              MOVE 'M' TO PPROUTIN
              PERFORM Z10-READPRIN
                 THRU Z10-EX
              MOVE 'TBCC' TO WCOM-CODABEND
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

           EXEC SQL
                CLOSE PSICOPL
           END-EXEC.

           PERFORM H05-ELABORA-DIFFERENZE
              THRU H05-EX.

           PERFORM VARYING WS-INDICLIE FROM 1 BY 1
             UNTIL TBCL-PROLOMER(WS-INDICLIE) = SPACES
                   PERFORM H10-CERCA-CENCOSTO
                      THRU H10-EX
                   COMPUTE WS-COMDIFFE = TBCL-CVLPRIMA(WS-INDICLIE) -
                                         TBCL-CVLDOPOO(WS-INDICLIE)
                   IF WS-CVLDIFFE GREATER ZEROES
                      MOVE WS-COMDIFFE TO TBCL-CVLDIFFP(WS-INDICLIE)
                   ELSE
                      MOVE WS-COMDIFFE TO TBCL-CVLDIFFN(WS-INDICLIE)
                   END-IF
           END-PERFORM.

       G15-EX. EXIT.

      *--------------------------------------------------------------*
       I00-INSERT-MOVIMENTI.
      *--------------------------------------------------------------*

           MOVE 'I00-'                 TO WCOM-CODABEND.
           MOVE 'INSERT'               TO WCOM-FILEFUNZ.
           MOVE 'MOVIMEN'              TO WCOM-FILENOME.
           STRING C852-CODSOCIE ' ' C852-CDMOVCON ' '
                  C852-NRMOVCON ' ' C852-DTREGMOV ' '
                  C852-CODCLIEN ' '
                  DELIMITED BY SIZE  INTO WCOM-FILECTRL.

           EXEC SQL
                INSERT INTO DCONTCLI.TMOVIMENTI
                       (CODSOCIE,CODCLIEN,CDMOVCON,
                        NRMOVCON,DTREGMOV,DATAVALU,
                        CENCOSTO,TIPOAGEN,CODAGENT,
                        FLPROVPA,CAUSACON,NRMOVRIF,
                        FLGRATEO,FLEXECON,BANDIVAL,
                        NRDICVAL,DTULTVAR,NRMOVRI2,
                        IMPROFAT,FLGSTORN,DTCONTAB,
                        TIPOMOVI,VOCEDOPR,IMPDOGPR,
                        CODUTVAR,ITAESTER,CODUTCRE)
                VALUES (:C852-CODSOCIE,:C852-CODCLIEN,:C852-CDMOVCON,
                        :C852-NRMOVCON,:C852-DTREGMOV,:C852-DATAVALU,
                        :C852-CENCOSTO,:C852-TIPOAGEN,:C852-CODAGENT,
                        :C852-FLPROVPA,:C852-CAUSACON,:C852-NRMOVRIF,
                        :C852-FLGRATEO,:C852-FLEXECON,:C852-BANDIVAL,
                        :C852-NRDICVAL,:C852-DTULTVAR,:C852-NRMOVRI2,
                        :C852-IMPROFAT,:C852-FLGSTORN,:C852-DTCONTAB,
                        :C852-TIPOMOVI,:C852-VOCEDOPR,:C852-IMPDOGPR,
                        :C852-CODUTVAR,:C852-ITAESTER,:C852-CODUTCRE)
           END-EXEC.

           IF SQLCODE = ZERO
              MOVE 'U'            TO PPROUTIN
              MOVE 'D'            TO PPTPFILE
              MOVE 'A'            TO PPTPOPER
              MOVE 'MOVIMEN'      TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX
           ELSE
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

       I00-EX. EXIT.

      *--------------------------------------------------------------*
       I05-INSERT-MOVISCAD.
      *--------------------------------------------------------------*

           MOVE 'I05-' TO WCOM-CODABEND.
           MOVE 'INSERT' TO WCOM-FILEFUNZ.
           MOVE 'MOVISCA' TO WCOM-FILENOME.
           STRING C855-CODSOCIE ' ' C855-CDMOVCON ' '
                  C855-NRMOVCON ' ' C855-DTREGMOV ' '
                  C855-CODCLIEN ' '
                  DELIMITED BY SIZE INTO WCOM-FILECTRL.

           EXEC SQL
                INSERT INTO DCONTCLI.TMOVISCAD
                      (FLAGSTOR,CODSOCIE,CODCLIEN,
                       CDMOVCON,NRMOVCON,DTREGMOV,
                       DTSCADEN,TIPOPAGA,DTDECORR,
                       IMPMOVIM,INDSEGNO,CVLMOVIM,
                       IMPORIVA,SCONTDED,PARTCONT,
                       BANCAPPO,DISTICON,BANCACON,
                       AACONTAB,NRCONTAB,NOTENOTE,
                       FLAGSALD,NETTSCON,PLUSSCAD,
                       SIMPROVV,CDSELTIP,CDSELMOV,
                       DTULTVAR,FLEXECON,OLDSCADE,
                       IMPROFAT,DATADIST,DTINCASS,
                       FLAGTRAT,TIPOSOLL,CAUSBANC,
                       CODDIVIS,CVLXXIVA,SCADORIG,
                       NUMGGMED,CODUTVAR)
                VALUES (:C855-FLAGSTOR,:C855-CODSOCIE,:C855-CODCLIEN,
                        :C855-CDMOVCON,:C855-NRMOVCON,:C855-DTREGMOV,
                        :C855-DTSCADEN,:C855-TIPOPAGA,:C855-DTDECORR,
                        :C855-IMPMOVIM,:C855-INDSEGNO,:C855-CVLMOVIM,
                        :C855-IMPORIVA,:C855-SCONTDED,:C855-PARTCONT,
                        :C855-BANCAPPO,:C855-DISTICON,:C855-BANCACON,
                        :C855-AACONTAB,:C855-NRCONTAB,:C855-NOTENOTE,
                        :C855-FLAGSALD,:C855-NETTSCON,:C855-PLUSSCAD,
                        :C855-SIMPROVV,:C855-CDSELTIP,:C855-CDSELMOV,
                        :C855-DTULTVAR,:C855-FLEXECON,:C855-OLDSCADE,
                        :C855-IMPROFAT,:C855-DATADIST,:C855-DTINCASS,
                        :C855-FLAGTRAT,:C855-TIPOSOLL,:C855-CAUSBANC,
                        :C855-CODDIVIS,:C855-CVLXXIVA,:C855-SCADORIG,
                        :C855-NUMGGMED,:C855-CODUTVAR)
           END-EXEC.

           IF SQLCODE = ZERO
              MOVE 'U'            TO PPROUTIN
              MOVE 'D'            TO PPTPFILE
              MOVE 'A'            TO PPTPOPER
              MOVE 'MOVISCA'      TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX
           ELSE
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

       I05-EX. EXIT.

      *--------------------------------------------------------------*
       I10-INSERT-MOVIDCE.
      *--------------------------------------------------------------*

           MOVE 'I10-' TO WCOM-CODABEND.
           MOVE 'INSERT' TO WCOM-FILEFUNZ.
           MOVE 'MOVIDCE' TO WCOM-FILENOME.
           STRING C858-CODSOCIE ' ' C858-CDMOVCON ' '
                  C858-NRMOVCON ' ' C858-DTREGMOV ' '
                  C858-CODCLIEN ' '
                  DELIMITED BY SIZE INTO WCOM-FILECTRL.

           EXEC SQL
                INSERT INTO DCONTCLI.TMOVIDCE
                      (CODSOCIE,CODCLIEN,CDMOVCON,
                       NRMOVCON,DTREGMOV,DTSCADEN,
                       CONTODCE,IMPARMOV,INDSEGNO,
                       CVLMOVIM,IMPORIVA,PERCEIVA,
                       ESENTIVA,IMPADIVA,ADDFATTU,
                       FLEXECON,DTULTVAR,OLDSCADE,
                       CVLXXIVA,CVLADIVA,CODUTVAR)
                VALUES (:C858-CODSOCIE,:C858-CODCLIEN,:C858-CDMOVCON,
                        :C858-NRMOVCON,:C858-DTREGMOV,:C858-DTSCADEN,
                        :C858-CONTODCE,:C858-IMPARMOV,:C858-INDSEGNO,
                        :C858-CVLMOVIM,:C858-IMPORIVA,:C858-PERCEIVA,
                        :C858-ESENTIVA,:C858-IMPADIVA,:C858-ADDFATTU,
                        :C858-FLEXECON,:C858-DTULTVAR,:C858-OLDSCADE,
                        :C858-CVLXXIVA,:C858-CVLADIVA,:C858-CODUTVAR)
           END-EXEC.

           IF SQLCODE = ZERO
              MOVE 'U'            TO PPROUTIN
              MOVE 'D'            TO PPTPFILE
              MOVE 'A'            TO PPTPOPER
              MOVE 'MOVIDCE'      TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX
           ELSE
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

       I10-EX. EXIT.

      *--------------------------------------------------------------*
       H00-ESEGUI-CALCOPER.
      *--------------------------------------------------------------*

           COMPUTE WS-PARCVLPR = (WS-CVLSCOLD   * C200-INSCOCON) / 100.
           COMPUTE WS-PARCVLDO = (WS-CVLMOVIM * C200-INSCOCON) / 100.
           COMPUTE WS-PARIMPOR = (C855-IMPMOVIM * C200-INSCOCON) / 100.
           COMPUTE WS-TCVLPRIM = WS-TCVLPRIM + WS-PARCVLPR.
           COMPUTE WS-TCVLDOPO = WS-TCVLDOPO + WS-PARCVLDO.
           COMPUTE WS-TIMPORTO = WS-TIMPORTO + WS-PARIMPOR.
           MOVE C200-PROLOMER TO TBCL-PROLOMER(WS-INDICLIE).
           MOVE C200-CODLINEA TO TBCL-CODLINEA(WS-INDICLIE).
           MOVE C200-TIPOSTAG TO TBCL-TIPOSTAG(WS-INDICLIE).
           MOVE C200-SECSCELT TO TBCL-SECSCELT(WS-INDICLIE).
           MOVE C200-FLASTOCK TO TBCL-FLASTOCK(WS-INDICLIE).

           IF C200-INSCOCON GREATER WS-INSCOCON
              MOVE C200-INSCOCON TO WS-INSCOCON
              MOVE WS-INDICLIE   TO WS-INDICMAX.

           IF C855-INDSEGNO = '+'
              COMPUTE TBCL-CVLPRIMA (WS-INDICLIE) =
                      TBCL-CVLPRIMA (WS-INDICLIE) + WS-PARCVLPR
              COMPUTE TBCL-CVLDOPOO (WS-INDICLIE) =
                      TBCL-CVLDOPOO (WS-INDICLIE) + WS-PARCVLDO
              COMPUTE TBCL-IMPMOVIM (WS-INDICLIE) =
                      TBCL-IMPMOVIM (WS-INDICLIE) + WS-PARIMPOR
           ELSE
              COMPUTE TBCL-CVLPRIMA (WS-INDICLIE) =
                      TBCL-CVLPRIMA (WS-INDICLIE) - WS-PARCVLPR
              COMPUTE TBCL-CVLDOPOO (WS-INDICLIE) =
                      TBCL-CVLDOPOO (WS-INDICLIE) - WS-PARCVLDO
              COMPUTE TBCL-IMPMOVIM (WS-INDICLIE) =
                      TBCL-IMPMOVIM (WS-INDICLIE) - WS-PARIMPOR.

           PERFORM L60-FETCH-PSICOPL
              THRU L60-EX.

       H00-EX. EXIT.

      *--------------------------------------------------------------*
       H05-ELABORA-DIFFERENZE.
      *--------------------------------------------------------------*

           COMPUTE WS-TCVLPRIM = WS-CVLSCOLD   - WS-TCVLPRIM.
           COMPUTE WS-TCVLDOPO = WS-CVLMOVIM   - WS-TCVLDOPO.
           COMPUTE WS-TIMPORTO = C855-IMPMOVIM - WS-TIMPORTO.

           IF C855-INDSEGNO = '+'
              COMPUTE TBCL-CVLPRIMA (WS-INDICMAX) =
                      TBCL-CVLPRIMA (WS-INDICMAX) + WS-TCVLPRIM
              COMPUTE TBCL-CVLDOPOO (WS-INDICMAX) =
                      TBCL-CVLDOPOO (WS-INDICMAX) + WS-TCVLDOPO
              COMPUTE TBCL-IMPMOVIM (WS-INDICMAX) =
                      TBCL-IMPMOVIM (WS-INDICMAX) + WS-TIMPORTO
           ELSE
              COMPUTE TBCL-CVLPRIMA (WS-INDICMAX) =
                      TBCL-CVLPRIMA (WS-INDICMAX) - WS-TCVLPRIM
              COMPUTE TBCL-CVLDOPOO (WS-INDICMAX) =
                      TBCL-CVLDOPOO (WS-INDICMAX) - WS-TCVLDOPO
              COMPUTE TBCL-IMPMOVIM (WS-INDICMAX) =
                      TBCL-IMPMOVIM (WS-INDICMAX) - WS-TIMPORTO.

       H05-EX. EXIT.

      *--------------------------------------------------------------*
       H10-CERCA-CENCOSTO.
      *--------------------------------------------------------------*

           INITIALIZE                         DCWCCOS
           MOVE C200-CODCLIEN              TO CCOS-CODCLIEN
           MOVE TBCL-CODLINEA(WS-INDICLIE) TO CCOS-CODLINEA
           MOVE TBCL-PROLOMER(WS-INDICLIE) TO CCOS-PROLOMER

           MOVE TBCL-TIPOSTAG(WS-INDICLIE) TO CCOS-TIPOSTAG.

           IF TBCL-SECSCELT(WS-INDICLIE) = '3' OR 'D'
              MOVE 'D'                     TO CCOS-FLREGDIF
           ELSE
              IF TBCL-FLASTOCK(WS-INDICLIE) = 'S'
                 MOVE 'S'                  TO CCOS-FLREGDIF.

           MOVE 'SCBCCOS'                  TO PPCALL
           CALL PPCALL USING DCPARM DCWCCOS.

           IF CCOS-SQLCODE = +100
              INITIALIZE                      DCWCCOS
              MOVE C200-CODCLIEN           TO CCOS-CODCLIEN
              MOVE 'SCBCCOS'               TO PPCALL
              CALL PPCALL USING DCPARM DCWCCOS.

           IF CCOS-SQLCODE = +100
               STRING 'RENATO: MANCA C/COSTO X CHIAVE CCOS : '
                       DCWCCOS
                  DELIMITED BY SIZE INTO PPMESSAG
               MOVE 'M'                   TO PPROUTIN
               PERFORM Z10-READPRIN
                  THRU Z10-EX.

          MOVE CCOS-CENCOCOM           TO TBCL-CENCOSTO(WS-INDICLIE).

       H10-EX. EXIT.

      *--------------------------------------------------------------*
       L00-FETCH-PMOVSCA.
      *--------------------------------------------------------------*

           MOVE 'L00-'                  TO WCOM-CODABEND.
           MOVE 'FETCH'                 TO WCOM-FILEFUNZ.
           MOVE 'PMOVSCA'               TO WCOM-FILENOME.

           EXEC SQL
                FETCH PMOVSCA
                 INTO  :C855-CODSOCIE,:C855-CODDIVIS,:C855-CODCLIEN,
                       :C855-CDMOVCON,:C855-DTREGMOV,:C855-NRMOVCON,
                       :C855-CVLMOVIM,:C855-IMPMOVIM,:C855-DTSCADEN,
                       :C855-OLDSCADE,:C855-IMPORIVA,:C855-INDSEGNO,
                       :C855-CVLXXIVA,:A963-SIGLADIV,:C852-NRMOVRIF,
                       :C852-CENCOSTO,:C861-CONTPROV
           END-EXEC.

           IF SQLCODE = ZERO
              MOVE 'U'            TO PPROUTIN
              MOVE 'D'            TO PPTPFILE
              MOVE 'R'            TO PPTPOPER
              MOVE 'PMOVSCA'      TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX.

       L00-EX. EXIT.

      *----------------------------------------------------------------*
       L05-LEGGI-CONTIDCE.
      *----------------------------------------------------------------*

           MOVE 'L05-' TO WCOM-CODABEND.
           MOVE 'SELECT' TO WCOM-FILEFUNZ.
           MOVE 'CONTIDC' TO WCOM-FILENOME.
           STRING C861-CODPIANO ' ' C861-CONTPROV
                  DELIMITED BY SIZE INTO WCOM-FILECTRL.

           EXEC SQL
                SELECT CONTODCE
                  INTO :C861-CONTODCE
                  FROM DCONTCLI.TCONTIDCE
                 WHERE CONTPROV = :C861-CONTPROV
                   AND CODPIANO = :C861-CODPIANO
           END-EXEC.

           IF SQLCODE NOT = +0
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

           MOVE 'U'            TO PPROUTIN.
           MOVE 'D'            TO PPTPFILE.
           MOVE 'R'            TO PPTPOPER.
           MOVE 'CONTDCE'      TO PPNMFILE.
           PERFORM Z10-READPRIN
              THRU Z10-EX.

       L05-EX. EXIT.

      *----------------------------------------------------------------*
       L10-LEGGI-CLIENTE.
      *----------------------------------------------------------------*

           MOVE 'L10-' TO WCOM-CODABEND.
           MOVE 'SELECT' TO WCOM-FILEFUNZ.
           MOVE 'CLIENTI' TO WCOM-FILENOME.
           MOVE A901-CODCLIEN TO WCOM-FILECTRL.

           EXEC SQL
                SELECT RAGFATTU,CITFATTU
                      ,PAESFATT
                  INTO :A901-RAGFATTU,:A901-CITFATTU
                      ,:A901-PAESFATT
                  FROM DANAGRAF.TCLIENTI
                 WHERE CODCLIEN = :A901-CODCLIEN
           END-EXEC.

           IF SQLCODE NOT = +0
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

           MOVE 'U'            TO PPROUTIN.
           MOVE 'D'            TO PPTPFILE.
           MOVE 'R'            TO PPTPOPER.
           MOVE 'CLIENTI'      TO PPNMFILE.
           PERFORM Z10-READPRIN
              THRU Z10-EX.

       L10-EX. EXIT.

      *----------------------------------------------------------------*
       L15-LEGGI-AARIFEMAX.
      *----------------------------------------------------------------*

           MOVE 'L15-' TO WCOM-CODABEND.
           MOVE 'SELECT' TO WCOM-FILEFUNZ.
           MOVE 'SICOPLM' TO WCOM-FILENOME.
           MOVE SPACES    TO C200-AARIFERI.
           STRING C200-CODCLIEN ' ' C200-CODSOCIE ' '
                  DELIMITED BY SIZE INTO WCOM-FILECTRL.

           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.

           EXEC SQL
                SELECT MAX(AARIFERI)
                  INTO :C200-AARIFERI
                  FROM DCONTCLI.TSICOPLNEW
                 WHERE CODSOCIE = :C200-CODSOCIE
                   AND CODCLIEN = :C200-CODCLIEN
                   AND INSCOCON<> 0
           END-EXEC.

           IF SQLCODE = +0
              MOVE 'U'            TO PPROUTIN
              MOVE 'D'            TO PPTPFILE
              MOVE 'R'            TO PPTPOPER
              MOVE 'SICOPLM'      TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX
           ELSE
              IF SQLCODE NOT = -305
                 PERFORM Z00-ERRORE
                    THRU Z00-EX.

           EXEC SQL WHENEVER SQLERROR GOTO Z00-ERRORE END-EXEC.

       L15-EX. EXIT.

      *----------------------------------------------------------------*
       L20-SCBNUM5.
      *----------------------------------------------------------------*

           MOVE 'SCBNUM5'         TO PPCALL.
           CALL PPCALL            USING DCPARM DCKYNUM.

           IF DCKY-RISPOSTA  NOT  = '****'
              STRING 'ERRORE DA SCBNUM5 ' DCKY-RISPOSTA
              DELIMITED BY SIZE INTO PPMESSAG
              MOVE 'M'         TO PPROUTIN
              PERFORM Z10-READPRIN
                 THRU Z10-EX
              PERFORM Z00-ERRORE
                 THRU Z00-EX
           ELSE
              MOVE 'U'             TO PPROUTIN
              MOVE 'D'             TO PPTPFILE
              MOVE 'R'             TO PPTPOPER
              MOVE 'SCBNUM5'       TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX.

       L20-EX. EXIT.

      *----------------------------------------------------------------*
       L25-CERCA-CENCOSTO.
      *----------------------------------------------------------------*

           MOVE 'L25-' TO WCOM-CODABEND.
           MOVE 'SELECT' TO WCOM-FILEFUNZ.
           MOVE 'C/COSTO' TO WCOM-FILENOME.
           MOVE WS-CODCLIEN TO WCOM-FILECTRL.

           INITIALIZE                         DCWCCOS
           MOVE WS-CODCLIEN                TO CCOS-CODCLIEN
           MOVE 'SCBCCOS'                  TO PPCALL
           CALL PPCALL USING DCPARM DCWCCOS.

           IF CCOS-SQLCODE = +100
               MOVE SPACES                TO CCOS-CENCOCOM
               STRING 'RENATO: MANCA C/COSTO X CHIAVE CCOS : '
                       DCWCCOS
                  DELIMITED BY SIZE INTO PPMESSAG
               MOVE 'M'                   TO PPROUTIN
               PERFORM Z10-READPRIN
                  THRU Z10-EX.

           MOVE CCOS-CENCOCOM              TO WS-CENCOSTO.

       L25-EX. EXIT.

      *----------------------------------------------------------------*
       L30-LEGGI-MOVIMENTI.
      *----------------------------------------------------------------*

           MOVE 'L30-' TO WCOM-CODABEND.
           MOVE 'SELECT' TO WCOM-FILEFUNZ.
           MOVE 'MOVIMEN' TO WCOM-FILENOME.
           STRING C852-CODSOCIE ' ' C852-CDMOVCON ' '
                  C852-NRMOVCON ' ' C852-DTREGMOV ' '
                  C852-CODCLIEN ' '
                  DELIMITED BY SIZE INTO WCOM-FILECTRL.

           EXEC SQL
                SELECT CODSOCIE,CODCLIEN,CDMOVCON,
                       NRMOVCON,DTREGMOV,DATAVALU,
                       CENCOSTO,TIPOAGEN,CODAGENT,
                       FLPROVPA,CAUSACON,NRMOVRIF,
                       FLGRATEO,FLEXECON,BANDIVAL,
                       NRDICVAL,DTULTVAR,NRMOVRI2,
                       IMPROFAT,FLGSTORN,DTCONTAB,
                       TIPOMOVI,VOCEDOPR,IMPDOGPR,
                       CODUTVAR,ITAESTER
                  INTO :C852-CODSOCIE,:C852-CODCLIEN,:C852-CDMOVCON,
                       :C852-NRMOVCON,:C852-DTREGMOV,:C852-DATAVALU,
                       :C852-CENCOSTO,:C852-TIPOAGEN,:C852-CODAGENT,
                       :C852-FLPROVPA,:C852-CAUSACON,:C852-NRMOVRIF,
                       :C852-FLGRATEO,:C852-FLEXECON,:C852-BANDIVAL,
                       :C852-NRDICVAL,:C852-DTULTVAR,:C852-NRMOVRI2,
                       :C852-IMPROFAT,:C852-FLGSTORN,:C852-DTCONTAB,
                       :C852-TIPOMOVI,:C852-VOCEDOPR,:C852-IMPDOGPR,
                       :C852-CODUTVAR,:C852-ITAESTER
                  FROM DCONTCLI.TMOVIMENTI
                 WHERE CODSOCIE = :C852-CODSOCIE
                   AND CODCLIEN = :C852-CODCLIEN
                   AND CDMOVCON = :C852-CDMOVCON
                   AND NRMOVCON = :C852-NRMOVCON
                   AND DTREGMOV = :C852-DTREGMOV
           END-EXEC.

           IF SQLCODE = +0
              MOVE 'U'             TO PPROUTIN
              MOVE 'D'             TO PPTPFILE
              MOVE 'R'             TO PPTPOPER
              MOVE 'MOVIMEN'       TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX
           ELSE
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

       L30-EX. EXIT.

      *----------------------------------------------------------------*
       L35-LEGGI-MOVISCAD.
      *----------------------------------------------------------------*

           MOVE 'L35-' TO WCOM-CODABEND.
           MOVE 'SELECT' TO WCOM-FILEFUNZ.
           MOVE 'MOVISCA' TO WCOM-FILENOME.
           STRING C855-CODSOCIE ' ' C855-CDMOVCON ' '
                  C855-NRMOVCON ' ' C855-DTREGMOV ' '
                  C855-CODCLIEN ' '
                  DELIMITED BY SIZE INTO WCOM-FILECTRL.

           EXEC SQL
                SELECT
                       FLAGSTOR,CODSOCIE,CODCLIEN,
                       CDMOVCON,NRMOVCON,DTREGMOV,
                       DTSCADEN,TIPOPAGA,DTDECORR,
                       IMPMOVIM,INDSEGNO,CVLMOVIM,
                       IMPORIVA,SCONTDED,PARTCONT,
                       BANCAPPO,DISTICON,BANCACON,
                       AACONTAB,NRCONTAB,NOTENOTE,
                       FLAGSALD,NETTSCON,PLUSSCAD,
                       SIMPROVV,CDSELTIP,CDSELMOV,
                       DTULTVAR,FLEXECON,OLDSCADE,
                       IMPROFAT,DATADIST,DTINCASS,
                       FLAGTRAT,TIPOSOLL,CAUSBANC,
                       CODDIVIS,CVLXXIVA,SCADORIG,
                       NUMGGMED,CODUTVAR
                  INTO :C855-FLAGSTOR,:C855-CODSOCIE,:C855-CODCLIEN,
                       :C855-CDMOVCON,:C855-NRMOVCON,:C855-DTREGMOV,
                       :C855-DTSCADEN,:C855-TIPOPAGA,:C855-DTDECORR,
                       :C855-IMPMOVIM,:C855-INDSEGNO,:C855-CVLMOVIM,
                       :C855-IMPORIVA,:C855-SCONTDED,:C855-PARTCONT,
                       :C855-BANCAPPO,:C855-DISTICON,:C855-BANCACON,
                       :C855-AACONTAB,:C855-NRCONTAB,:C855-NOTENOTE,
                       :C855-FLAGSALD,:C855-NETTSCON,:C855-PLUSSCAD,
                       :C855-SIMPROVV,:C855-CDSELTIP,:C855-CDSELMOV,
                       :C855-DTULTVAR,:C855-FLEXECON,:C855-OLDSCADE,
                       :C855-IMPROFAT,:C855-DATADIST,:C855-DTINCASS,
                       :C855-FLAGTRAT,:C855-TIPOSOLL,:C855-CAUSBANC,
                       :C855-CODDIVIS,:C855-CVLXXIVA,:C855-SCADORIG,
                       :C855-NUMGGMED,:C855-CODUTVAR
                  FROM DCONTCLI.TMOVISCAD
                 WHERE CODSOCIE = :C855-CODSOCIE
                   AND CDMOVCON = :C855-CDMOVCON
                   AND NRMOVCON = :C855-NRMOVCON
                   AND DTREGMOV = :C855-DTREGMOV
                   AND CODCLIEN = :C855-CODCLIEN
                   AND DTSCADEN = :C855-DTSCADEN
                   AND OLDSCADE = :C855-OLDSCADE
           END-EXEC.

           IF SQLCODE = +0
              MOVE 'U'             TO PPROUTIN
              MOVE 'D'             TO PPTPFILE
              MOVE 'R'             TO PPTPOPER
              MOVE 'MOVISCA'       TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX
           ELSE
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

       L35-EX. EXIT.

      *----------------------------------------------------------------*
       L40-LEGGI-MOVIDCE.
      *----------------------------------------------------------------*

           MOVE 'L40-' TO WCOM-CODABEND.
           MOVE 'SELECT' TO WCOM-FILEFUNZ.
           MOVE 'MOVIDCE' TO WCOM-FILENOME.
           STRING C858-CODSOCIE ' ' C858-CDMOVCON ' '
                  C858-NRMOVCON ' ' C858-DTREGMOV ' '
                  C858-CODCLIEN ' '
                  DELIMITED BY SIZE INTO WCOM-FILECTRL.

           EXEC SQL
                SELECT
                      CODSOCIE,CODCLIEN,CDMOVCON,
                      NRMOVCON,DTREGMOV,DTSCADEN,
                      CONTODCE,IMPARMOV,INDSEGNO,
                      CVLMOVIM,IMPORIVA,PERCEIVA,
                      ESENTIVA,IMPADIVA,ADDFATTU,
                      FLEXECON,DTULTVAR,OLDSCADE,
                      CVLXXIVA,CVLADIVA,CODUTVAR
                  INTO
                      :C858-CODSOCIE,:C858-CODCLIEN,:C858-CDMOVCON,
                      :C858-NRMOVCON,:C858-DTREGMOV,:C858-DTSCADEN,
                      :C858-CONTODCE,:C858-IMPARMOV,:C858-INDSEGNO,
                      :C858-CVLMOVIM,:C858-IMPORIVA,:C858-PERCEIVA,
                      :C858-ESENTIVA,:C858-IMPADIVA,:C858-ADDFATTU,
                      :C858-FLEXECON,:C858-DTULTVAR,:C858-OLDSCADE,
                      :C858-CVLXXIVA,:C858-CVLADIVA,:C858-CODUTVAR
                  FROM
                      DCONTCLI.TMOVIDCE
                 WHERE CODSOCIE = :C858-CODSOCIE
                   AND CDMOVCON = :C858-CDMOVCON
                   AND NRMOVCON = :C858-NRMOVCON
                   AND DTREGMOV = :C858-DTREGMOV
                   AND DTSCADEN = :C858-DTSCADEN
                   AND OLDSCADE = :C858-OLDSCADE
           END-EXEC.

           IF SQLCODE = +0
              MOVE 'U'             TO PPROUTIN
              MOVE 'D'             TO PPTPFILE
              MOVE 'R'             TO PPTPOPER
              MOVE 'MOVIDCE'       TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX
           ELSE
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

       L40-EX. EXIT.

      *----------------------------------------------------------------*
       L45-LEGGI-SICONOOK.
      *----------------------------------------------------------------*

           MOVE 'L45-' TO WCOM-CODABEND.
           MOVE 'SELECT' TO WCOM-FILEFUNZ.
           MOVE 'SICNOOK' TO WCOM-FILENOME.
           STRING C200-CODCLIEN ' ' C200-CODSOCIE ' '
                  C200-AARIFERI ' '
                  DELIMITED BY SIZE INTO WCOM-FILECTRL.

           EXEC SQL
                SELECT INSCOCON
                  INTO :C200-INSCOCON
                  FROM DCONTCLI.TSICOPLNEW
                 WHERE CODSOCIE = :C200-CODSOCIE
                   AND CODCLIEN = :C200-CODCLIEN
                   AND AARIFERI = :C200-AARIFERI
                   AND INSCOCON<> 0
                   AND (INSCOCON<   0 OR
                        INSCOCON> 100)
                 FETCH FIRST ROWS ONLY
           END-EXEC.

           IF SQLCODE = +0
              MOVE 'U'            TO PPROUTIN
              MOVE 'D'            TO PPTPFILE
              MOVE 'R'            TO PPTPOPER
              MOVE 'SICOPLM'      TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX
           ELSE
              IF SQLCODE NOT = +100
                 PERFORM Z00-ERRORE
                    THRU Z00-EX.

       L45-EX. EXIT.

      *----------------------------------------------------------------*
       L50-LEGGI-SICOMAXAN.
      *----------------------------------------------------------------*

           MOVE 'L50-' TO WCOM-CODABEND.
           MOVE 'SELECT' TO WCOM-FILEFUNZ.
           MOVE 'SICMAXX' TO WCOM-FILENOME.
           STRING C200-CODCLIEN ' ' C200-CODSOCIE ' '
                  DELIMITED BY SIZE INTO WCOM-FILECTRL.

           EXEC SQL
                SELECT PROLOMER,INSCOCON
                      ,CODLINEA,TIPOSTAG
                      ,SECSCELT,FLASTOCK
                  INTO :C200-PROLOMER, :C200-INSCOCON
                      ,:C200-CODLINEA, :C200-TIPOSTAG
                      ,:C200-SECSCELT, :C200-FLASTOCK
                  FROM DCONTCLI.TSICOPLNEW     S
                 WHERE CODSOCIE = :C200-CODSOCIE
                   AND CODCLIEN = :C200-CODCLIEN
                   AND AARIFERI = :C200-AARIFERI
                   AND INSCOCON<> 0
                   AND INSCOCON =
                       (SELECT MAX(I.INSCOCON)
                          FROM DCONTCLI.TSICOPLNEW I
                         WHERE S.CODSOCIE = I.CODSOCIE
                           AND S.CODCLIEN = I.CODCLIEN
                           AND S.AARIFERI = I.AARIFERI
                           AND I.INSCOCON > 0)
                 FETCH FIRST ROWS ONLY
           END-EXEC.

           IF SQLCODE = +0
              MOVE 'U'            TO PPROUTIN
              MOVE 'D'            TO PPTPFILE
              MOVE 'R'            TO PPTPOPER
              MOVE 'SICOPLM'      TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX
           ELSE
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

           INITIALIZE                         DCWCCOS
           MOVE C200-CODCLIEN              TO CCOS-CODCLIEN
           MOVE C200-CODLINEA              TO CCOS-CODLINEA
           MOVE C200-PROLOMER              TO CCOS-PROLOMER

           MOVE C200-TIPOSTAG              TO CCOS-TIPOSTAG.

           IF C200-SECSCELT = '3' OR 'D'
              MOVE 'D'                     TO CCOS-FLREGDIF
           ELSE
              IF C200-FLASTOCK = 'S'
                 MOVE 'S'                  TO CCOS-FLREGDIF.

           MOVE 'SCBCCOS'                  TO PPCALL
           CALL PPCALL USING DCPARM DCWCCOS.

           IF CCOS-SQLCODE = +100
               STRING 'RENATO: MANCA C/COSTO X CHIAVE CCOS : '
                       DCWCCOS
                  DELIMITED BY SIZE INTO PPMESSAG
               MOVE 'M'                   TO PPROUTIN
               PERFORM Z10-READPRIN
                  THRU Z10-EX.

       L50-EX. EXIT.

      *----------------------------------------------------------------*
       L55-FETCH-SUMSCOPE.
      *----------------------------------------------------------------*

           MOVE 'L55-'                  TO WCOM-CODABEND.
           MOVE 'FETCH'                 TO WCOM-FILEFUNZ.
           MOVE 'PCVLSCO'               TO WCOM-FILENOME.

           EXEC SQL
                FETCH PCVLSCO
                 INTO  :WS-SOCISCOP,:WS-CLIESCOP,:WS-CDMOSCOP,
                       :WS-DTRESCOP,:WS-NRMOSCOP,:WS-DTSCSCOP,
                       :WS-OLDSSCOP,:WS-INDSSCOP,:WS-CVLSCOPE
           END-EXEC.

           IF SQLCODE = ZERO
              MOVE 'U'            TO PPROUTIN
              MOVE 'D'            TO PPTPFILE
              MOVE 'R'            TO PPTPOPER
              MOVE 'PCVLSCO'      TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX.

       L55-EX. EXIT.

      *----------------------------------------------------------------*
       L60-FETCH-PSICOPL.
      *----------------------------------------------------------------*

           MOVE 'L60-'                  TO WCOM-CODABEND.
           MOVE 'FETCH'                 TO WCOM-FILEFUNZ.
           MOVE 'PSICOPL'               TO WCOM-FILENOME.

           EXEC SQL
                FETCH PSICOPL
                 INTO  :C200-PROLOMER,:C200-INSCOCON
                      ,:C200-CODLINEA,:C200-TIPOSTAG
                      ,:C200-SECSCELT,:C200-FLASTOCK
           END-EXEC.

           IF SQLCODE = ZERO
              MOVE 'U'            TO PPROUTIN
              MOVE 'D'            TO PPTPFILE
              MOVE 'R'            TO PPTPOPER
              MOVE 'PSICOPL'      TO PPNMFILE
              PERFORM Z10-READPRIN
                 THRU Z10-EX.

       L60-EX. EXIT.

      *----------------------------------------------------------------*
       M00-MESSCONTO.
      *----------------------------------------------------------------*

           MOVE SPACES TO PPMESSAG.
           STRING 'MANCA CONTO FONPER SOC. ' A531-CODSOCIE
                  DELIMITED BY SIZE INTO PPMESSAG
           MOVE 'M' TO PPROUTIN
           PERFORM Z10-READPRIN
              THRU Z10-EX
           MOVE 'CPRO' TO WCOM-CODABEND
           PERFORM Z00-ERRORE
              THRU Z00-EX.

       M00-EX. EXIT.

      *----------------------------------------------------------------*
       M05-MESSDIFFE.
      *----------------------------------------------------------------*

           MOVE SPACES TO PPMESSAG.
           MOVE WS-TCVSCOPE   TO WS-CVLDOPED.
           MOVE C855-CVLMOVIM TO WS-IMPORTED.
           MOVE WS-COMODIFF TO WS-PEREDITT.
           STRING 'DIFF. ECCESSIVA ---> ' WS-PEREDITT
                  ' TRA SCOPERTO ---> ' WS-CVLDOPED
                  ' E RICALCOLO  ---> ' WS-IMPORTED
                  ' CLI.  ---> ' C855-CODCLIEN
                  ' DIV.  ---> ' WS-CODDIVIS
                  DELIMITED BY SIZE INTO PPMESSAG
           MOVE 'M' TO PPROUTIN
           PERFORM Z10-READPRIN
              THRU Z10-EX.

       M05-EX. EXIT.

      *--------------------------------------------------------------*
       S00-SCRIVI-FILEOUT.
      *--------------------------------------------------------------*

           COMPUTE WS-TOTALE = WS-TOTALE + FINA-CVLDIVIS
           WRITE REC-FILEOUT FROM FINA-RECORDXX.

           MOVE 'U'            TO PPROUTIN
           MOVE 'S'            TO PPTPFILE
           MOVE 'A'            TO PPTPOPER
           MOVE 'FILEOUT'      TO PPNMFILE
           PERFORM Z10-READPRIN
              THRU Z10-EX.

       S00-EX. EXIT.

      *--------------------------------------------------------------*
       U00-UPDATE-MOVISCAD.
      *--------------------------------------------------------------*

           MOVE 'U00-'                 TO WCOM-CODABEND.
           MOVE 'UPDATE'               TO WCOM-FILEFUNZ.
           MOVE 'MOVISCA'              TO WCOM-FILENOME.
           MOVE SPACES                 TO WCOM-FILECTRL.
           STRING C855-CODSOCIE ' ' C855-CODCLIEN ' '
                  C855-CDMOVCON ' ' C855-NRMOVCON ' '
                  C855-DTREGMOV ' ' C855-DTSCADEN
              DELIMITED BY SIZE      INTO WCOM-FILECTRL.

           EXEC SQL
                UPDATE DCONTCLI.TMOVISCAD
                   SET PARTCONT = :WS-PARTCONT,
                       CDSELTIP = 'X',
                       CDSELMOV = 'J',
                       FLAGSALD = 'S'
                 WHERE CODSOCIE = :C855-CODSOCIE
                   AND CDMOVCON = :C855-CDMOVCON
                   AND NRMOVCON = :C855-NRMOVCON
                   AND DTREGMOV = :C855-DTREGMOV
                   AND CODCLIEN = :C855-CODCLIEN
                   AND DTSCADEN = :C855-DTSCADEN
                   AND OLDSCADE = :C855-OLDSCADE
           END-EXEC.

           IF SQLCODE NOT = +0
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

           MOVE 'U'            TO PPROUTIN.
           MOVE 'D'            TO PPTPFILE.
           MOVE 'U'            TO PPTPOPER.
           MOVE 'MOVISCA'      TO PPNMFILE.
           PERFORM Z10-READPRIN
              THRU Z10-EX.

       U00-EX. EXIT.
      *--------------------------------------------------------------*
       W00-WRITE.
      *--------------------------------------------------------------*
           WRITE RIGA FROM WS-REC-STAMPA.
           MOVE SPACES TO WS-REC-STAMPA.
       W00-EX. EXIT.
      *--------------------------------------------------------------*
       Z00-ERRORE.
      *--------------------------------------------------------------*

           MOVE 'SCDB2ER'    TO PPCALL.
           CALL PPCALL   USING DCPARM DCDB2ER SQLCA.

       Z00-EX. EXIT.

      *--------------------------------------------------------------*
       Z10-READPRIN.
      *--------------------------------------------------------------*

           MOVE 'READPRIN'   TO PPCALL.
           CALL PPCALL    USING DCPARM.

       Z10-EX. EXIT.

