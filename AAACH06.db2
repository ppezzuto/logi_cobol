      *-------------*  PROGRAMMA  BATCH  MVS COBOL2 DB2  *-------------*

       ID  DIVISION.
       PROGRAM-ID.                     AAACH06.

      ******************************************************************
      *                                                                *
      *  RICALCOLO MENSILE DEI CONTROVALORI DELLE REGISTRAZIONI DA     *
      *  SALDARE DELLA SOCIETA' INDICATA IN SCHEDA PARAMETRO           *
      *                                                                *
      *  AGGIORNAMENTO CONTROVALORI SULLE TABELLE TMOVISCAD E TMOVIDCE *
      *  E RELATIVA QUADRATURA.                                        *
      *                                                                *
      *  SCRITTURA DI UN FILE   IN OUTPUT CON LE DIFFERENZE TRA PRIMA  *
      *  E DOPO DA INVIARE AL G.T.M.                                   *
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
       01  FILLER                      PIC X(8) VALUE '*DCWCAMB'.
           COPY DCWCAMB.
       01  FILLER                      PIC X(8) VALUE '*DCWCHIU'.
           COPY DCWCHIU.
       01  FILLER                      PIC X(8) VALUE '*DCWDTEL'.
           COPY DCWDTEL.
       01  FILLER                      PIC X(8) VALUE '*DCARR0*'.
           COPY DCARR0.
       01  FILLER                      PIC X(8) VALUE '*DCWCCOS'.
           COPY DCWCCOS.

       01  FILLER                      PIC X(8) VALUE 'TAB.CLIE'.
       01  TABCLIE.
         05  ELEM-TABCLIE OCCURS 50.
             10  TBCL-PROLOMER           PIC X(2).
             10  TBCL-CODLINEA           PIC X(2).
             10  TBCL-TIPOSTAG           PIC X(1).
             10  TBCL-SECSCELT           PIC X(1).
             10  TBCL-FLASTOCK           PIC X(1).
             10  TBCL-CENCOSTO           PIC X(5).
             10  TBCL-IMPMOVIM           PIC S9(11)V99 COMP-3.
             10  TBCL-CVLPRIMA           PIC S9(11)V99 COMP-3.
             10  TBCL-CVLDOPOO           PIC S9(11)V99 COMP-3.
             10  TBCL-CVLPERDI           PIC  9(11)V99 COMP-3.
             10  TBCL-CVLUTILE           PIC  9(11)V99 COMP-3.
             10  TBCL-CVLDIFFE           PIC S9(11)V99 COMP-3.

       01  FILLER                      PIC X(8) VALUE 'TAB.CECO'.
       01  TABCECO.
         05  ELEM-TABCECO OCCURS 300.
             10  TBCC-CODSOCIE           PIC XX.
             10  TBCC-CENCOSTO           PIC X(5).
             10  TBCC-CVLPRIMA           PIC S9(11)V99 COMP-3.
             10  TBCC-CVLDOPOO           PIC S9(11)V99 COMP-3.
             10  TBCC-CVLPERDI           PIC S9(11)V99 COMP-3.
             10  TBCC-CVLUTILE           PIC S9(11)V99 COMP-3.
             10  TBCC-CVLDIFFE           PIC S9(11)V99 COMP-3.

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
             10  TBST-CVLUTILE           PIC S9(11)V99 COMP-3.
             10  TBST-CVLPERDI           PIC S9(11)V99 COMP-3.
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
                   10  FILLER            PIC X.
                   10  SK-TIPOREGI       PIC X(2).
               07  FILLER                PIC X(69).
           05 WS-ELABORAT              PIC XX                VALUE 'NO'.
           05 WS-DATAGIOR              PIC X(8).
           05 WS-MINIDATA              PIC X(8).
           05 WS-MAXIDATA              PIC X(8).
           05 WS-DTCAMBIO              PIC X(8).
           05 WS-SALVELEM              PIC X(49).
           05 WS-INSCOCON              PIC S9(11)V99  COMP-3.
           05 WS-COMDIFFE              PIC S9(11)V99  COMP-3.
           05 WS-PARCVLPR              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-PARCVLDO              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-PARIMPOR              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLSCOLD              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVSCANEW              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLDIFFE              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TCVLPRIM              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TCVLDOPO              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TIMPORTO              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-IMPXCONV              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-IVAXCONV              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOCVLDOP              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOCVLPRI              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOCVLUTI              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOCVLPER              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOTCVLDC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOTCIVDC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOTUTICC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOTPERCC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLDIFDC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-CVLDIVDC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-DIFFEREN              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-DIFFERCC              PIC S9(11)V99  COMP-3 VALUE +0.
           05 WS-TOTALE                PIC S9(11)V99         VALUE +0.
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
           05 WS-PEREDITT              PIC Z(7)9,99-.
           05 WS-CVLPRIED              PIC Z(7)9,99-.
           05 WS-CVLDOPED              PIC Z(7)9,99-.
           05 WS-IMPORTED              PIC Z(7)9,99-.
           05 WS-CVLDIFED              PIC Z(7)9,99-.
           05 WS-CVLMOVED              PIC Z(7)9,99-.
           05 WS-CVLUTIED              PIC Z(7)9,99.
           05 WS-CVLUTIEX REDEFINES WS-CVLUTIED PIC X(11).
           05 WS-CVLPERED              PIC Z(7)9,99.
           05 WS-CVLPEREX REDEFINES WS-CVLPERED PIC X(11).
           05 WS-CVLDELED              PIC Z(7)9,99-.
           05 WS-INDITABE              PIC S9(5).
           05 WS-INDICLIE              PIC S9(5).
           05 WS-INDICMAX              PIC S9(5).
           05 WS-CODSOCIE              PIC XX.
           05 WS-NOMSOCIE              PIC X(23).
           05 WS-SIGLADIV              PIC XXX.
           05 WS-CODCLIEN              PIC X(5)  VALUE SPACES.
           05 WS-CENCOSTO              PIC X(5).
           05 WS-SWORDINA              PIC XX                VALUE 'SI'.
           05 WS-CVLTTTFP              PIC  9(11)V99  COMP-3 VALUE  0.
           05 WS-CVLTTTFN              PIC  9(11)V99  COMP-3 VALUE  0.
           05 WS-RIGATOT.
              10 FILLER                PIC X(24) VALUE SPACES.
              10 FILLER                PIC X(10) VALUE '* TOTALE *'.
              10 FILLER                PIC X(14) VALUE SPACES.
              10 WS-CVLTOPRI           PIC Z(7)9,99-.
              10 FILLER                PIC X     VALUE SPACES.
              10 WS-CVLTODOP           PIC Z(7)9,99-.
              10 FILLER                PIC X     VALUE SPACES.
              10 WS-CVLTOUTI           PIC Z(7)9,99-.
              10 FILLER                PIC X     VALUE SPACES.
              10 WS-CVLTOPER           PIC Z(7)9,99.
              10 FILLER                PIC XX    VALUE SPACES.
              10 WS-CVLTODIF           PIC Z(7)9,99-.
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

      *------------------------------ DCONTCLI.TSICOPLNEW
       01  FILLER                      PIC X(8) VALUE 'FDCLI200'.
           EXEC SQL INCLUDE FDCLI200 END-EXEC.

      *------------------------------ DCONTCLI.TMOVIMENTI
       01  FILLER                      PIC X(8) VALUE 'FDCLI852'.
           EXEC SQL INCLUDE FDCLI852 END-EXEC.

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
                    SELECT DISTINCT
                           S.CODSOCIE,S.CODDIVIS,S.CODCLIEN,
                           S.CDMOVCON,S.DTREGMOV,S.NRMOVCON,
                           S.CVLMOVIM,S.IMPMOVIM,S.DTSCADEN,
                           S.OLDSCADE,S.IMPORIVA,S.INDSEGNO,
                           S.CVLXXIVA,M.CENCOSTO,D.SIGLADIV
                      FROM DCONTCLI.TMOVISCAD  S,
                           DCONTCLI.TMOVIMENTI M,
                           DCONTCLI.TMOVIDCE   E,
                           DCONTCLI.TCONTIDCE  P,
                           DANAGRAF.TSOCIETA   T,
                           DANAGRAF.TDIVISE    D
                     WHERE S.FLAGSALD     <> 'S'
                       AND S.FLAGSALD     <> 'A'
                       AND S.CODSOCIE     = :C855-CODSOCIE
                       AND NOT S.DTREGMOV > :WS-MAXIDATA
                       AND S.CODSOCIE =  M.CODSOCIE
                       AND S.CDMOVCON =  M.CDMOVCON
                       AND S.NRMOVCON =  M.NRMOVCON
                       AND S.DTREGMOV =  M.DTREGMOV
                       AND S.CODCLIEN =  M.CODCLIEN
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
                       AND NOT P.CONTPROV IN ('FONPER','PERDIT'
                                             ,'STFOND')
                       AND S.CODDIVIS =  D.CODDIVIS
                       AND D.CODDIVRI =  T.CODDIVRI
                     ORDER BY 1 , 3 , 2 , 4 , 6
           END-EXEC.

           EXEC SQL DECLARE PMOVDCE CURSOR FOR
                    SELECT IMPARMOV,CVLMOVIM,CONTODCE,
                           IMPORIVA,CVLXXIVA
                      FROM DCONTCLI.TMOVIDCE
                     WHERE CODSOCIE = :C858-CODSOCIE
                       AND CDMOVCON = :C858-CDMOVCON
                       AND NRMOVCON = :C858-NRMOVCON
                       AND DTREGMOV = :C858-DTREGMOV
                       AND CODCLIEN = :C858-CODCLIEN
                       AND DTSCADEN = :C858-DTSCADEN
                       AND OLDSCADE = :C858-OLDSCADE
                       FOR UPDATE OF CVLMOVIM, CVLXXIVA
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

           EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           MOVE 'AAACH06'              TO PPNAME
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

           IF WS-ELABORAT = 'SI'
              PERFORM B10-STAMPA-TABELLA
                 THRU B10-EX.

           MOVE 'MAIN'                 TO WCOM-CODABEND.
           MOVE 'CLOSE'                TO WCOM-FILEFUNZ.
           MOVE 'PMOVSCA'              TO WCOM-FILENOME.
           MOVE SPACES                 TO WCOM-FILECTRL
           EXEC SQL CLOSE PMOVSCA    END-EXEC.

           DISPLAY 'TOTALE ' WS-TOTALE

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

           IF  SK-AGGIORNA NOT EQUAL 'SI' AND 'NO'
               MOVE 'SKAG'             TO WCOM-CODABEND
               MOVE 'AGGIORN'          TO WCOM-FILEFUNZ
               MOVE 'SKEDA-PARAMETRO'  TO WCOM-FILENOME
               MOVE  WS-SKEDA          TO WCOM-FILECTRL
               PERFORM Z00-ERRORE
                  THRU Z00-EX.

           IF  SK-TIPOREGI NOT EQUAL 'U' AND 'M'
               MOVE 'SKAG'             TO WCOM-CODABEND
               MOVE 'TIPOREG'          TO WCOM-FILEFUNZ
               MOVE 'SKEDA-PARAMETRO'  TO WCOM-FILENOME
               MOVE  WS-SKEDA          TO WCOM-FILECTRL
               PERFORM Z00-ERRORE
                  THRU Z00-EX.

           IF SK-TIPOREGI = 'U' AND SK-AGGIORNA = 'SI'
              STRING 'RENATO: FUNZIONI -AGGIORNA- E -TIPO REGISTRAZ.-'
                     ' INCOMPATIBILI'
                     DELIMITED BY SIZE INTO PPMESSAG
              MOVE 'M' TO PPROUTIN
              PERFORM Z10-READPRIN
                 THRU Z10-EX
              MOVE 'A00-'              TO WCOM-CODABEND
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

           IF SK-TIPOREGI = 'U'
              EXEC SQL
                   SELECT CONTODCE
                   INTO :C861-CONTODCE
                   FROM DCONTCLI.TCONTIDCE
                  WHERE CODPIANO = :A531-CODPIANO
                    AND CONTPROV = 'FONCAM'
              END-EXEC
              IF SQLCODE NOT = ZERO
                 MOVE 'A00-'             TO WCOM-CODABEND
                 MOVE 'SELECT '          TO WCOM-FILEFUNZ
                 MOVE 'CONTPRV'          TO WCOM-FILENOME
                 MOVE SPACES             TO WCOM-FILECTRL
                 STRING 'FONCAM ' A531-CODPIANO
                        DELIMITED BY SIZE INTO WCOM-FILECTRL
                 PERFORM Z00-ERRORE
                    THRU Z00-EX.

           MOVE SK-CODSOCIE            TO C855-CODSOCIE.
           MOVE SPACES                 TO DCWDTEL.
           MOVE 'R'                    TO DTEL-TIPORICH.
           MOVE 'M'                    TO DTEL-DATATIPO.
           MOVE 'AAACH06'              TO DTEL-SIGLDATA.
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
           MOVE 'AAACH06'              TO DTEL-SIGLDATA.
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
           COMPUTE WS-CVLDIFFE = C855-CVLMOVIM - WS-CVLSCOLD.

           IF WS-CVLDIFFE NOT = ZEROES
              MOVE 'SI'          TO WS-ELABORAT
              PERFORM C10-AGGIORNA-MOVISCAD
                 THRU C10-EX
              PERFORM C15-ELABORA-DIFFE
                 THRU C15-EX.

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
                           TBST-CVLUTILE (WS-INDITABE) -
                           TBST-CVLPERDI (WS-INDITABE)
                   COMPUTE WS-DIFFEREN = WS-DIFFEREN +
                           TBST-CVLDIFFE (WS-INDITABE)
                   IF TBST-CVLDIFFE (WS-INDITABE) NOT EQUAL ZEROES
                      PERFORM C25-DETTAGLIO
                         THRU C25-EX
                   END-IF
           END-PERFORM.

           PERFORM D15-TOTALI-SOCIETA
              THRU D15-EX.

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
       C10-AGGIORNA-MOVISCAD.
      *--------------------------------------------------------------*

           IF SK-AGGIORNA = 'SI'
              PERFORM U00-UPDATE-MOVISCAD
                 THRU U00-EX.

       C10-EX. EXIT.

      *--------------------------------------------------------------*
       C15-ELABORA-DIFFE.
      *--------------------------------------------------------------*

           PERFORM D00-SISTEMA-TABEL
              THRU D00-EX.

           MOVE C855-CODSOCIE TO C858-CODSOCIE.
           MOVE C855-CODCLIEN TO C858-CODCLIEN.
           MOVE C855-CDMOVCON TO C858-CDMOVCON.
           MOVE C855-NRMOVCON TO C858-NRMOVCON.
           MOVE C855-DTREGMOV TO C858-DTREGMOV.
           MOVE C855-DTSCADEN TO C858-DTSCADEN.
           MOVE C855-OLDSCADE TO C858-OLDSCADE.

           IF C855-IMPORIVA GREATER ZEROES
              COMPUTE WS-IMPXCONV = C855-IMPMOVIM -
                                    C855-IMPORIVA
              MOVE A531-CODDIVRI TO WS-CODDIVRI
              MOVE A531-CODDIVIS TO WS-CODDIVSO
              MOVE C855-CODDIVIS TO WS-CODDIVIS
              MOVE C855-IMPORIVA TO WS-IVAXCONV
              PERFORM C00-CHIAMA-SCBCB10
                 THRU C00-EX
              MOVE CAMB-DECICAMB TO ARR0-DECIDIVI
              MOVE CAMB-CONTROVA(1) TO ARR0-IMPORTOX
              PERFORM C05-ARROTONDA-IMP
                 THRU C05-EX
              MOVE ARR0-IMPORTOX    TO C855-CVLMOVIM
              MOVE CAMB-DECICAMB TO ARR0-DECIDIVI
              MOVE CAMB-CONTROVA(2) TO ARR0-IMPORTOX
              PERFORM C05-ARROTONDA-IMP
                 THRU C05-EX
              MOVE ARR0-IMPORTOX    TO C855-CVLXXIVA.

           EXEC SQL OPEN  PMOVDCE    END-EXEC.

           PERFORM L05-FETCH-PMOVDCE
              THRU L05-EX.

           MOVE ZEROES        TO WS-TOTCVLDC
                                 WS-TOTCIVDC.
           PERFORM D05-ELABORA-PMOVDCE
              THRU D05-EX
             UNTIL SQLCODE = +100.

           EXEC SQL CLOSE  PMOVDCE    END-EXEC.

           COMPUTE WS-CVLDIFDC = WS-CVSCANEW   - (WS-TOTCVLDC +
                                                  WS-TOTCIVDC)

           IF WS-CVLDIFDC NOT EQUAL ZEROES
              PERFORM D10-AGGIORNA-MOVIDCE
                 THRU D10-EX
              IF CAMB-DECICAMB = '0'
                 IF WS-CVLDIFDC GREATER 1 OR
                                LESS   -1
                 PERFORM M00-MESSAGGIO
                    THRU M00-EX
                 END-IF
              ELSE
                 IF WS-CVLDIFDC GREATER 0,01 OR
                                LESS   -0,01
                    PERFORM M00-MESSAGGIO
                       THRU M00-EX.

       C15-EX. EXIT.

      *--------------------------------------------------------------*
       C20-TESTATA.
      *--------------------------------------------------------------*

           MOVE TBST-CODSOCIE (WS-INDITABE) TO WS-CODSOCIE.
           MOVE '1'    TO WS-ASA
           STRING 'DIVISIONE - T - '
                  'SOCIETA'' ' WS-CODSOCIE ' ' WS-NOMSOCIE ' '
                  'RIEPILOGO DIFFERENZE CAMBI AL '
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

           IF TBST-CODCLIEN (WS-INDITABE) NOT = A901-CODCLIEN
              MOVE TBST-CODCLIEN (WS-INDITABE) TO A901-CODCLIEN
              PERFORM L10-LEGGI-CLIENTE
                 THRU L10-EX.

           COMPUTE WS-TOCVLPRI = WS-TOCVLPRI +
                   TBST-CVLPRIMA (WS-INDITABE).
           COMPUTE WS-TOCVLUTI = WS-TOCVLUTI +
                   TBST-CVLUTILE (WS-INDITABE).
           COMPUTE WS-TOCVLPER = WS-TOCVLPER +
                   TBST-CVLPERDI (WS-INDITABE).
           COMPUTE WS-TOCVLDOP = WS-TOCVLDOP +
                   TBST-CVLDOPOO (WS-INDITABE).
           MOVE   TBST-CVLPRIMA (WS-INDITABE) TO WS-CVLPRIED.
           MOVE   TBST-CVLDOPOO (WS-INDITABE) TO WS-CVLDOPED.
           MOVE   TBST-IMPMOVIM (WS-INDITABE) TO WS-IMPORTED.
           MOVE   SPACES                      TO WS-CVLUTIEX
                                                 WS-CVLPEREX.
           MOVE   TBST-CVLDIFFE (WS-INDITABE) TO WS-PEREDITT.

           IF TBST-CVLPERDI (WS-INDITABE) NOT = ZEROES
              MOVE TBST-CVLPERDI(WS-INDITABE) TO WS-CVLPERED.

           IF TBST-CVLUTILE (WS-INDITABE) NOT = ZEROES
              MOVE TBST-CVLUTILE(WS-INDITABE) TO WS-CVLUTIED.

           STRING TBST-CODCLIEN (WS-INDITABE) ' '
                  A901-RAGFATTU ' '
                  TBST-SIGLADIV (WS-INDITABE) ' '
                  WS-IMPORTED                 ' '
                  WS-CVLPRIED                 ' '
                  WS-CVLDOPED                 ' '
                  WS-CVLUTIED                 '  '
                  WS-CVLPERED                 '  '
                  WS-PEREDITT                 ' '
                  TBST-CENCOSTO (WS-INDITABE)
                  DELIMITED BY SIZE INTO WS-RIG.

           PERFORM W00-WRITE THRU W00-EX

           COMPUTE WS-CRIGSTAM = WS-CRIGSTAM + 1.

       C25-EX. EXIT.

      *--------------------------------------------------------------*
       D00-SISTEMA-TABEL.
      *--------------------------------------------------------------*

           IF C855-INDSEGNO = '-'
              COMPUTE WS-CVLDIFFE = WS-CVLDIFFE * -1.

           PERFORM E00-RIPARTIZ-CENCOSTO
              THRU E00-EX.

           MOVE A531-SOCIEGTM TO WS-CODSOCIE.
           MOVE A531-NOMSOCIE TO WS-NOMSOCIE.
           MOVE A963-SIGLADIV TO WS-SIGLADIV.
           MOVE C855-CODCLIEN TO WS-CODCLIEN.
           PERFORM E05-AGG-TABESTAMPA
              THRU E05-EX
           VARYING WS-INDICLIE FROM 1 BY 1
             UNTIL TBCL-CENCOSTO(WS-INDICLIE) = SPACES.

       D00-EX. EXIT.

      *--------------------------------------------------------------*
       D05-ELABORA-PMOVDCE.
      *--------------------------------------------------------------*

           MOVE A531-CODDIVRI TO WS-CODDIVRI.
           MOVE A531-CODDIVIS TO WS-CODDIVSO.
           MOVE C855-CODDIVIS TO WS-CODDIVIS.
           MOVE C858-IMPARMOV TO WS-IMPXCONV.
           MOVE C858-IMPORIVA TO WS-IVAXCONV.
           PERFORM C00-CHIAMA-SCBCB10
              THRU C00-EX.
           MOVE CAMB-CONTROVA(1) TO C858-CVLMOVIM.
           MOVE CAMB-CONTROVA(2) TO C858-CVLXXIVA.
           COMPUTE WS-TOTCVLDC = WS-TOTCVLDC + C858-CVLMOVIM.
           COMPUTE WS-TOTCIVDC = WS-TOTCIVDC + C858-CVLXXIVA.
           PERFORM E10-AGGIORNA-PMOVDCE
              THRU E10-EX.

           PERFORM L05-FETCH-PMOVDCE
              THRU L05-EX.

       D05-EX. EXIT.

      *--------------------------------------------------------------*
       D10-AGGIORNA-MOVIDCE.
      *--------------------------------------------------------------*

           IF SK-AGGIORNA = 'SI'
              PERFORM U10-UPDATE-MOVIDCE
                 THRU U10-EX.

       D10-EX. EXIT.

      *--------------------------------------------------------------*
       D15-TOTALI-SOCIETA.
      *--------------------------------------------------------------*

           PERFORM W00-WRITE THRU W00-EX 2 TIMES
           MOVE WS-DIFFEREN            TO WS-CVLTODIF.
           MOVE WS-TOCVLPRI            TO WS-CVLTOPRI.
           MOVE WS-TOCVLDOP            TO WS-CVLTODOP.
           MOVE WS-TOCVLUTI            TO WS-CVLTOUTI.
           MOVE WS-TOCVLPER            TO WS-CVLTOPER.
           MOVE WS-RIGATOT             TO WS-RIG
           PERFORM W00-WRITE THRU W00-EX.

           IF WS-DIFFEREN NOT EQUAL ZEROES
              PERFORM E15-ORDITABE
                 THRU E15-EX UNTIL WS-SWORDINA = 'NO'
              PERFORM E20-STAMPA-TABCENCO
                 THRU E20-EX
              MOVE 1 TO WS-INDITABE
              PERFORM E25-CREA-GTM
                 THRU E25-EX
                UNTIL TBST-CODCLIEN (WS-INDITABE) = SPACES.

       D15-EX. EXIT.

      *--------------------------------------------------------------*
       E00-RIPARTIZ-CENCOSTO.
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

       E00-EX. EXIT.

      *--------------------------------------------------------------*
       E05-AGG-TABESTAMPA.
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
              COMPUTE TBST-CVLUTILE (WS-INDITABE) =
                      TBST-CVLUTILE (WS-INDITABE) +
                      TBCL-CVLUTILE (WS-INDICLIE)
              COMPUTE TBST-CVLPERDI (WS-INDITABE) =
                      TBST-CVLPERDI (WS-INDITABE) +
                      TBCL-CVLPERDI (WS-INDICLIE).

       E05-EX. EXIT.

      *--------------------------------------------------------------*
       E10-AGGIORNA-PMOVDCE.
      *--------------------------------------------------------------*

           IF SK-AGGIORNA = 'SI'
              PERFORM U05-UPDATE-PMOVDCE
                 THRU U05-EX.

       E10-EX. EXIT.

      *--------------------------------------------------------------*
       E15-ORDITABE.
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

       E15-EX. EXIT.

      *--------------------------------------------------------------*
       E20-STAMPA-TABCENCO.
      *--------------------------------------------------------------*

           PERFORM VARYING WS-INDITABE FROM 1 BY 1
             UNTIL TBCC-CENCOSTO(WS-INDITABE) = SPACES
                   COMPUTE TBCC-CVLDIFFE (WS-INDITABE) =
                           TBCC-CVLUTILE (WS-INDITABE) -
                           TBCC-CVLPERDI (WS-INDITABE)
           END-PERFORM.

           MOVE ZEROES TO WS-TOCVLPRI
                          WS-TOCVLDOP
                          WS-TOCVLUTI
                          WS-TOCVLPER
                          WS-DIFFERCC.

           MOVE 60 TO WS-CRIGSTAM.
           PERFORM F10-RIEPILOGO-CENCOSTO
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
           MOVE WS-TOCVLUTI            TO WS-CVLTOUTI.
           MOVE WS-TOCVLPER            TO WS-CVLTOPER.
           STRING 'TOTALE' WS-CVLTOPRI ' ' WS-CVLTODOP ' '
                  WS-CVLTOUTI ' ' WS-CVLTOPER '  '
                  WS-CVLTODIF ' ***'
                  DELIMITED BY SIZE INTO WS-RIG.
           PERFORM W00-WRITE THRU W00-EX.

       E20-EX. EXIT.

      *--------------------------------------------------------------*
       E25-CREA-GTM.
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
              PERFORM F35-SCARICA-TABECLIE
                 THRU F35-EX.

       E25-EX. EXIT.

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
       F05-CREA-RECORD-DUE-DOPPIO.
      *--------------------------------------------------------------*

           IF TBCL-CVLDIFFE (WS-INDICLIE) NOT EQUAL ZEROES
              MOVE 'DIFCAM'      TO FINA-CONTODCE
              PERFORM F00-CREA-RECORD-DUE
                 THRU F00-EX
              IF FINA-INDSEGN2 = '+'
                 MOVE '-' TO FINA-INDSEGN2
              ELSE
                 MOVE '+' TO FINA-INDSEGN2
              END-IF
              MOVE C861-CONTODCE TO FINA-CONTODCE
              PERFORM S00-SCRIVI-FILEOUT
                 THRU S00-EX.

       F05-EX. EXIT.

      *--------------------------------------------------------------*
       F10-RIEPILOGO-CENCOSTO.
      *--------------------------------------------------------------*

           IF WS-CRIGSTAM > 57
              PERFORM G00-TESTATA-CENCOSTO
                 THRU G00-EX.

           IF TBCC-CVLDIFFE (WS-INDITABE) NOT = ZEROES
              PERFORM G05-STAMPA-CENCOSTO
                 THRU G05-EX.

       F10-EX. EXIT.

      *--------------------------------------------------------------*
       F15-CENCOS-MOVIM.
      *--------------------------------------------------------------*

           MOVE C852-CENCOSTO TO WS-CENCOSTO.

           IF WS-CENCOSTO NOT GREATER SPACES
              PERFORM L20-CERCA-CENCOSTO
                 THRU L20-EX.

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

           PERFORM L25-LEGGI-SICONOOK
              THRU L25-EX.

           IF SQLCODE NOT = +100
              PERFORM L30-LEGGI-SICOMAXAN
                 THRU L30-EX
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
           COMPUTE TBCC-CVLUTILE (WS-INDITABE) =
                   TBCC-CVLUTILE (WS-INDITABE) +
                   TBCL-CVLUTILE (WS-INDICLIE).
           COMPUTE TBCC-CVLPERDI (WS-INDITABE) =
                   TBCC-CVLPERDI (WS-INDITABE) +
                   TBCL-CVLPERDI (WS-INDICLIE).

       F30-EX. EXIT.

      *--------------------------------------------------------------*
       F35-SCARICA-TABECLIE.
      *--------------------------------------------------------------*

           INITIALIZE FINA-RECORDXX.
           MOVE A531-SOCIEGTM       TO FINA-CODSOCIE.
           MOVE 'T'                 TO FINA-DIVISION.
           IF FINA-CODSOCIE = '22' OR 'BS'
              MOVE 'A'              TO FINA-DIVISION.
           IF FINA-CODSOCIE = 'EG' OR 'DQ'
              MOVE 'V'              TO FINA-DIVISION.

           MOVE 'C'                 TO FINA-CODICECL.
           MOVE 'S'                 TO FINA-ACQUVEND
                                       FINA-TIPODOCU.
           MOVE ZEROES              TO FINA-NRDOCUME
           MOVE WS-CODCLIEN         TO FINA-CLIECONS.
           MOVE WS-MAXIDATA (3:2)   TO FINA-DTDOCUAA.
           MOVE WS-MAXIDATA (5:2)   TO FINA-DTDOCUMM.
           MOVE WS-MAXIDATA (7:2)   TO FINA-DTDOCUGG.
           MOVE FINA-DTDOCUAA (2:1) TO FINA-COMPETAA.
           MOVE '13'                TO FINA-COMPETMM.
           MOVE '1'                 TO FINA-TIPORECO.
           MOVE 'E'                 TO FINA-ESTEINTE.
           MOVE 'SR  '              TO FINA-CODIMOVI.
           MOVE 'CLIE'              TO FINA-CODIMOVI.
           MOVE WS-SIGLADIV         TO FINA-SIGLADIV.
           MOVE WS-DIFFEREN         TO WS-DIFFEDEC.

           IF SK-TIPOREGI = 'U'
              MOVE ZEROES           TO FINA-CVLMOVIM
                                       FINA-IMPMOVIM
              MOVE '+'              TO FINA-INDSEGN1
           ELSE
              IF WS-DIFFEREN LESS ZERO
                 MOVE '-'              TO FINA-INDSEGN1
              ELSE
                 MOVE '+'              TO FINA-INDSEGN1
              END-IF
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

           IF SK-TIPOREGI = 'M'
              MOVE 'DIFCAM'      TO FINA-CONTODCE
              PERFORM F00-CREA-RECORD-DUE
                 THRU F00-EX
              VARYING WS-INDICLIE FROM 1 BY 1
                UNTIL WS-INDICLIE GREATER 50 OR
                      TBCL-CENCOSTO(WS-INDICLIE) NOT GREATER SPACES
           ELSE
              PERFORM F05-CREA-RECORD-DUE-DOPPIO
                 THRU F05-EX
              VARYING WS-INDICLIE FROM 1 BY 1
                UNTIL WS-INDICLIE GREATER 50 OR
                      TBCL-CENCOSTO(WS-INDICLIE) NOT GREATER SPACES.

       F35-EX. EXIT.

      *--------------------------------------------------------------*
       G00-TESTATA-CENCOSTO.
      *--------------------------------------------------------------*

           MOVE '1' TO WS-ASA
           STRING 'DIVISIONE - T - '
                  'SOCIETA'' ' WS-CODSOCIE ' ' WS-NOMSOCIE ' '
                  'RIEPILOGO DIFFERENZE CAMBI AL '
                  WS-MAXIDATA (7:2) '/' WS-MAXIDATA (5:2) '/'
                  WS-MAXIDATA (1:4) ' PER CENTRO DI COSTO'
                  DELIMITED BY SIZE INTO WS-RIG.
           PERFORM W00-WRITE THRU W00-EX

           PERFORM W00-WRITE THRU W00-EX

           STRING 'C/COS '
                  '-CTVL.PRIMA- '
                  '-CTVL. DOPO- '
                  '----UTILE--- '
                  '---PERDITA-- '
                  'DIFF. CAMBIO '
                  DELIMITED BY SIZE INTO WS-RIG.
           PERFORM W00-WRITE THRU W00-EX

           PERFORM W00-WRITE THRU W00-EX

           MOVE 5      TO WS-CRIGSTAM.

       G00-EX. EXIT.

      *--------------------------------------------------------------*
       G05-STAMPA-CENCOSTO.
      *--------------------------------------------------------------*

           COMPUTE WS-TOCVLPRI = WS-TOCVLPRI +
                                 TBCC-CVLPRIMA (WS-INDITABE).
           COMPUTE WS-TOCVLDOP = WS-TOCVLDOP +
                                 TBCC-CVLDOPOO (WS-INDITABE).
           COMPUTE WS-TOCVLPER = WS-TOCVLPER +
                                 TBCC-CVLPERDI (WS-INDITABE).
           COMPUTE WS-TOCVLUTI = WS-TOCVLUTI +
                                 TBCC-CVLUTILE (WS-INDITABE).
           COMPUTE WS-DIFFERCC = WS-DIFFERCC +
                                 TBCC-CVLDIFFE (WS-INDITABE).
           MOVE   TBCC-CVLPRIMA (WS-INDITABE) TO WS-CVLPRIED.
           MOVE   TBCC-CVLDOPOO (WS-INDITABE) TO WS-CVLDOPED.
           MOVE   TBCC-CVLDIFFE (WS-INDITABE) TO WS-PEREDITT.
           MOVE   SPACES                      TO WS-CVLUTIEX
                                                 WS-CVLPEREX.

           IF TBCC-CVLPERDI (WS-INDITABE) NOT = ZEROES
              MOVE TBCC-CVLPERDI(WS-INDITABE) TO WS-CVLPERED.

           IF TBCC-CVLUTILE (WS-INDITABE) NOT = ZEROES
              MOVE TBCC-CVLUTILE(WS-INDITABE) TO WS-CVLUTIED.

           STRING TBCC-CENCOSTO(WS-INDITABE)  ' '
                  WS-CVLPRIED                 ' '
                  WS-CVLDOPED                 ' '
                  WS-CVLUTIED                 '  '
                  WS-CVLPERED                 '  '
                  WS-PEREDITT                 ' '
                  DELIMITED BY SIZE INTO WS-RIG.

           PERFORM W00-WRITE THRU W00-EX.

           COMPUTE WS-CRIGSTAM = WS-CRIGSTAM + 1.

       G05-EX. EXIT.

      *--------------------------------------------------------------*
       G10-UNCENCOSTO.
      *--------------------------------------------------------------*
           IF C855-INDSEGNO = '+'
              COMPUTE TBCL-CVLPRIMA (1) =
                      TBCL-CVLPRIMA (1) + WS-CVLSCOLD
              COMPUTE TBCL-CVLDOPOO (1) =
                      TBCL-CVLDOPOO (1) + C855-CVLMOVIM
              COMPUTE TBCL-IMPMOVIM (1) =
                      TBCL-IMPMOVIM (1) + C855-IMPMOVIM
           ELSE
              COMPUTE TBCL-CVLPRIMA (1) =
                      TBCL-CVLPRIMA (1) - WS-CVLSCOLD
              COMPUTE TBCL-CVLDOPOO (1) =
                      TBCL-CVLDOPOO (1) - C855-CVLMOVIM
              COMPUTE TBCL-IMPMOVIM (1) =
                      TBCL-IMPMOVIM (1) - C855-IMPMOVIM.

           IF WS-CVLDIFFE GREATER ZEROES
              MOVE WS-CVLDIFFE TO TBCL-CVLUTILE (1)
           ELSE
              MOVE WS-CVLDIFFE TO TBCL-CVLPERDI (1).

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

           PERFORM L35-FETCH-PSICOPL
              THRU L35-EX.

           IF SQLCODE = +0
              MOVE ZEROES TO WS-INSCOCON
              PERFORM H00-ESEGUI-CALCOPER
                 THRU H00-EX
              VARYING WS-INDICLIE FROM 1 BY 1
                UNTIL SQLCODE = +100 OR
                      WS-INDICLIE > 50.

           IF WS-INDICLIE > 50
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
                      MOVE WS-COMDIFFE TO TBCL-CVLUTILE(WS-INDICLIE)
                   ELSE
                      MOVE WS-COMDIFFE TO TBCL-CVLPERDI(WS-INDICLIE)
                   END-IF
           END-PERFORM.

       G15-EX. EXIT.

      *--------------------------------------------------------------*
       H00-ESEGUI-CALCOPER.
      *--------------------------------------------------------------*

           COMPUTE WS-PARCVLPR = (WS-CVLSCOLD   * C200-INSCOCON) / 100.
           COMPUTE WS-PARCVLDO = (C855-CVLMOVIM * C200-INSCOCON) / 100.
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

           PERFORM L35-FETCH-PSICOPL
              THRU L35-EX.

       H00-EX. EXIT.

      *--------------------------------------------------------------*
       H05-ELABORA-DIFFERENZE.
      *--------------------------------------------------------------*

           COMPUTE WS-TCVLPRIM = WS-CVLSCOLD   - WS-TCVLPRIM.
           COMPUTE WS-TCVLDOPO = C855-CVLMOVIM - WS-TCVLDOPO.
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
                       :C855-CVLXXIVA,:C852-CENCOSTO,:A963-SIGLADIV
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
       L05-FETCH-PMOVDCE.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH PMOVDCE
                 INTO :C858-IMPARMOV,:C858-CVLMOVIM,:C858-CONTODCE,
                      :C858-IMPORIVA,:C858-CVLXXIVA
           END-EXEC.

           IF SQLCODE = ZERO
              MOVE 'U'            TO PPROUTIN
              MOVE 'D'            TO PPTPFILE
              MOVE 'R'            TO PPTPOPER
              MOVE 'PMOVDCE'      TO PPNMFILE
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
                SELECT RAGFATTU, PAESFATT
                  INTO :A901-RAGFATTU, :A901-PAESFATT
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
       L20-CERCA-CENCOSTO.
      *----------------------------------------------------------------*

           MOVE 'L20-' TO WCOM-CODABEND.
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

       L20-EX. EXIT.

      *----------------------------------------------------------------*
       L25-LEGGI-SICONOOK.
      *----------------------------------------------------------------*

           MOVE 'L25-' TO WCOM-CODABEND.
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

       L25-EX. EXIT.

      *----------------------------------------------------------------*
       L30-LEGGI-SICOMAXAN.
      *----------------------------------------------------------------*

           MOVE 'L30-' TO WCOM-CODABEND.
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

       L30-EX. EXIT.

      *----------------------------------------------------------------*
       L35-FETCH-PSICOPL.
      *----------------------------------------------------------------*

           MOVE 'L35-'                  TO WCOM-CODABEND.
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

       L35-EX. EXIT.

      *----------------------------------------------------------------*
       M00-MESSAGGIO.
      *----------------------------------------------------------------*

           MOVE WS-CVLDIFDC TO WS-CVLDIFED.
           MOVE C855-CVLMOVIM TO WS-CVLMOVED.
           STRING C858-CODSOCIE ' ' C858-CDMOVCON ' ' C858-NRMOVCON
              ' ' C858-DTREGMOV ' ' C858-CODCLIEN ' ' C858-DTSCADEN
              ' ' C858-CONTODCE ' ' C858-OLDSCADE
              ' DIFF.===> ' WS-CVLDIFED
              ' TOT. MOVISCAD===> ' WS-CVLMOVED
              DELIMITED BY SIZE INTO PPMESSAG.
           MOVE 'M' TO PPROUTIN
           PERFORM Z10-READPRIN
              THRU Z10-EX.

       M00-EX. EXIT.

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
                   SET CVLMOVIM = :C855-CVLMOVIM,
                       CVLXXIVA = :C855-CVLXXIVA
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
       U05-UPDATE-PMOVDCE.
      *--------------------------------------------------------------*

           MOVE 'U05-'                 TO WCOM-CODABEND.
           MOVE 'UPDATE'               TO WCOM-FILEFUNZ.
           MOVE 'PMOVDCE'              TO WCOM-FILENOME.
           MOVE SPACES                 TO WCOM-FILECTRL.
           STRING C858-CODSOCIE ' ' C858-CDMOVCON ' '
                  C858-NRMOVCON ' ' C858-DTREGMOV ' '
                  C858-CODCLIEN ' '
              DELIMITED BY SIZE      INTO WCOM-FILECTRL.

           EXEC SQL
                UPDATE DCONTCLI.TMOVIDCE
                   SET CVLMOVIM = :C858-CVLMOVIM,
                       CVLXXIVA = :C858-CVLXXIVA
                 WHERE CURRENT OF PMOVDCE
           END-EXEC.

           IF SQLCODE NOT = +0
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

           MOVE 'U'            TO PPROUTIN.
           MOVE 'D'            TO PPTPFILE.
           MOVE 'U'            TO PPTPOPER.
           MOVE 'PMOVDCE'      TO PPNMFILE.
           PERFORM Z10-READPRIN
               THRU Z10-EX.

       U05-EX. EXIT.

      *--------------------------------------------------------------*
       U10-UPDATE-MOVIDCE.
      *--------------------------------------------------------------*

           MOVE 'U10-'                 TO WCOM-CODABEND.
           MOVE 'UPDATE'               TO WCOM-FILEFUNZ.
           MOVE 'MDCEQUA'              TO WCOM-FILENOME.
           MOVE SPACES                 TO WCOM-FILECTRL.
           STRING C858-CODSOCIE ' ' C858-CDMOVCON ' '
                  C858-NRMOVCON ' ' C858-DTREGMOV ' '
                  C858-CODCLIEN ' ' C858-CONTODCE ' '
                  C858-DTSCADEN ' ' C858-OLDSCADE ' '
              DELIMITED BY SIZE      INTO WCOM-FILECTRL.

           EXEC SQL
                UPDATE DCONTCLI.TMOVIDCE
                   SET CVLMOVIM = (CVLMOVIM + :WS-CVLDIFDC),
                       CVLXXIVA = (CVLXXIVA + :WS-CVLDIVDC)
                 WHERE CODSOCIE = :C858-CODSOCIE
                   AND CDMOVCON = :C858-CDMOVCON
                   AND NRMOVCON = :C858-NRMOVCON
                   AND DTREGMOV = :C858-DTREGMOV
                   AND CODCLIEN = :C858-CODCLIEN
                   AND DTSCADEN = :C858-DTSCADEN
                   AND CONTODCE = :C858-CONTODCE
                   AND OLDSCADE = :C858-OLDSCADE
           END-EXEC.

           IF SQLCODE NOT = +0
              PERFORM Z00-ERRORE
                 THRU Z00-EX.

           MOVE 'U'            TO PPROUTIN.
           MOVE 'D'            TO PPTPFILE.
           MOVE 'U'            TO PPTPOPER.
           MOVE 'MDCEQUA'      TO PPNMFILE.
           PERFORM Z10-READPRIN
               THRU Z10-EX.

       U10-EX. EXIT.

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

