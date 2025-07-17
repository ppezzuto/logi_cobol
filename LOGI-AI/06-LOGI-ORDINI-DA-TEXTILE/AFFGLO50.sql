       UPDATE DSKCOSTO.TCONTROLSKCOS A
          SET A.DATAESTR = HEX(CURRENT DATE)
             ,A.TIMEESTR = HEX(CURRENT TIME)
        WHERE A.CODSOCIE = '22'
          AND A.DATAESTR <= ' '
          AND A.DTINIPER  = (SELECT MIN(C.DTINIPER)
                               FROM DSKCOSTO.TCONTROLSKCOS C
                              WHERE A.CODSOCIE  = C.CODSOCIE
                                AND C.DATAESTR <= ' ')
          AND (A.DTFINPER =(SELECT B.DTINELAB
                              FROM DANAGRAF.TDATEELAB B
                             WHERE B.SIGLDATA = 'AAAAAAA'
                               AND NOT EXISTS
                           (SELECT C.DTINELAB
                              FROM DANAGRAF.TDATEELAB C
                             WHERE C.SIGLDATA = 'LOGISTI'))
                       OR
               A.DTFINPER =(SELECT B.DTINELAB
                              FROM DANAGRAF.TDATEELAB B
                             WHERE B.SIGLDATA = 'LOGISTI'))
