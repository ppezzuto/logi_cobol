SELECT CODCLIEN||SUBSTR(X'3B',1,1) AS CODCLIEN
      ,RAGFATTU||SUBSTR(X'3B',1,1) AS RAGFATTU
      ,SUBSTR(AAMMCOMP,1,6)||SUBSTR(X'3B',1,1) AS AAMMCOMP
      ,TIPSERVI||SUBSTR(X'3B',1,1) AS TIPSERVI
      ,CONTODCE||SUBSTR(X'3B',1,1) AS CONTODCE
      ,CODSERVI||SUBSTR(X'3B',1,1) AS CODSERVI
      ,DESSERVI||SUBSTR(X'3B',1,1) AS DESSERVI
      ,CENCOSTD||SUBSTR(X'3B',1,1) AS CENCOSTD
      ,CENCOSTA||SUBSTR(X'3B',1,1) AS CENCOSTA
      ,UNMISURA||SUBSTR(X'3B',1,1) AS UNMISURA
      ,SUBSTR(SUBSTR(DIGITS
                    (DECIMAL(QTAORDIN, 9, 2)), 1, 7)||','||
              SUBSTR(DIGITS
                    (DECIMAL(QTAORDIN, 9, 2)), 8, 2)
                   , 1 , 10 )||SUBSTR(X'3B',1,1)
                                   AS QTAORDIN
      ,SUBSTR(SUBSTR(DIGITS
                    (DECIMAL(PREZZORD, 11, 4)), 1, 7)||','||
              SUBSTR(DIGITS
                    (DECIMAL(PREZZORD, 11, 4)), 8, 4)
                   , 1 , 12 )||SUBSTR(X'3B',1,1)
                                   AS PREZZORD
      ,SUBSTR(SUBSTR(DIGITS
                    (DECIMAL(IMPORDIN, 13, 2)), 1, 11)||','||
              SUBSTR(DIGITS
                    (DECIMAL(IMPORDIN, 13, 2)), 12, 2)
                   , 1 , 14 )||SUBSTR(X'3B',1,1)
                                   AS IMPORDIN
      ,NOTENOTE||SUBSTR(X'3B',1,1) AS NOTENOTE
      ,SUBSTR(SPACE(200), 1, 110)  AS FILLER
  FROM (
SELECT O.CODCLIEN,C.RAGFATTU
      ,LEFT(O.DTFATTUR,6) AS AAMMCOMP
      ,O.TIPSERVI,O.CONTODCE
      ,R.CODSERVI,S.DESSERVI                                         
      ,R.CENCOSTD,R.CENCOSTA                                         
      ,R.UNMISURA,R.QTAORDIN,R.PREZZORD,R.IMPORDIN
      ,R.NOTENOTE
  FROM DLOGISTI.TORDILOGI    O                                       
      ,DLOGISTI.TRIGORDILOGI R                                       
      ,DLOGISTI.TSERVIZI     S                                       
      ,DANAGRAF.TCLIENTI     C
      ,DANAGRAF.TDATEELAB   D
 WHERE O.AANUMORD = R.AANUMORD                                       
   AND O.CODSOCIE = '22'                                             
   AND R.DTANNULL = ' '                                              
   AND R.CODSERVI = S.CODSERVI                                       
   AND O.CODCLIEN = C.CODCLIEN  
   AND D.SIGLDATA='MENSILE' 
   AND LEFT(O.DTFATTUR,4) = LEFT(D.DTINELAB,4)
   AND O.AANUMFAT > ' '  
  ORDER BY O.CODCLIEN,O.AAMMCOMP
   ) AS X
