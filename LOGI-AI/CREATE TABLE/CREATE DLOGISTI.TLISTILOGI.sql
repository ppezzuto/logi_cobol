CREATE TABLE DLOGISTI.TLISTILOGI (
  CODCLIEN	CHARACTER(5)	NOT NULL,
  DTINILIS	CHARACTER(6)	NOT NULL,
  DTFINLIS	CHARACTER(6)	NOT NULL,
  CODSERVI	CHARACTER(6)	NOT NULL,
  TIPSERVI	CHARACTER(1)	NOT NULL	DEFAULT ,
  CODDIVIS	CHARACTER(2)	NOT NULL	DEFAULT ,
  PREZZLIS	DECIMAL(11, 4)	NOT NULL	DEFAULT ,
  PERCLIST	DECIMAL(5, 2)	NOT NULL	DEFAULT ,
  DTCREAZI	CHARACTER(8)	NOT NULL	DEFAULT ,
  DTULTVAR	CHARACTER(8)	NOT NULL	DEFAULT ,
  CODUTVAR	CHARACTER(6)	NOT NULL	DEFAULT 
  ) 
  IN DLOGISTI_TS
  COMPRESS YES ADAPTIVE
  ORGANIZE BY ROW;