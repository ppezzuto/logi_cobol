CREATE TABLE DCONTCLI.TCONTIDCE (
  CONTODCE	CHARACTER(6)	NOT NULL,
  CODPIANO	CHARACTER(2)	NOT NULL,
  CONTPROV	CHARACTER(6)	NOT NULL,
  DESCRDCE	CHARACTER(25)	NOT NULL	DEFAULT ,
  ANNULLAM	CHARACTER(1)	NOT NULL	DEFAULT ,
  TIPCONTO	CHARACTER(2)	NOT NULL	DEFAULT ,
  CONTGRUP	CHARACTER(6)	NOT NULL	DEFAULT ,
  NOTECON1	CHARACTER(33)	NOT NULL	DEFAULT ,
  NOTECON2	CHARACTER(33)	NOT NULL	DEFAULT ,
  INDBILQU	CHARACTER(1)	NOT NULL	DEFAULT ,
  FLCOMMES	CHARACTER(1)	NOT NULL	DEFAULT 
  ) 
  IN DCONTCLI_TS
  COMPRESS YES ADAPTIVE
  ORGANIZE BY ROW;