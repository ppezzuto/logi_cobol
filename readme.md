# LOGI - Sistema di Logistica (COBOL)

Questo repository contiene il codice sorgente, le copybook, i job JCL e gli script SQL relativi al **sistema LOGI**, utilizzato per la gestione dei flussi logistici tra sistemi interni ed esterni all'azienda.

## 🎯 Obiettivo

Il sistema LOGI ha l'obiettivo di:
- gestire i flussi informativi di ordini, servizi, fatture e addebiti;
- normalizzare e validare i dati in ingresso;
- orchestrare l'interazione tra sistemi legacy (es. PRP, PRA, CICS) e sistemi esterni (FASHION, TEXTILE, TESI);
- generare output strutturati (file, aggiornamenti DB2) per altri sistemi aziendali o contabili.

## 🧩 Struttura del repository


## 🔄 Moduli funzionali

Il sistema LOGI è suddiviso in moduli, ciascuno responsabile di una specifica funzione:

| Modulo | Descrizione |
|--------|-------------|
| 01     | Servizi e ordini verso FASHION |
| 02     | Servizi verso TESI |
| 03     | Ordini da FASHION |
| 04     | **PRP - Prefatture Passive** (genera ordini passivi) |
| 05     | **PRA - Prefatture Attive** (genera ordini attivi) |
| 06     | Ordini da TEXTILE |
| 07     | Moduli CICS (interattivi) |
| 08     | Fatturazione logistica |
| 09     | Addebiti intercompany |
| 10     | Generazione file ordilogistica |
| 11     | Calcolo ratei passivi |

## 🧠 Caratteristiche tecniche

- Linguaggio: COBOL
- Database: DB2
- Scheduler: JCL batch
- Logging e tracciamento: routine `READPRIN`, `Z00-ERRORE`
- Accesso parametrico tramite SK-PARAMETRI
- Codifica tracciati tramite COPY

## 📁 File principali

- Programmi COBOL (es. `AFFGLO90`, `AVALO55.cblx`)
- Copybook per i tracciati file (`*.cpy`)
- Script di creazione tabelle DB2 (`CREATE TABLE/*.sql`)
- Diagrammi e analisi funzionale (`doc/`)

## 📌 Note

Il sistema è progettato per essere **modulare, riavviabile e robusto**:
- ogni modulo è indipendente e gestisce il proprio input/output;
- gli errori DB2 sono intercettati e gestiti;
- la tracciabilità è garantita con logging costante e parametri audit.

## 👤 Contatti

> Per domande o supporto sull'analisi COBOL, contattare il team tecnico interno o utilizzare GitHub Issues.
