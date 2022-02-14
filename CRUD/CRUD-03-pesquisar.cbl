      ******************************************************************
      * Author: CAMILA CECÍLIA
      * Date: 27 OUT. 2021
      * Purpose: CONSULTAR PRODUTO PELO CODIGO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUD-03-pesquisar.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUTOS ASSIGN TO
           'C:\ARQUIVOS\produtos.txt'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS COD-PRODUTO
           FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
      ***** NOME DO FILE-CONTROL ***************************************
       FD PRODUTOS.
       01 REG-PRODUTO.
           03 COD-PRODUTO      PIC 9(03).
           03 NOME-PRODUTO     PIC X(20).
           03 ESTOQUE-PRODUTO  PIC 9(09).

       WORKING-STORAGE SECTION.
       77 WS-FS                PIC 99.
       77 WS-VALIDAR           PIC 9(1).

       01 WS-PRODUTO.
           03 WS-COD-PRODUTO       PIC 9(03).
           03 WS-NOME-PRODUTO      PIC X(20).
           03 WS-ESTOQUE-PRODUTO   PIC 9(09).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN I-O PRODUTOS.
            DISPLAY "----------- CONSULTA DE PRODUTOS -----------"
            SET  WS-FS     TO 0.
            INITIALISE WS-VALIDAR
            INITIALISE COD-PRODUTO

            PERFORM UNTIL COD-PRODUTO IS POSITIVE
               DISPLAY 'INFORME O CODIGO DO PRODUTO: '
               ACCEPT COD-PRODUTO
            END-PERFORM

            READ PRODUTOS RECORD INTO WS-PRODUTO
               KEY IS COD-PRODUTO
                   INVALID KEY
                       IF WS-VALIDAR = 0 THEN
                           DISPLAY "NENHUM REGISTRO"
                       END-IF
                       DISPLAY 'CODIGO INVÁLIDO'
                   NOT INVALID KEY
                       DISPLAY 'COD. PRODUTO: ' WS-COD-PRODUTO
                       DISPLAY 'NOME PRODUTO: ' WS-NOME-PRODUTO
                       DISPLAY 'ESTOQUE PRODUTO: ' WS-ESTOQUE-PRODUTO
            END-READ.
            CLOSE PRODUTOS.
       END PROGRAM CRUD-03-pesquisar.
