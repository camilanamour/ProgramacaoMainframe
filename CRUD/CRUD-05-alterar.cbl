      ******************************************************************
      * Author: CAMILA CECILIA
      * Date: 03 NOV 2021
      * Purpose: ALTERAR REGISTRO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUD-05-alterar.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUTOS ASSIGN TO 'C:\ARQUIVOS\produtos.txt'
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
       77 WS-FS        PIC 99.
       77 WS-ALTERAR   PIC X VALUE SPACE.

       01 WS-PRODUTO.
           03 WS-COD-PRODUTO       PIC 9(03).
           03 WS-NOME-PRODUTO      PIC X(20).
           03 WS-ESTOQUE-PRODUTO   PIC 9(09).

       77 WS-COD-VALIDAR       PIC 9(03).
       77 WS-NOME-VALIDAR      PIC X(20).
       77 WS-ESTOQUE-VALIDAR   PIC 9(09).

       77  WS-ENQUANTO     PIC 9  VALUES ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN I-O PRODUTOS.
            DISPLAY '----- ALTERACAO DE PRODUTOS -----'
            DISPLAY 'INFORME O CODIGO DO PRODUTO:'
            ACCEPT COD-PRODUTO
            MOVE COD-PRODUTO TO WS-COD-VALIDAR
            IF WS-COD-VALIDAR IS NUMERIC THEN
               READ PRODUTOS RECORD INTO WS-PRODUTO
                   KEY IS COD-PRODUTO
                       INVALID KEY
                           DISPLAY 'CODIGO DO PRODUTO INVALIDO'
                       NOT INVALID KEY
                           DISPLAY 'CODIGO DO PRODUTO: ' WS-COD-PRODUTO
                           DISPLAY 'NOME DO PRODUTO: ' WS-NOME-PRODUTO
                           DISPLAY 'QTDE EM ESTOQUE: '
                           WS-ESTOQUE-PRODUTO
                           MOVE 'S' TO WS-ALTERAR
               END-READ
             ELSE
                  DISPLAY 'CODIGO INVALIDO'
             END-IF.

             IF WS-ALTERAR EQUAL 'S'
                PERFORM UNTIL WS-ENQUANTO EQUALS 1
                   DISPLAY 'INFORME O NOME DO PRODUTO: '
                   ACCEPT NOME-PRODUTO
                   MOVE NOME-PRODUTO TO WS-NOME-VALIDAR
                   IF WS-NOME-VALIDAR IS ALPHABETIC THEN
                      IF WS-NOME-VALIDAR EQUALS SPACES THEN
                         MOVE WS-NOME-PRODUTO TO NOME-PRODUTO
                      END-IF
                      MOVE 1 TO WS-ENQUANTO
                    END-IF
                 END-PERFORM
                 DISPLAY 'INFORME A QUANTIDADE EM ESTOQUE:'
                 ACCEPT ESTOQUE-PRODUTO
                 ADD ESTOQUE-PRODUTO TO WS-ESTOQUE-VALIDAR
                 IF WS-ESTOQUE-VALIDAR EQUALS ZEROS THEN
                      MOVE WS-ESTOQUE-PRODUTO TO ESTOQUE-PRODUTO
                 END-IF
                 REWRITE REG-PRODUTO
                 END-REWRITE

                 IF WS-NOME-VALIDAR EQUALS SPACES AND
                    WS-ESTOQUE-VALIDAR EQUALS ZEROS THEN
                    DISPLAY 'NADA FOI ALTERADO!'
                 ELSE
                    DISPLAY 'ALTERADO COM SUCESSO!'
                 END-IF
            END-IF.

            CLOSE PRODUTOS
            STOP RUN.
       END PROGRAM CRUD-05-alterar.
