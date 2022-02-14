      ******************************************************************
      * Author: CAMILA CECILIA
      * Date: 03 NOV. 2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUD-06-delete.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUTOS ASSIGN TO
           'C:\ARQUIVOS\produtos.txt'
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
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
       77 WS-EXCLUIR   PIC X VALUE SPACE.

       01 WS-PRODUTO.
           03 WS-COD-PRODUTO       PIC 9(03).
           03 WS-NOME-PRODUTO      PIC X(20).
           03 WS-ESTOQUE-PRODUTO   PIC 9(09).

       01 WS-VALIDAR.
           03 WS-COD-VALIDAR       PIC 9(03).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN I-O PRODUTOS
            DISPLAY '----- EXCLUSAO DE REGISTROS ------'
            DISPLAY 'INFORME O CODIGO DO PRODUTO:'
            ACCEPT COD-PRODUTO
            ADD COD-PRODUTO TO WS-COD-VALIDAR
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
                           MOVE 'S' TO WS-EXCLUIR
               END-READ
            ELSE
               DISPLAY 'CODIGO INVALIDO'
            END-IF.
            IF WS-EXCLUIR EQUAL 'S'
                DELETE PRODUTOS RECORD
                INVALID KEY
                   DISPLAY 'CÓDIGO DO PRODUTO INVALIDO'
                NOT INVALID KEY
                   DISPLAY 'REGISTRO DELETADO!'
                END-DELETE
            END-IF.

            CLOSE PRODUTOS.

            STOP RUN.
       END PROGRAM CRUD-06-delete.
