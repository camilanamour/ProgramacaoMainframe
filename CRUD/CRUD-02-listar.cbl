      ******************************************************************
      * Author: CAMILA CECILIA
      * Date: 27 OUT 2021
      * Purpose: LISTAR PRODUTOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUD-02-listar.

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
       77 WS-FS                PIC 99.
       77 WS-EOF               PIC 99.
       77 WS-CONTADOR          PIC 99.

       01 WS-PRODUTO.
           03 WS-COD-PRODUTO       PIC 9(03).
           03 WS-NOME-PRODUTO      PIC X(20).
           03 WS-ESTOQUE-PRODUTO   PIC 9(09).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN INPUT PRODUTOS
            DISPLAY "----------- LISTAGEM DE PRODUTOS -----------"
            SET WS-EOF     TO 0.
            SET  WS-FS     TO 0.
            INITIALISE WS-CONTADOR

            PERFORM UNTIL WS-EOF EQUAL 1
               READ PRODUTOS INTO WS-PRODUTO
               AT END
                   IF WS-CONTADOR = 0 THEN
                       DISPLAY 'NENHUM REGISTRO'
                   END-IF
                   MOVE 1 TO WS-EOF
               NOT AT END
                   DISPLAY ' '
                   DISPLAY 'COD. PRODUTO: ' WS-COD-PRODUTO
                   DISPLAY 'NOME PRODUTO: ' WS-NOME-PRODUTO
                   DISPLAY 'ESTOQUE PRODUTO: ' WS-ESTOQUE-PRODUTO
                   ADD 1 TO WS-CONTADOR
               END-READ
            END-PERFORM.

            DISPLAY ' '
            DISPLAY 'QUANTIDADE DE PRODUTOS CADASTRADOS: ' WS-CONTADOR

            CLOSE PRODUTOS.
            STOP RUN.
            END PROGRAM CRUD-02-listar.
