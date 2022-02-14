      ******************************************************************
      * Author: CAMILA CECILIA
      * Date: 27 OUT 2021
      * Purpose: CADASTRAR PRODUTOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUD-01-cadastrar.

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
       77 WS-FS                PIC 99.
       77 WS-RESP              PIC A(1).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "----------- CADASTRO DE PRODUTOS -----------"
            SET  WS-FS     TO 0.
      ****** LEITURA E ESCRITA *****************************************
            OPEN I-O PRODUTOS
      ****** ARQUIVO NÃO EXISTE ****************************************
            IF WS-FS EQUALS 35 THEN
                OPEN OUTPUT PRODUTOS
            END-IF

            DISPLAY 'COMECAR REGISTROS? (S|N)'
            ACCEPT WS-RESP

            IF WS-FS EQUALS ZEROS
                PERFORM UNTIL WS-RESP = 'N'
                   DISPLAY 'INFORME O CODIGO DO PRODUTO:'
                   ACCEPT COD-PRODUTO
                   DISPLAY 'INFORME O NOME DO PRODUTO:'
                   ACCEPT NOME-PRODUTO
                   DISPLAY 'INFORME O QUANTIDADE ESTOQUE:'
                   ACCEPT ESTOQUE-PRODUTO
      ********* GRAVAÇÃO ***********************************************
                   WRITE REG-PRODUTO
                   DISPLAY ' '
                   IF WS-FS NOT EQUAL ZEROS
                       DISPLAY 'ERRO: NÃO GRAVOU O REGISTRO'
                       DISPLAY 'FILE STATUS: ' WS-FS
                   ELSE
                       DISPLAY 'REGISTRO GRAVADO COM SUCESSO!'
                   END-IF
                   DISPLAY ' '
                   DISPLAY 'INSERIR MAIS UM PRODUTO? (S|N)'
                   ACCEPT WS-RESP
                END-PERFORM
            ELSE
                DISPLAY 'ERRO AO CRIAR AQUIVO'
                DISPLAY 'FILE STATUS: ' WS-FS
            END-IF.
            CLOSE PRODUTOS.
            STOP RUN.
       END PROGRAM CRUD-01-cadastrar.
