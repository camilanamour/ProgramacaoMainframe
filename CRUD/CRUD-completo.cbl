      ******************************************************************
      * Author: CAMILA CECILIA
      * Date: 03 NOV 2021
      * Purpose: CRUD - COMPLETO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUD-completo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUTOS ASSIGN TO 'C:\ARQUIVOS\produtos.txt'
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS COD-PRODUTO
           FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
      ***** NOME DO FILE-CONTROL ***************************************
       FD PRODUTOS.
       01 REG-PRODUTO.
           03 COD-PRODUTO          PIC 9(03).
           03 NOME-PRODUTO         PIC X(20).
           03 ESTOQUE-PRODUTO      PIC 9(09).

       WORKING-STORAGE SECTION.
       77 WS-FS                    PIC 99.
      ****** CADASTRAR ********************************************************
       77 WS-RESP                  PIC A(1).
      ****** LISTAR ***********************************************************
       77 WS-EOF                   PIC 99.
       77 WS-CONTADOR              PIC 99.
      ****** CONSULTAR ********************************************************
       77 WS-VALIDAR               PIC 9(1).
      ****** ALTERAR **********************************************************
       77 WS-ALTERAR               PIC X VALUE SPACE.
      ****** EXCLUIR **********************************************************
       77 WS-EXCLUIR   PIC X VALUE SPACE.
      ****** ARQUIVO **********************************************************
       01 WS-PRODUTO.
           03 WS-COD-PRODUTO       PIC 9(03).
           03 WS-NOME-PRODUTO      PIC X(20).
           03 WS-ESTOQUE-PRODUTO   PIC 9(09).
      ****** VALIDAR VALORES **************************************************
       77 WS-COD-VALIDAR       PIC 9(03).
       77 WS-NOME-VALIDAR      PIC X(20).
       77 WS-ESTOQUE-VALIDAR   PIC 9(09).
      ****** MENU *************************************************************
       77  WS-ENQUANTO     PIC 9  VALUES ZEROS.

       PROCEDURE DIVISION.
      ********************************************************* MENU **********
       MAIN-PROCEDURE SECTION.
           PERFORM UNTIL WS-ENQUANTO EQUAL 6
               DISPLAY ' '
               DISPLAY '(1)CADASTRAR (2)LISTAR (3)CONSULTAR (4)ALTERAR '
               '(5)DELETAR (6)SAIR'
               ACCEPT WS-ENQUANTO
           EVALUATE WS-ENQUANTO
               WHEN 1
                   GO TO CADASTRAR
               WHEN 2
                   GO TO LISTAR
               WHEN 3
                   GO TO CONSULTAR
               WHEN 4
                   GO TO ALTERAR
               WHEN 5
                   GO TO DELETAR
               WHEN 6
                   DISPLAY 'FIM DO SISTEMA'
               WHEN OTHER
                   DISPLAY 'VALOR INVALIDO'
               END-EVALUATE
           END-PERFORM.
           STOP RUN.

      ************************************** CADASTRAR ************************
       CADASTRAR SECTION.
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
            CLOSE PRODUTOS
           GO TO MAIN-PROCEDURE.

      ********************************************** LISTAR *******************
       LISTAR SECTION.
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

            CLOSE PRODUTOS
           GO TO MAIN-PROCEDURE.

      ************************************************ CONSULTAR **************
       CONSULTAR SECTION.
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
            CLOSE PRODUTOS
           GO TO MAIN-PROCEDURE.

      ************************************************ ALTERAR ****************
       ALTERAR SECTION.
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
           GO TO MAIN-PROCEDURE.

      *********************************************** DELETAR *****************
       DELETAR SECTION.
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
           GO TO MAIN-PROCEDURE.

       END PROGRAM CRUD-completo.
