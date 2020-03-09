      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     PRV00900.
       AUTHOR.                         AmarildoMB.
       DATE-WRITTEN.                   09 MAR 2020.
      *----------------------------------------------------------------*
      *REMARKS.
      *     *----------------------------------------------------------*
      *     *#NOME     : PRV00900 ---> Validar CPF / CNPJ              *
      *     *----------------------------------------------------------*
      *     *#TIPO     : COBOL - MICRO FOCUS - NET EXPRESS 5           *
      *     *----------------------------------------------------------*
      *     *#ANALISTA : AMARILDO MBORGES                              *
      *     *----------------------------------------------------------*
      *     *#FUNCAO   : Validar os campos CPF e CNPJ.                 *
      *     *----------------------------------------------------------*
      *     * VERSAO 01    -    Amarildo M Borges      -    09.03.2020 *
      *     *----------------------------------------------------------*
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
       01  WS-AUXILIARES.
           03 WSS-IND-N                PIC 9(002)          VALUE ZEROES.
           03 WSS-IND-O                PIC 9(002)          VALUE ZEROES.
           03 WSS-IND-P                PIC 9(002)          VALUE ZEROES.
           03 WSS-SOMA                 PIC 9(008)          VALUE ZEROES.
           03 WSS-NUMERO-F             PIC 9(015)          VALUE ZEROES.
           03 WSS-NUMERO               PIC 9(015)          VALUE ZEROES.
           03 WSS-NUMERO-R             REDEFINES WSS-NUMERO.
              05  WSS-NUMERO-T         PIC 9(001)  OCCURS 15 TIMES.
           03 WSS-PESOS                PIC X(028)          VALUE SPACES.
           03 WSS-PESOS-R              REDEFINES WSS-PESOS.
              05  WSS-PESOS-T          PIC 9(002)  OCCURS 14 TIMES.
           03 WSS-QUOCI                PIC 9(008)          VALUE ZEROES.
           03 WSS-RESTO                PIC 9(008)          VALUE ZEROES.
           03 WSS-MENSAGEM             PIC X(078)          VALUE SPACES.
           03 WSS-PESOS-CPF            PIC X(028)          VALUE
                                        '0000000011100908070605040302'.
           03 WSS-PESOS-CNPJ           PIC X(028)          VALUE
                                        '0706050403020908070605040302'.
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  LKS-PARAMETRO.
           03 FILLER                   PIC S9(04) COMP.
           03 LKS-PROGRAM              PIC  X(008).
           03 LKS-NUMERO-I             PIC  9(015).
           03 LKS-TIPO-CALCULO         PIC  X(004).
           03 LKS-RETORNO              PIC  9(001).
      *----------------------------------------------------------------*
      * LKS-NUMERO-I     = número informado
      * LKS-TIPO-CALCULO = CPF ou CNPJ
      * LKS-RETORNO      = 0 - codigo verificado está correto
      *                  = 1 - código verificado está com erro
      *----------------------------------------------------------------*
      *
      *================================================================*
       PROCEDURE                       DIVISION USING LKS-PARAMETRO.
      *================================================================*
       RT-PRINCIPAL                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM RT-INICIALIZAR      THRU RT-INICIALIZARX.
      *
           PERFORM RT-PROCESSAR        THRU RT-PROCESSARX.   
      *
           PERFORM RT-FINALIZAR        THRU RT-FINALIZARX.
      *
           GOBACK.

       RT-PRINCIPALX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-INICIALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ZEROES                 TO LKS-RETORNO.
      *
           MOVE LKS-NUMERO-I           TO WSS-NUMERO.
      *
       RT-INICIALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-PROCESSAR                    SECTION.
      *----------------------------------------------------------------*
      *
           EVALUATE LKS-TIPO-CALCULO
              WHEN 'CPF '
                    PERFORM RT-VALIDA-CPF
                                       THRU RT-VALIDA-CPFX
              WHEN 'CNPJ'
                    PERFORM RT-VALIDA-CNPJ
                                       THRU RT-VALIDA-CNPJX
           END-EVALUATE.
      *
       RT-PROCESSARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-VALIDA-CPF                   SECTION.
      *----------------------------------------------------------------*
      *
           MOVE WSS-PESOS-CPF          TO WSS-PESOS
           MOVE 05                     TO WSS-IND-N
           MOVE 06                     TO WSS-IND-P
           MOVE 13                     TO WSS-IND-O
           MOVE ZEROES                 TO WSS-SOMA
           PERFORM RT-CALC-DIGITO-1    THRU RT-CALC-DIGITO-1X.
      *
           MOVE 05                     TO WSS-IND-N
           MOVE 05                     TO WSS-IND-P
           MOVE 14                     TO WSS-IND-O
           MOVE ZEROES                 TO WSS-SOMA
           PERFORM RT-CALC-DIGITO-2    THRU RT-CALC-DIGITO-2X.
      *
       RT-VALIDA-CPFX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-VALIDA-CNPJ                  SECTION.
      *----------------------------------------------------------------*
      *
           MOVE WSS-PESOS-CNPJ         TO WSS-PESOS
           MOVE 01                     TO WSS-IND-N
           MOVE 02                     TO WSS-IND-P
           MOVE 13                     TO WSS-IND-O
           MOVE ZEROES                 TO WSS-SOMA
           PERFORM RT-CALC-DIGITO-1    THRU RT-CALC-DIGITO-1X.
      *
           MOVE 01                     TO WSS-IND-N
           MOVE 01                     TO WSS-IND-P
           MOVE 14                     TO WSS-IND-O
           MOVE ZEROES                 TO WSS-SOMA
           PERFORM RT-CALC-DIGITO-2    THRU RT-CALC-DIGITO-2X.
      *
       RT-VALIDA-CNPJX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-CALC-DIGITO-1                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ZEROES                 TO WSS-SOMA.
      *
           PERFORM UNTIL WSS-IND-N     GREATER WSS-IND-O
                   COMPUTE WSS-SOMA    EQUAL WSS-SOMA +
                                      (WSS-NUMERO-T (WSS-IND-N) *
                                       WSS-PESOS-T  (WSS-IND-P))
                   ADD 1               TO WSS-IND-N
                                          WSS-IND-P
           END-PERFORM.
      *
           DIVIDE WSS-SOMA             BY 11
                                       GIVING WSS-QUOCI
                                       REMAINDER WSS-RESTO.
      *
           IF WSS-RESTO                EQUAL 0 OR 1
              MOVE ZEROES              TO WSS-NUMERO-T (14)
           ELSE
              SUBTRACT WSS-RESTO       FROM 11 GIVING WSS-NUMERO-T (14)
           END-IF.
      *
       RT-CALC-DIGITO-1X.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-CALC-DIGITO-2                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ZEROES                 TO WSS-SOMA.
      *
           PERFORM UNTIL WSS-IND-N     GREATER WSS-IND-O
                   COMPUTE WSS-SOMA    EQUAL WSS-SOMA +
                                            (WSS-NUMERO-T (WSS-IND-N) *
                                             WSS-PESOS-T  (WSS-IND-P))
                   ADD 1               TO WSS-IND-N
                                          WSS-IND-P
           END-PERFORM.
      *
           DIVIDE WSS-SOMA             BY 11
                                       GIVING WSS-QUOCI
                                       REMAINDER WSS-RESTO.
      *
           IF WSS-RESTO                EQUAL 0 OR 1
              MOVE ZEROES              TO WSS-NUMERO-T (15)
           ELSE
              SUBTRACT WSS-RESTO       FROM 11 GIVING WSS-NUMERO-T (15)
           END-IF.
      *
       RT-CALC-DIGITO-2X.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-FINALIZAR                    SECTION.
      *----------------------------------------------------------------*
      *
           MOVE WSS-NUMERO             TO WSS-NUMERO-F.
      *
           IF LKS-NUMERO-I             EQUAL WSS-NUMERO-F
              MOVE 0                   TO LKS-RETORNO                
           ELSE                                    
              MOVE 1                   TO LKS-RETORNO                
           END-IF.                                  
      *                                    
       RT-FINALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                   F I M  D O  P R O G R A M A
      *----------------------------------------------------------------*