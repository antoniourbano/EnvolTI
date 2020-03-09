      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     PT000000.
       AUTHOR.                         AmarildoMB.
       DATE-WRITTEN.                   09 MAR 2020.
       DATE-COMPILED.
      *----------------------------------------------------------------*
      *REMARKS.                                                        *
      *     *----------------------------------------------------------*
      *     *#NOME     : PT000000 ---> Prova COBOL - Menu Principal    *
      *     *----------------------------------------------------------*
      *     *#TIPO     : COBOL - MICRO FOCUS - NET EXPRESS 5           *
      *     *----------------------------------------------------------*
      *     *#ANALISTA : AMARILDO MBORGES                              *
      *     *----------------------------------------------------------*
      *     *#FUNCAO   : ESTE PROGRAMA TEM COMO FUNCAO EXIBIR MENU E   *
      *     *            OPCOES INICIAIS DE NAVEGACAO PARA O USUARIO   *
      *     *----------------------------------------------------------*
      *     * VERSAO 01    -    Amarildo M Borges      -    09.03.2020 *
      *     *----------------------------------------------------------*
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
       77  FILLER                      PIC  X(032)         VALUE
           'III  WORKING STORAGE SECTION III'.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - AUXILIARES
      *----------------------------------------------------------------*
       01  WSS-AUXILIARES.
           03  WSS-ENT                  PIC  X(001)        VALUE SPACES.
           03  WSS-SAI                  PIC  X(001)        VALUE SPACES.
           03  WSS-VLT                  PIC  X(001)        VALUE SPACES.
           03  WSS-OPC                  PIC  9             VALUE ZEROS.
           03  WSS-CAD-CV               PIC  9             VALUE ZEROS.
           03  WSS-REL-CV               PIC  9             VALUE ZEROS.
           03  WSS-CAD-REL-EXC          PIC  9             VALUE ZEROS.
      *  ---> variaveis de tela
           03  WSS-LINHA                PIC  9(003) COMP-3 VALUE ZEROS.
           03  WSS-LIMPA-LINHA          PIC  X(080)        VALUE SPACES.
           03  WSS-TIT-CAD              PIC  X(026)        VALUE SPACES.
           03  WSS-TIT-CR               PIC  X(009)        VALUE SPACES.
      *    03  WSS-TMENSAGEM            PIC  X(068)        VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - DATA DO SISTEMA
      *----------------------------------------------------------------*
           03  WSS-DATA-SIS.
               05  WSS-DATA-SIS-DIA     PIC  X(002).
               05  WSS-DATA-SIS-MES     PIC  X(002).
               05  WSS-DATA-SIS-ANO     PIC  X(004).
      *
      *----------------------------------------------------------------*
      *    AREA PARA CHAMADA AO SUBPROGRAMA PT000001
      *    - Manutenção Cliente / Vendedores
      *----------------------------------------------------------------*
      *
       01  WSPT001C                    PIC X(08)           VALUE
                                                            'PT00001C'.
      *
       01  WSPT001V                    PIC X(08)           VALUE
                                                            'PT00001V'.
      *
      *----------------------------------------------------------------*
      *    AREA PARA CHAMADA AO SUBPROGRAMA PT000002
      *    - Relatorios Cliente / Vendedores
      *----------------------------------------------------------------*
      *
       01  WSPT002C                    PIC X(08)           VALUE
                                                            'PT00002C'.
      *
       01  WSPT002V                    PIC X(08)           VALUE
                                                            'PT00002V'.
      *
      *----------------------------------------------------------------*
      *    AREA PARA CHAMADA AO SUBPROGRAMA PT000003
      *    - Distribuição Cliente / Vendedores
      *----------------------------------------------------------------*
      *
       01  WSPT0003                    PIC X(08)           VALUE
                                                            'PT000003'.
      *
      *----------------------------------------------------------------*
      *    Parametros para programas chamados
      *  ---> variaveis comunicação entre programas
      *----------------------------------------------------------------*
      *
           COPY 'SUB-PRGS.CPY'         REPLACING ==::== BY == WSS ==.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'FFF  FIM DA WORKING-STORAGE  FFF'.
      *
      *----------------------------------------------------------------*
      *    DESCRICAO DE TELAS
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *    TELA PRINCIPAL - APRESENTACAO DE MENUS
      *----------------------------------------------------------------*
       01  TELA-PRINCIPAL.
           03  BLANK SCREEN.
           03  F   LINE 01 COL 02   VALUE 'Prova Tecnica'.
           03  F   LINE 01 COL 75   VALUE 'COBOL'.
           03  F   LINE 02 COL 01   VALUE '-----------------------------
      -        '---------------------------------------------------'.
           03  F   LINE 03 COL 02   VALUE 'Carteira de Clientes'.
           03  TL-PRINCIPAL-DATA.
               05  F LINE 03 COL 70 PIC 99  USING WSS-DATA-SIS-DIA.
               05  F LINE 03 COL 72 VALUE '/'.
               05  F LINE 03 COL 73 PIC 99  USING WSS-DATA-SIS-MES.
               05  F LINE 03 COL 75 VALUE '/'.
               05  F LINE 03 COL 76 PIC 9999 USING WSS-DATA-SIS-ANO.
           03  F   LINE 05 COL 28   VALUE  '..... MENU PRINCIPAL .....'
                                                                  BLINK.
           03  F   LINE 08 COL 28   VALUE  ' 1 - Cadastros '.
           03  F   LINE 10 COL 28   VALUE  ' 2 - Relatorios '.
           03  F   LINE 12 COL 28   VALUE  ' 3 - Executar '.
           03  F   LINE 14 COL 28   VALUE  ' 9 - Sair do Sistema '.
           03  F   LINE 18 COL 28   VALUE  ' Opcao:[ ]'.
           03  F   LINE 23 COL 01   VALUE '-----------------------------
      -        '---------------------------------------------------'.
      *
      *----------------------------------------------------------------*
      *    TELA CADASTROS - APRESENTACAO DE MENUS
      *----------------------------------------------------------------*
       01  TELA-CADASTROS-RELATORIOS.
           03 F LINE 05 COL 28       PIC X(26) USING WSS-TIT-CAD BLINk.
           03 F LINE 08 COL 28 VALUE ' 1 - '.
           03 F LINE 08 COL 33       PIC X(09) USING WSS-TIT-CR.
           03 F LINE 08 COL 42 VALUE ' Clientes'.
           03 F LINE 10 COL 28 VALUE ' 2 - '.
           03 F LINE 10 COL 33       PIC X(09) USING WSS-TIT-CR.
           03 F LINE 10 COL 42 VALUE ' Vendedores'.
           03 F LINE 14 COL 28 VALUE ' 9 - Voltar ao Menu Principal'.
           03 F LINE 18 COL 28 VALUE ' Opcao:[ ] '.
      *
      *----------------------------------------------------------------*
      *    TELA DIVERSOS MENSAGEMS
      *----------------------------------------------------------------*
       01  TELA-MENSAGENS.
      *  ---> Linha Limpa Mensagem
           03 LL-M LINE 24 COL 01      BLANK LINE.
      *
      *  ---> Linha Exibe Label
           03 LE-L LINE 24 COL 01      VALUE ' Mensagem: ' BLINK.
      *
      *  ---> Linha Exibe Mensagem
           03 LE-M LINE 24 COL 12      PIC  X(068) USING WSS-MENSAGEM.
      *
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
       RT-PRINCIPAL                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM RT-INICIALIZAR      THRU RT-INICIALIZARX.
      *
           PERFORM UNTIL WSS-CAD-REL-EXC
                                       EQUAL 9
               PERFORM RT-PROCESSAR    THRU RT-PROCESSARX
           END-PERFORM.
      *
           PERFORM RT-FINALIZAR        THRU RT-FINALIZARX.
      *
           STOP RUN.
      *
       RT-PRINCIPALX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-INICIALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  WSS-AUXILIARES
                                       WSS-DATA-SIS
              REPLACING ALPHANUMERIC   BY SPACES
                        NUMERIC        BY ZEROS.
      *
           MOVE FUNCTION CURRENT-DATE(7:2)
                                       TO  WSS-DATA-SIS-DIA.
           MOVE FUNCTION CURRENT-DATE(5:2)
                                       TO  WSS-DATA-SIS-MES.
           MOVE FUNCTION CURRENT-DATE(1:4)
                                       TO  WSS-DATA-SIS-ANO.
      *
       RT-INICIALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-PROCESSAR                    SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ZEROS                  TO WSS-CAD-REL-EXC.
      *
           MOVE 'Informe opcao desejada.'
                                       TO WSS-MENSAGEM.
      *
           DISPLAY TELA-PRINCIPAL
                   TELA-MENSAGENS.
           ACCEPT  WSS-CAD-REL-EXC     LINE 18 COL 36
                                            WITH PROMPT AUTO-SKIP.
           EVALUATE WSS-CAD-REL-EXC
               WHEN 1
                    PERFORM RT-CADASTROS
                                       THRU RT-CADASTROSX
               WHEN 2
                    PERFORM RT-RELATORIOS
                                       THRU RT-RELATORIOSX
               WHEN 3
                    PERFORM RT-EXECUTAR
                                       THRU RT-EXECUTARX
               WHEN 9
                    CONTINUE
               WHEN OTHER
                    MOVE 'Opcao invalida!!! Tente novamente.'
                                       TO WSS-MENSAGEM
                    DISPLAY TELA-MENSAGENS
                    ACCEPT WSS-CAD-REL-EXC
                                       LINE 18 COL 36
                                            WITH PROMPT AUTO-SKIP
           END-EVALUATE.
      *
       RT-PROCESSARX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    Chama subprograma para manutencao de CLIENTES / VENDEDORES    *
      *----------------------------------------------------------------*
       RT-CADASTROS                    SECTION.
      *
           MOVE ZEROS                  TO WSS-CAD-CV.
      *
           PERFORM RT-LIMPAR-TELA      THRU RT-LIMPAR-TELAX.
      *
           MOVE '   ..... CADASTROS .....  '
                                       TO WSS-TIT-CAD.
           MOVE 'Cadastrar'            TO WSS-TIT-CR.
      *
           MOVE 'Informe opcao desejada.'
                                       TO WSS-MENSAGEM.
      *
           DISPLAY TELA-CADASTROS-RELATORIOS
                   TELA-MENSAGENS.
           ACCEPT  WSS-CAD-CV          LINE 18 COL 36
                                            WITH PROMPT AUTO-SKIP.
      *
           EVALUATE WSS-CAD-CV
               WHEN 1
                    CALL WSPT001C      USING WSS-PARM
                    IF WSS-RETORNO     EQUAL '00'
                       PERFORM         RT-CADASTROS
                    ELSE
                       MOVE 'Erro de processamento programa PT00001C.'
                                       TO WSS-MENSAGEM
                       DISPLAY TELA-MENSAGENS
                       ACCEPT WSS-ENT  LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
                    END-IF
               WHEN 2
                    CALL WSPT001V      USING WSS-PARM
                    IF WSS-RETORNO     EQUAL '00'
                       PERFORM         RT-CADASTROS
                    ELSE
                       MOVE 'Erro de processamento programa PT00001V.'
                                       TO WSS-MENSAGEM
                       DISPLAY TELA-MENSAGENS
                       ACCEPT WSS-ENT  LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
                    END-IF
               WHEN 9
                    CONTINUE
               WHEN OTHER
                    MOVE 'Opcao invalida!!! Tente novamente.'
                                       TO WSS-MENSAGEM
                    DISPLAY TELA-MENSAGENS
                    ACCEPT WSS-CAD-CV  LINE 18 COL 360
                                            WITH PROMPT AUTO-SKIP
           END-EVALUATE.
      *
       RT-CADASTROSX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-RELATORIOS                   SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ZEROS                  TO WSS-REL-CV.
      *
           PERFORM RT-LIMPAR-TELA      THRU RT-LIMPAR-TELAX.
      *
           MOVE '  ..... RELATORIOS .....  '
                                       TO WSS-TIT-CAD
           MOVE 'Relatorio'            TO WSS-TIT-CR
      *
           MOVE 'Informe opcao desejada.'
                                       TO WSS-MENSAGEM.
      *
           DISPLAY TELA-CADASTROS-RELATORIOS
                   TELA-MENSAGENS.
           ACCEPT  WSS-REL-CV          LINE 18 COL 36
                                            WITH PROMPT AUTO-SKIP.
      *
           EVALUATE WSS-REL-CV
               WHEN 1
                    CALL WSPT002C      USING WSS-PARM
      *
                    IF WSS-RETORNO     EQUAL '00'
                       PERFORM         RT-RELATORIOS
                    ELSE
                       MOVE 'Erro de processamento programa PT00002C.'
                                       TO WSS-MENSAGEM
                       DISPLAY TELA-MENSAGENS
                       ACCEPT WSS-ENT  LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
                    END-IF
               WHEN 2
                    CALL WSPT002V      USING WSS-PARM
      *
                    IF WSS-RETORNO     EQUAL '00'
                       PERFORM         RT-RELATORIOS
                    ELSE
                       MOVE 'Erro de processamento programa PT00002V.'
                                       TO WSS-MENSAGEM
                       DISPLAY TELA-MENSAGENS
                       ACCEPT WSS-ENT  LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
                    END-IF
               WHEN 9
                    CONTINUE
               WHEN OTHER
                    MOVE 'Opcao invalida!!! Tente novamente.'
                                       TO WSS-MENSAGEM
                    DISPLAY TELA-MENSAGENS
                    ACCEPT WSS-REL-CV  LINE 18 COL 360
                                            WITH PROMPT AUTO-SKIP
           END-EVALUATE.
      *
       RT-RELATORIOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-EXECUTAR                     SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM RT-LIMPAR-TELA      THRU RT-LIMPAR-TELAX.
      *
           CALL WSPT0003               USING WSS-PARM.
      *
           IF WSS-RETORNO              EQUAL '00'
              MOVE 'DISTRIBUIÇÃO realizada com sucesso!!! Pres. qualquer
      -       ' tecla para sair'       TO WSS-MENSAGEM
           ELSE
              MOVE
                 'Erro de processamento de Distribuição (prg PT00003).'
                                       TO WSS-MENSAGEM
           END-IF.
      *
           DISPLAY TELA-MENSAGENS.
           ACCEPT WSS-ENT              LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP.
      *
       RT-EXECUTARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-LIMPAR-TELA                  SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 4                      TO WSS-LINHA.
      *
           PERFORM VARYING WSS-LINHA   FROM 04 BY 01
                   UNTIL   WSS-LINHA   EQUAL 22
              DISPLAY WSS-LIMPA-LINHA  LINE WSS-LINHA COL 1
           END-PERFORM.
      *
       RT-LIMPAR-TELAX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-FINALIZAR                    SECTION.
      *----------------------------------------------------------------*
      *
            MOVE ZEROS                 TO RETURN-CODE.
      *
       RT-FINALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                   F I M  D O  P R O G R A M A
      *----------------------------------------------------------------*
