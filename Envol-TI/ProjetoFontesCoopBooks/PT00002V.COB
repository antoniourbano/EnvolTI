      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     PT000002.
       AUTHOR.                         AmarildoMB.
       DATE-WRITTEN.                   09 MAR 2020.
       DATE-COMPILED.
      *----------------------------------------------------------------*
      *REMARKS.
      *     *----------------------------------------------------------*
      *     *#NOME     : PT000002 ---> RELATORIO - CLIENTES / VENDEDOR *
      *     *----------------------------------------------------------*
      *     *#TIPO     : COBOL - MICRO FOCUS - NET EXPRESS 5           *
      *     *----------------------------------------------------------*
      *     *#ANALISTA : AMARILDO MBORGES                              *
      *     *----------------------------------------------------------*
      *     *#FUNÇÃO   : FORMATAR RELATORIO DE CLIENTES E VENDEDORES   *
      *     *----------------------------------------------------------*
      *     * VERSÃO 01    -    Amarildo M Borges      -    09.03.2020 *
      *     *----------------------------------------------------------*
      *                                                                *
      *     ARQUIVOS.....: ARQ-VENDEDOR - ARQUIVO DE VENDEDOR          *
      *                    SRT-VENDEDOR - ARQUIVO DE SORT              * 
      *                    REL-VENDEDOR - ARQUIVO DE RELATORIO         *
      *----------------------------------------------------------------*
      *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.                  DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
      *
      *  ---> Arquivo VENDEDOR
           COPY 'SEL-VENDEDOR.CPY'     REPLACING ==::== BY == -FDV ==.
      *
      *  ---> Arquivo SORT (Ordena dados)
           SELECT  SRT-VENDEDOR        ASSIGN TO 'SRT-VENDEDOR.TMP'
                   FILE STATUS         IS FS-VEN-SRT.
      *
      *  ---> Arquivo RELATORIO
           SELECT  REL-VENDEDOR        ASSIGN TO 'REL-VENDEDOR.TXT'
                   ORGANIZATION        IS LINE SEQUENTIAL
                   FILE STATUS         IS FS-VEN-REL.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-VENDEDOR                VALUE OF FILE-ID IS 'VENDEDOR'
           LABEL RECORDS               IS STANDARD
           DATA RECORD                 IS VEN-FDV.
       COPY 'FD-VENDEDOR.CPY'          REPLACING ==::== BY == -FDV ==.
      *
       SD  SRT-VENDEDOR.
       01  REG-SRTV.
           03  SRTV-CODIGO             PIC  9(003).
           03  SRTV-CPF                PIC  9(011).
           03  SRTV-NOME               PIC  X(040).
           03  SRTV-LATITUDE           PIC S9(003)V9(008).
           03  SRTV-LONGITUDE          PIC S9(003)V9(008).

       FD  REL-VENDEDOR.
       01  REG-REL-VENDEDOR            PIC  X(120).
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
       01  WSS-AUXILIARES.
           03  WSS-ENT                 PIC  X(001)         VALUE SPACES.
           03  WSS-IND                 PIC  9(002)         VALUE ZEROS.
           03  WSS-LINHA               PIC  9(003) COMP-3  VALUE ZEROS.
           03  WSS-LIMPA-LINHA         PIC  X(080)         VALUE SPACES.
           03  WSS-TITULO              PIC  X(012)         VALUE SPACES.
      *
      *  ---> Variaveis de tela.
           03  WSS-T-ORDENA            PIC  X(001)         VALUE SPACES.
           03  WSS-T-CLASSIF           PIC  X(001)         VALUE SPACES.
           03  WSS-T-FILTRO            PIC  9              VALUE ZEROS. S.
           03  WSS-T-CVENDEDOR         PIC  9(003)         VALUE ZEROS.
           03  WSS-T-NVENDEDOR         PIC  X(040)         VALUE SPACES.
      *
           03  WSS-FIM-ARQ             PIC  X(001)         VALUE SPACES.
           03  WSS-FIM-SORT            PIC  X(001)         VALUE SPACES.
           03  WSS-CPF-AUX             PIC  X(011)         VALUE SPACES.
           03  WSS-MENSAGEM            PIC  X(068)         VALUE SPACES.
      *
      *  ---> Variaveis relatorio.
           03  WSS-REL-NLIN            PIC  99             VALUE ZEROS.
           03  WSS-REL-NPAG            PIC  999            VALUE ZEROS.
      *
           03  WSS-REL-DAT.
               05  WSS-REL-DAT-DIA     PIC  99             VALUE ZEROS.
               05  WSS-REL-DAT-MES     PIC  99             VALUE ZEROS.
               05  WSS-REL-DAT-ANO     PIC  9999           VALUE ZEROS.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - FILE STATUS
      *----------------------------------------------------------------*
           03  FS-VENDEDOR             PIC  X(002)         VALUE SPACES.
           03  FS-VEN-SRT              PIC  X(002)         VALUE SPACES.
           03  FS-VEN-REL              PIC  X(002)         VALUE SPACES.
      *
       01  WSS-CPF-EDITADO.
           03  WSS-CPF-EDT-1           PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE ".".
           03  WSS-CPF-EDT-2           PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE ".".
           03  WSS-CPF-EDT-3           PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE "-".
           03  WSS-CPF-EDT-4           PIC  X(002)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *    AREA DE ARQUIVOS NA WORKING
      *----------------------------------------------------------------*
      *
      *  ---> VENDEDOR
       COPY 'FD-VENDEDOR.CPY'          REPLACING ==::== BY == -WSV ==.
      *
      *----------------------------------------------------------------*
      * AREA DE DECLARACAO DO LAY-OUT DO RELATORIO                     *
      *----------------------------------------------------------------
       01  CABECALHO.
           03  CAB-L1.
               05 FILLER               PIC  X(001)         VALUE SPACES.
               05 FILLER               PIC  X(014)         VALUE
                                           'PROVA TECNICA'.
               05 FILLER               PIC  X(084)         VALUE SPACES.
               05 FILLER               PIC  X(023)         VALUE
                                           'CARTEIRA DE CLIENTES'.
      *
           03  CAB-L2.
               05 FILLER               PIC  X(041)         VALUE SPACES.
               05 FILLER               PIC  X(024)         VALUE
                                           'R E L A T O R I O  D E  '.
               05 RLC-TITULO           PIC  X(020)         VALUE SPACES.
               05 FILLER               PIC  X(041)         VALUE SPACES.
      *
           03  CAB-L3.
               05 FILLER               PIC  X(002)         VALUE SPACES.
               05 FILLER               PIC  X(012)         VALUE
                                           'COD.VENDEDOR'.
               05 FILLER               PIC  X(002)         VALUE SPACES.
               05 FILLER               PIC  X(008)         VALUE
                                           'CPF'.
               05 FILLER               PIC  X(012)         VALUE SPACES.
               05 FILLER               PIC  X(012)         VALUE
                                           'NOME'.
               05 FILLER               PIC  X(035)         VALUE SPACES.
               05 FILLER               PIC  X(008)         VALUE
                                           'LATITUDE'.
               05 FILLER               PIC  X(006)         VALUE SPACES.
               05 FILLER               PIC  X(009)         VALUE
                                           'LONGITUDE'.
      *
       01  DETALHE.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  RLD-VCODIGO             PIC  9(007)         VALUE ZEROS.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  RLD-CPF                 PIC  X(018)         VALUE ZEROS.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  RLD-NOME                PIC  X(040)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  RLD-LATITUDE            PIC +ZZ9,99999999   VALUE ZEROS.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  RLD-LONGITUDE           PIC +ZZ9,99999999   VALUE ZEROS.
      *
       01  RODAPE.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(006)         VALUE
                                            'Data: '.
           03  RLR-REL-DAT-DIA         PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  RLR-REL-DAT-MES         PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  RLR-REL-DAT-ANO         PIC  9(004)         VALUE ZEROS.
           03  FILLER                  PIC  X(087)         VALUE SPACES.
           03  FILLER                  PIC  X(008)         VALUE
                                            'Pagina: '.
           03  RLR-PAGINA              PIC  9(003)         VALUE ZEROS.
      *
       01  RELATORIO-AUXILIARES.
           03 LINHA-TRACO.
              05 FILLER                PIC  X(120)         VALUE ALL
                                            '-'.
      *
           03 LINHA-BRANCO.
              05 FILLER                PIC  X(120)         VALUE ALL
                                            SPACES.
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  LKS-PARM.
           03  FILLER                  PIC S9(004) COMP.
           03  LKS-PARAM               PIC  X(001).
           03  LKS-RETORNO             PIC  X(002).
           03  LKS-MENSAGEM            PIC  X(050).
      *
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *    TELA IMPORT VENDEDOR - CADASTRO
      *----------------------------------------------------------------*
       01  TELA-TITULO-VENDEDOR.
           03 F LINE 05 COL 28         VALUE ' VENDEDOR -'        BLINK.
           03 F LINE 05 COL 39         PIC X(12) USING WSS-TITULO BLINK.
      *
       01  TELA-VENDEDOR-RELATORIO.
           03 TCR1 LINE 08 COL 10  VALUE 'Tipo de ordenacao: '.
           03 TCR2 LINE 09 COL 15  VALUE '<A>scendente <D>ecrescente'.
           03 TCR3 LINE 11 COL 10  VALUE 'Tipo de Classificacao: '.
           03 TCR4 LINE 12 COL 15  VALUE
                                   '<C>od Vendedor <N>ome Vendedor'.
           03 TCR5 LINE 14 COL 10  VALUE 'Filtra por: '.
           03 TCR6 LINE 16 COL 15  VALUE '1 - Codigo do Vendedor: '.
           03 TCR7 LINE 18 COL 15  VALUE '2 - Nome do Vendedor..: '.
      *
      *----------------------------------------------------------------*
      *    TELA MENSAGEMS
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
       PROCEDURE                       DIVISION USING LKS-PARM.
      *================================================================*
       RT-PRINCIPAL                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM RT-INICIALIZA       THRU RT-INICIALIZAX.
           PERFORM RT-PROCESSA         THRU RT-PROCESSAX.
           PERFORM RT-FINALIZA         THRU RT-FINALIZAX.
      *
       RT-PRINCIPALX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina de inicialização                                        *
      *----------------------------------------------------------------*
       RT-INICIALIZA                   SECTION.
      *
           INITIALIZE                  WSS-AUXILIARES
                                       REG-WSV
                                       LKS-PARM
              REPLACING ALPHANUMERIC   BY SPACES
                        NUMERIC        BY ZEROS.
      *
           MOVE 'RELATORIO GERADO COM SUCESSO'
                                       TO LKS-MENSAGEM.
      *
           MOVE ZEROS                  TO LKS-RETORNO.
      *
           PERFORM RT-ABRE-ARQUIVOS    THRU RT-ABRE-ARQUIVOSX.
      *
       RT-INICIALIZAX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Abertura de arquivos
      *----------------------------------------------------------------*
       RT-ABRE-ARQUIVOS                SECTION.
      *
           OPEN INPUT ARQ-VENDEDOR.
      *
           IF FS-VENDEDOR              EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO LKS-RETORNO
              MOVE 'ERRO NA ABERTURA DO ARQUIVO VENDEDORES'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZA      THRU RT-FINALIZAX
           END-IF.
      *
           OPEN OUTPUT REL-VENDEDOR.
      *
           IF FS-VEN-REL               EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO LKS-RETORNO
              MOVE 'ERRO NA ABERTURA DO ARQUIVO REL-VENDEDOR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZA      THRU RT-FINALIZAX
           END-IF.
      *
       RT-ABRE-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Processamento Principal                                        *
      *----------------------------------------------------------------*
       RT-PROCESSA                     SECTION.
      *
           PERFORM RT-LIMPAR-TELA      THRU RT-LIMPAR-TELAX.
      *
           MOVE '  RELATORIOS'         TO WSS-TITULO
      *
           DISPLAY TELA-TITULO-VENDEDOR.
      *
           MOVE 'Informe tipo de ordenação.'
                                       TO WSS-MENSAGEM.
      *
           DISPLAY TCR1
                   TCR2
                   TELA-MENSAGENS.
      *
           ACCEPT WSS-T-ORDENA           LINE 08 COL 29
                                            WITH PROMPT AUTO-SKIP.
      *
           MOVE 'Informe tipo de classificação.'
                                       TO WSS-MENSAGEM.
      *
           DISPLAY TCR3
                   TCR4
                   TELA-MENSAGENS.
      *
           ACCEPT WSS-T-CLASSIF          LINE 11 COL 33
                                            WITH PROMPT AUTO-SKIP.
      *
           MOVE 'Informe filtro para relatorio. <0> para sem filtros.'
                                       TO WSS-MENSAGEM.


           DISPLAY TCR5
                   TCR6
                   TCR7
                   TELA-MENSAGENS.
      *
       REC-FILTRO.
           ACCEPT WSS-T-FILTRO         LINE 14 COL 22
                                            WITH PROMPT AUTO-SKIP.
      *
           EVALUATE WSS-T-FILTRO
               WHEN 0
                    CONTINUE
               WHEN 1
                    ACCEPT WSS-T-CVENDEDOR
                                       LINE 16 COL 39
                                            WITH PROMPT AUTO-SKIP
               WHEN 2
                    ACCEPT WSS-T-NVENDEDOR
                                       LINE 18 COL 39
                                            WITH PROMPT AUTO-SKIP
               WHEN OTHER
                    MOVE
                   'Opcao invalida...Pressione qualquer tecla para sair'
                                       TO WSS-MENSAGEM
                    DISPLAY TELA-MENSAGENS
                    ACCEPT WSS-ENT     LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
                    MOVE ZEROS         TO WSS-T-FILTRO
                    PERFORM REC-FILTRO
           END-EVALUATE.
      *
           IF  WSS-T-ORDENA            EQUAL 'A' OR 'a'
               IF  WSS-T-CLASSIF       EQUAL 'C' OR 'c'
                   SORT SRT-VENDEDOR
                     ON ASCENDING      KEY SRTV-CODIGO
                     INPUT PROCEDURE   IS RT-INPUT-SORT
                     OUTPUT PROCEDURE  IS RT-OUTPUT-SORT
               ELSE
                   SORT SRT-VENDEDOR
                     ON ASCENDING      KEY SRTV-NOME
                     INPUT PROCEDURE   IS RT-INPUT-SORT
                     OUTPUT PROCEDURE  IS RT-OUTPUT-SORT
               END-IF
           ELSE
               IF  WSS-T-CLASSIF       EQUAL 'C' OR 'c'
                   SORT SRT-VENDEDOR
                     ON DESCENDING     KEY SRTV-CODIGO
                     INPUT PROCEDURE   IS RT-INPUT-SORT
                     OUTPUT PROCEDURE  IS RT-OUTPUT-SORT
               ELSE
                   SORT SRT-VENDEDOR
                     ON DESCENDING     KEY SRTV-NOME
                     INPUT PROCEDURE   IS RT-INPUT-SORT
                     OUTPUT PROCEDURE  IS RT-OUTPUT-SORT
               END-IF
           END-IF.
      *
       RT-PROCESSAX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina de INPUT do Sort                                        *
      *----------------------------------------------------------------*
       RT-INPUT-SORT                   SECTION.
      *
           PERFORM RT-LER-VENDEDOR     THRU RT-LER-VENDEDORX.
      *
           PERFORM
              UNTIL WSS-FIM-ARQ        EQUAL 'S' OR 's'
                 PERFORM RT-ALIMENTA-SORT
                                       THRU RT-ALIMENTA-SORTX
           END-PERFORM.
      *
       RT-INPUT-SORTX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina de alimentação do Sort                                  *
      *----------------------------------------------------------------*
       RT-ALIMENTA-SORT                SECTION.
      *
           IF WSS-T-CVENDEDOR          EQUAL ZEROS
              IF WSS-T-NVENDEDOR       EQUAL SPACES
                 RELEASE REG-SRTV      FROM REG-WSV
              ELSE
                 IF NOME-WSV        EQUAL WSS-T-NVENDEDOR
                    RELEASE REG-SRTV   FROM REG-WSV
                 END-IF
              END-IF
           ELSE
              IF CODIGO-WSV            EQUAL WSS-T-CVENDEDOR
                 RELEASE REG-SRTV      FROM REG-WSV
              END-IF
           END-IF
      *
           PERFORM RT-LER-VENDEDOR     THRU RT-LER-VENDEDORX.
      *
       RT-ALIMENTA-SORTX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina OUTPUT do Sort                                          *
      *----------------------------------------------------------------*
       RT-OUTPUT-SORT                  SECTION.
      *
           RETURN SRT-VENDEDOR         INTO REG-SRTV
               AT END
                  MOVE 'S'             TO WSS-FIM-SORT
           END-RETURN.
      *
           PERFORM
              UNTIL WSS-FIM-SORT       EQUAL 'S' or 's'
              PERFORM RT-GERA-RELATORIO
                                       THRU RT-GERA-RELATORIOX
           END-PERFORM.
      *
       RT-OUTPUT-SORTX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Geração do Relatório de Vendedores
      *----------------------------------------------------------------*
       RT-GERA-RELATORIO               SECTION.
      *
           EVALUATE WSS-REL-NLIN
               WHEN ZEROS
                    PERFORM RT-IMPRIME-REL-CAB
                                       THRU RT-IMPRIME-REL-CABX
                    MOVE 6             TO WSS-REL-NLIN
               WHEN GREATER ZEROS
                    PERFORM RT-IMPRIME-REL-DET
                                       THRU RT-IMPRIME-REL-DETX
      *
                    RETURN SRT-VENDEDOR
                                       INTO REG-SRTV
                        AT END
                           MOVE 'S'    TO WSS-FIM-SORT
                    END-RETURN
      *
                    ADD 1              TO WSS-REL-NLIN
               WHEN EQUAL 55
                    PERFORM RT-IMPRIME-REL-RDP
                                       THRU RT-IMPRIME-REL-RDPX
                    MOVE ZEROS         TO WSS-REL-NLIN
           END-EVALUATE.
      *
       RT-GERA-RELATORIOX.
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
      * Finaliza programa e devolve controle ao programa chamador
      *----------------------------------------------------------------*
       RT-FINALIZA                     SECTION.
      *
           PERFORM VARYING WSS-IND     FROM WSS-REL-NLIN   BY 1
                UNTIL WSS-IND          GREATER  54
                WRITE REG-REL-VENDEDOR FROM LINHA-BRANCO   AFTER 1
           END-PERFORM.
      *
           PERFORM RT-IMPRIME-REL-RDP  THRU RT-IMPRIME-REL-RDPX.
           MOVE ZEROS                  TO WSS-REL-NLIN.
      *
           PERFORM RT-FECHAR-ARQUIVOS  THRU RT-FECHAR-ARQUIVOSX.
      *
           GOBACK.
      *
       RT-FINALIZAX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Leitura arquivo Vendedor
      *----------------------------------------------------------------*
       RT-LER-VENDEDOR                 SECTION.
      *
           READ ARQ-VENDEDOR
                NEXT
                INTO REG-WSV
             AT END
                MOVE 'S'               TO WSS-FIM-ARQ
                PERFORM RT-INPUT-SORT
           END-READ.
      *
           IF FS-VENDEDOR              EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 10                  TO LKS-RETORNO
              MOVE 'ERRO NA LEITURA DO ARQUIVO ARQ-VENDEDOR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZA      THRU RT-FINALIZAX
           END-IF.
      *
       RT-LER-VENDEDORX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Fechamento de arquivos
      *----------------------------------------------------------------*
       RT-FECHAR-ARQUIVOS              SECTION.
      *
           CLOSE ARQ-VENDEDOR.
      *
           IF FS-VENDEDOR              EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO LKS-RETORNO
              MOVE 'ERRO NA ABERTURA DO ARQUIVO ARQ-VENDEDOR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZA      THRU RT-FINALIZAX
           END-IF.
      *
           CLOSE REL-VENDEDOR.
      *
           IF FS-VEN-REL               EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 14                  TO LKS-RETORNO
              MOVE 'ERRO NO FECHAMENTO DO ARQUIVO DE RELATORIO'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZA      THRU RT-FINALIZAX
           END-IF.
      *
       RT-FECHAR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Formatação de CPF
      *----------------------------------------------------------------*
       RT-CPF                          SECTION.
      *
           MOVE WSS-CPF-AUX(01:3)     TO WSS-CPF-EDT-1.
           MOVE WSS-CPF-AUX(05:3)     TO WSS-CPF-EDT-2.
           MOVE WSS-CPF-AUX(08:3)     TO WSS-CPF-EDT-3.
           MOVE WSS-CPF-AUX(10:2)     TO WSS-CPF-EDT-4.
      *
       RT-CPFX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Grava cabecalho relatório
      *----------------------------------------------------------------*
       RT-IMPRIME-REL-CAB              SECTION.
      *
           MOVE 'V E N D E D O R E S'  TO RLC-TITULO.
      *
           IF WSS-REL-NLIN             EQUAL ZEROS
              WRITE REG-REL-VENDEDOR   FROM LINHA-TRACO AFTER 1
           ELSE
              WRITE REG-REL-VENDEDOR   AFTER ADVANCING PAGE
              WRITE REG-REL-VENDEDOR   FROM LINHA-TRACO AFTER 1
           END-IF.
      *
           WRITE REG-REL-VENDEDOR      FROM CAB-L1 AFTER 2.
           WRITE REG-REL-VENDEDOR      FROM CAB-L2 AFTER 1.
           WRITE REG-REL-VENDEDOR      FROM LINHA-BRANCO AFTER 1.
           WRITE REG-REL-VENDEDOR      FROM CAB-L3 AFTER 1.
           WRITE REG-REL-VENDEDOR      FROM LINHA-TRACO AFTER 1.
      *
           ADD 1                       TO WSS-REL-NPAG.
      *
       RT-IMPRIME-REL-CABX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Grava linhas detalhe relatório
      *----------------------------------------------------------------*
       RT-IMPRIME-REL-DET              SECTION.
      *
           MOVE SRTV-CODIGO            TO RLD-VCODIGO.
      *
           MOVE SRTV-CPF               TO WSS-CPF-AUX.
           PERFORM RT-CPF              THRU RT-CPFX.
      *
           MOVE WSS-CPF-EDITADO        TO RLD-CPF.
           MOVE SRTV-NOME              TO RLD-NOME.
           MOVE SRTV-LATITUDE          TO RLD-LATITUDE.
           MOVE SRTV-LONGITUDE         TO RLD-LONGITUDE.
      *
           WRITE REG-REL-VENDEDOR      FROM DETALHE AFTER 1.

           IF FS-VEN-REL               EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 13                  TO LKS-RETORNO
              MOVE 'ERRO AO GRAVAR DETALHE NO REL DE VENDEDOR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZA      THRU RT-FINALIZAX
           END-IF.
      *
       RT-IMPRIME-REL-DETX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Grava linhas rodape relatório
      *----------------------------------------------------------------*
       RT-IMPRIME-REL-RDP              SECTION.
      *
           WRITE REG-REL-VENDEDOR      FROM LINHA-TRACO AFTER 1.
      *
           MOVE FUNCTION CURRENT-DATE(7:2)
                                       TO  RLR-REL-DAT-DIA.
           MOVE FUNCTION CURRENT-DATE(5:2)
                                       TO  RLR-REL-DAT-MES.
           MOVE FUNCTION CURRENT-DATE(1:4)
                                       TO  RLR-REL-DAT-ANO.
      *
           MOVE WSS-REL-NPAG           TO RLR-PAGINA.
      *
           WRITE REG-REL-VENDEDOR      FROM RODAPE AFTER 1.
      *
           IF FS-VEN-REL               EQUAL ZEROS
              MOVE ZEROS               TO WSS-REL-NLIN
           ELSE
              MOVE 13                  TO LKS-RETORNO
              MOVE 'ERRO AO GRAVAR DETALHE NO REL DE VENDEDOR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZA      THRU RT-FINALIZAX
           END-IF.
      *
       RT-IMPRIME-REL-RDPX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                   F I M  D O  P R O G R A M A
      *----------------------------------------------------------------*
