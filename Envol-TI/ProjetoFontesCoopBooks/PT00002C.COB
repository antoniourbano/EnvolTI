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
      *     *#FUNÇÃO   : FORMATAR RELATORIO DE CLIENTES E DE CLIENTES  *
      *     *----------------------------------------------------------*
      *     * VERSÃO 01    -    Amarildo M Borges      -    09.03.2020 *
      *     *----------------------------------------------------------*
      *                                                                *
      *     ARQUIVOS.....: ARQ-CLIENTES - ARQUIVO DE CLIENTES          *
      *                    SRT-CLIENTES - ARQUIVO DE SORT              *
      *                    REL-CLIENTES - ARQUIVO DE RELATORIO         *
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
      *  ---> Arquivo CLIENTES
           COPY 'SEL-CLIENTES.CPY'     REPLACING ==::== BY == -FDC ==.
      *
      *  ---> Arquivo SORT (Ordena dados)
           SELECT  SRT-CLIENTES        ASSIGN TO 'SRT-CLIENTES.TMP'
                   FILE STATUS         IS FS-CLI-SRT.
      *
      *  ---> Arquivo SORT (Ordena dados)
           SELECT  SRT-DISTRIBUICAO    ASSIGN TO 'SRT-DISTRIBUICAO.TMP'
                   FILE STATUS         IS FS-DST-SRT.
      *
      *  ---> Arquivo RELATORIO
           SELECT  REL-CLIENTES        ASSIGN TO 'REL-CLIENTES.TXT'
                   ORGANIZATION        IS LINE SEQUENTIAL
                   FILE STATUS         IS FS-CLI-REL.
      *
      *  ---> Arquivo DISTRIBUIÇÃO
           SELECT  ARQ-DISTRIBUIR      ASSIGN TO DISK
                   ORGANIZATION        IS LINE SEQUENTIAL
                   FILE STATUS         IS FS-DISTRIBUIR.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-CLIENTES                VALUE OF FILE-ID IS 'CLIENTES'
           LABEL RECORDS               IS STANDARD
           DATA RECORD                 IS CLI-FDC.
       COPY 'FD-CLIENTES.CPY'          REPLACING ==::== BY == -FDC ==.
      *
       SD  SRT-CLIENTES.
       01  REG-SRTC.
           03  SRTC-CODIGO             PIC  9(007).
           03  SRTC-CNPJ               PIC  9(014).
           03  SRTC-RSOCIAL            PIC  X(040).
           03  SRTC-LATITUDE           PIC S9(003)V9(008).
           03  SRTC-LONGITUDE          PIC S9(003)V9(008).
      *
       SD  SRT-DISTRIBUICAO.
       01  REG-SRTD.
           03  SRTD-CCLIENTE           PIC  9(007).
           03  SRTD-RSOCIAL            PIC  X(040).
           03  SRTD-CVENDEDOR          PIC  9(003).
           03  SRTD-NOME               PIC  X(040).
           03  SRTD-DISTANCIA          PIC  9(009).
      *
       FD  REL-CLIENTES.
       01  REG-REL-CLIENTES            PIC  X(120).
      *
       FD  ARQ-DISTRIBUIR              VALUE OF FILE-ID IS
                                                        'DISTRIBUICAO'
           LABEL RECORDS               IS STANDARD
           DATA RECORD                 IS DST-FDD.
       COPY 'FD-DISTRIBUIR.CPY'        REPLACING ==::== BY == -FDD ==.  .
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
           03  WSS-T-FILTRO            PIC  9              VALUE ZEROS.
           03  WSS-T-CCLIENTE          PIC  ZZZZZZ9        VALUE ZEROS.
           03  WSS-T-RSOCIAL           PIC  X(040)         VALUE SPACES.
           03  WSS-T-CVENDEDOR         PIC  9(003)         VALUE ZEROS.
      *
           03  WSS-FIM-ARQ             PIC  X(001)         VALUE SPACES.
           03  WSS-FIM-SORT            PIC  X(001)         VALUE SPACES.
           03  WSS-CNPJ-AUX            PIC  X(014)         VALUE SPACES.
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
           03  FS-CLIENTES             PIC  X(002)         VALUE SPACES.
           03  FS-CLI-SRT              PIC  X(002)         VALUE SPACES.
           03  FS-DST-SRT              PIC  X(002)         VALUE SPACES.
           03  FS-CLI-REL              PIC  X(002)         VALUE SPACES.
           03  FS-DISTRIBUIR           PIC  X(002)         VALUE SPACES.
      *
       01  WSS-CNPJ-EDITADO.
           03  WSS-CNPJ-EDT-1          PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE ".".
           03  WSS-CNPJ-EDT-2          PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE ".".
           03  WSS-CNPJ-EDT-3          PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE "/".
           03  WSS-CNPJ-EDT-4          PIC  X(004)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE "-".
           03  WSS-CNPJ-EDT-5          PIC  X(002)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *    AREA DE ARQUIVOS NA WORKING
      *----------------------------------------------------------------*
      *
      *  ---> CLIENTES
       COPY 'FD-CLIENTES.CPY'          REPLACING ==::== BY == -WSC ==.
      *
      *  ---> DISTRIBUICAO
       COPY 'FD-DISTRIBUIR.CPY'        REPLACING ==::== BY == -WSD ==.
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
                                           'COD.CLIENTE'.
               05 FILLER               PIC  X(002)         VALUE SPACES.
               05 FILLER               PIC  X(008)         VALUE
                                           'CPF'.
               05 FILLER               PIC  X(012)         VALUE SPACES.
               05 FILLER               PIC  X(012)         VALUE
                                           'RAZÃO SOCIAL'.
               05 FILLER               PIC  X(035)         VALUE SPACES.
               05 FILLER               PIC  X(008)         VALUE
                                           'LATITUDE'.
               05 FILLER               PIC  X(006)         VALUE SPACES.
               05 FILLER               PIC  X(009)         VALUE
                                           'LONGITUDE'.
               05 FILLER               PIC  X(002)         VALUE SPACES.
      *
       01  DETALHE-VENDEDOR.
           03  FILLER                  PIC  X(016)         VALUE SPACES.
           03  FILLER                  PIC  X(011)         VALUE
                                           'VENDEDOR: '.
           03  RLD-VCODIGO             PIC  9(007)         VALUE ZEROS.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  RLD-VNOME               PIC  X(040)         VALUE ZEROS.
      *
       01  DETALHE-CLIENTE.
           03  FILLER                  PIC  X(006)         VALUE SPACES.
           03  RLD-CODIGO              PIC  9(007)         VALUE ZEROS.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  RLD-CNPJ                PIC  X(018)         VALUE ZEROS.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  RLD-RSOCIAL             PIC  X(040)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  RLD-LATITUDE            PIC +ZZ9,99999999   VALUE ZEROS.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  RLD-LONGITUDE           PIC +ZZ9,99999999   VALUE ZEROS.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
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
      *    TELA IMPORT CLIENTES - CADASTRO
      *----------------------------------------------------------------*
       01  TELA-TITULO-CLIENTES.
           03 F LINE 05 COL 28         VALUE ' CLIENTES -'        BLINK.
           03 F LINE 05 COL 39         PIC X(12) USING WSS-TITULO BLINK.
      *
       01  TELA-CLIENTES-RELATORIO.
           03 TCR1 LINE 08 COL 10  VALUE 'Tipo de ordenacao: '.
           03 TCR2 LINE 09 COL 15  VALUE '<A>scendente <D>ecrescente'.
           03 TCR3 LINE 11 COL 10  VALUE 'Tipo de Classificacao: '.
           03 TCR4 LINE 12 COL 15  VALUE
                                   '<C>od cliente <N>ome cliente'.
           03 TCR5 LINE 14 COL 10  VALUE 'Filtra por: '.
           03 TCR6 LINE 16 COL 15  VALUE '1 - Codigo do cliente.: '.
           03 TCR7 LINE 18 COL 15  VALUE '2 - Nome do cliente...: '.
           03 TCR8 LINE 20 COL 15  VALUE '3 - Codigo do vendedor: '.
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
           PERFORM RT-INICIALIZAR      THRU RT-INICIALIZARX.
           PERFORM RT-PROCESSAR        THRU RT-PROCESSARX.
           PERFORM RT-FINALIZAR        THRU RT-FINALIZARX.
      *
       RT-PRINCIPALX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina de inicialização                                        *
      *----------------------------------------------------------------*
       RT-INICIALIZAR                  SECTION.
      *
           INITIALIZE                  WSS-AUXILIARES
                                       REG-WSC
                                       LKS-PARM
              REPLACING ALPHANUMERIC   BY SPACES
                        NUMERIC        BY ZEROS.
      *
           MOVE 'RELATORIO GERADO COM SUCESSO'
                                       TO LKS-MENSAGEM.
      *
           MOVE ZEROS                  TO LKS-RETORNO.
      *
       RT-INICIALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Abertura de arquivos
      *----------------------------------------------------------------*
       RT-ABRE-ARQUIVOS                SECTION.
      *
           IF WSS-T-FILTRO             EQUAL 3
              OPEN I-O ARQ-CLIENTES
           ELSE
              OPEN INPUT ARQ-CLIENTES
           END-IF.
      *
           IF FS-CLIENTES              EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO LKS-RETORNO
              MOVE 'ERRO NA ABERTURA DO ARQUIVO CLIENTES'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
           OPEN OUTPUT REL-CLIENTES.
      *
           IF FS-CLI-REL               EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO LKS-RETORNO
              MOVE 'ERRO NA ABERTURA DO ARQUIVO REL-CLIENTES'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
           OPEN INPUT ARQ-DISTRIBUIR.
      *
            IF FS-DISTRIBUIR           EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO LKS-RETORNO
              MOVE 'ERRO NA ABERTURA DO ARQUIVO DISTRIBUIR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-ABRE-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Processamento Principal                                        *
      *----------------------------------------------------------------*
       RT-PROCESSAR                    SECTION.
      *
           PERFORM RT-LIMPAR-TELA      THRU RT-LIMPAR-TELAX.
      *
           MOVE '  RELATORIOS'         TO WSS-TITULO
      *
           DISPLAY TELA-TITULO-CLIENTES.
      *
           MOVE 'Informe tipo de ordenação.'
                                       TO WSS-MENSAGEM.
      *
           DISPLAY TCR1
                   TCR2
                   TELA-MENSAGENS.
      *
           ACCEPT WSS-T-ORDENA         LINE 08 COL 29
                                            WITH PROMPT AUTO-SKIP.
      *
           MOVE 'Informe tipo de classificação.'
                                       TO WSS-MENSAGEM.
      *
           DISPLAY TCR3
                   TCR4
                   TELA-MENSAGENS.
      *
           ACCEPT WSS-T-CLASSIF        LINE 11 COL 33
                                            WITH PROMPT AUTO-SKIP.
      *
           MOVE 'Informe filtro para relatorio. <0> para sem filtros.'
                                       TO WSS-MENSAGEM.


           DISPLAY TCR5
                   TCR6
                   TCR7
                   TCR8
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
                    ACCEPT WSS-T-CCLIENTE
                                       LINE 16 COL 39
                                            WITH PROMPT AUTO-SKIP
               WHEN 2
                    ACCEPT WSS-T-RSOCIAL
                                       LINE 18 COL 39
                                            WITH PROMPT AUTO-SKIP
               WHEN 3
                    ACCEPT WSS-T-CVENDEDOR
                                       LINE 20 COL 39
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
           PERFORM RT-ABRE-ARQUIVOS    THRU RT-ABRE-ARQUIVOSX.
      *
           IF WSS-T-FILTRO             EQUAL 3
              PERFORM RT-SORT-DIST     THRU RT-SORT-DISTX
           ELSE
              PERFORM RT-SORT-CLIENTE  THRU RT-SORT-CLIENTEX
           END-IF.
      *
       RT-PROCESSARX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina de Sort do Vendedor
      *----------------------------------------------------------------*
       RT-SORT-DIST                    SECTION.
      *
           SORT SRT-DISTRIBUICAO
                ON ASCENDING           KEY SRTD-CVENDEDOR
                   INPUT PROCEDURE     IS RT-INPUT-SORT-DIST
                   OUTPUT PROCEDURE    IS RT-OUTPUT-SORT-DIST.
      *
       RT-SORT-DISTX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina de Sort do Cliente
      *----------------------------------------------------------------*
       RT-SORT-CLIENTE                 SECTION.
      *
           IF WSS-T-ORDENA             EQUAL 'A' OR 'a'
              IF WSS-T-CLASSIF         EQUAL 'C' OR 'c'
                 SORT SRT-CLIENTES
                      ON ASCENDING     KEY SRTC-CODIGO
                      INPUT PROCEDURE  IS RT-INPUT-SORT-CLIENTE
                      OUTPUT PROCEDURE IS RT-OUTPUT-SORT-CLIENTE
              ELSE
                 SORT SRT-CLIENTES
                      ON ASCENDING     KEY SRTC-RSOCIAL
                      INPUT PROCEDURE  IS RT-INPUT-SORT-CLIENTE
                      OUTPUT PROCEDURE IS RT-OUTPUT-SORT-CLIENTE
              END-IF
           ELSE
              IF WSS-T-CLASSIF         EQUAL 'C' OR 'c'
                 SORT SRT-CLIENTES
                      ON DESCENDING    KEY SRTC-CODIGO
                      INPUT PROCEDURE  IS RT-INPUT-SORT-CLIENTE
                      OUTPUT PROCEDURE IS RT-OUTPUT-SORT-CLIENTE
              ELSE
                 SORT SRT-CLIENTES
                      ON DESCENDING    KEY SRTC-RSOCIAL
                      INPUT PROCEDURE  IS RT-INPUT-SORT-CLIENTE
                      OUTPUT PROCEDURE IS RT-OUTPUT-SORT-CLIENTE
              END-IF
           END-IF.
      *
       RT-SORT-CLIENTEX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina de INPUT Sort DISTRIBUICAO
      *----------------------------------------------------------------*
       RT-INPUT-SORT-DIST              SECTION.
      *
           PERFORM RT-LER-DISTRIBUICAO THRU RT-LER-DISTRIBUICAOX
      *
           PERFORM UNTIL WSS-FIM-ARQ   EQUAL 'S' OR 's'
              PERFORM RT-ALIMENTA-SORT-DIST
                                       THRU RT-ALIMENTA-SORT-DISTX
           END-PERFORM.
      *
       RT-INPUT-SORT-DISTX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina de INPUT Sort CLIENTE
      *----------------------------------------------------------------*
       RT-INPUT-SORT-CLIENTE           SECTION.
      *
           PERFORM RT-LER-CLIENTES     THRU RT-LER-CLIENTESX.
      *
           PERFORM UNTIL WSS-FIM-ARQ   EQUAL 'S' OR 's'
              PERFORM RT-ALIMENTA-SORT-CLIENTE
                                       THRU RT-ALIMENTA-SORT-CLIENTEX
           END-PERFORM.
      *
       RT-INPUT-SORT-CLIENTEX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina de alimentação do Sort - Filtros selecionados na tela   *
      * ---> Clientes por vendedor
      *----------------------------------------------------------------*
       RT-ALIMENTA-SORT-DIST           SECTION.
      *
           IF CVENDEDOR-WSD            EQUAL WSS-T-CVENDEDOR
              RELEASE REG-SRTD         FROM REG-WSD
           END-IF.
      *
           PERFORM RT-LER-DISTRIBUICAO THRU RT-LER-DISTRIBUICAOX.
      *
       RT-ALIMENTA-SORT-DISTX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina de alimentação do Sort - Filtros selecionados na tela   *
      * ---> Clientes
      *----------------------------------------------------------------*
       RT-ALIMENTA-SORT-CLIENTE        SECTION.
      *
           IF WSS-T-CCLIENTE           EQUAL ZEROS
              IF WSS-T-RSOCIAL         EQUAL SPACES
                 IF WSS-T-CVENDEDOR    EQUAL ZEROS
                    RELEASE REG-SRTC   FROM REG-WSC
                 ELSE
                    IF CVENDEDOR-WSD   EQUAL WSS-T-CVENDEDOR
                       RELEASE REG-SRTC
                                       FROM REG-WSC
                    END-IF
                 END-IF
              ELSE
                 IF RSOCIAL-WSC        EQUAL WSS-T-RSOCIAL
                    RELEASE REG-SRTC   FROM REG-WSC
                 END-IF
              END-IF
           ELSE
              IF CODIGO-WSC            EQUAL WSS-T-CCLIENTE
                 RELEASE REG-SRTC      FROM REG-WSC
              END-IF
           END-IF.
      *
           PERFORM RT-LER-CLIENTES     THRU RT-LER-CLIENTESX.
      *
       RT-ALIMENTA-SORT-CLIENTEX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina OUTPUT do Sort DISTRIBUICAO
      *----------------------------------------------------------------*
       RT-OUTPUT-SORT-DIST             SECTION.
      *
           RETURN SRT-DISTRIBUICAO     INTO REG-SRTD
              AT END
                 MOVE 'S'              TO WSS-FIM-SORT
           END-RETURN.
      *
           PERFORM UNTIL WSS-FIM-SORT  EQUAL 'S' or 's'
              PERFORM RT-GERA-RELATORIO
                                       THRU RT-GERA-RELATORIOX
           END-PERFORM.
      *
       RT-OUTPUT-SORT-CLIENTEX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Rotina OUTPUT do Sort                                          *
      *----------------------------------------------------------------*
       RT-OUTPUT-SORT-CLIENTE          SECTION.
      *
           IF  WSS-T-FILTRO            EQUAL 3
               RETURN SRT-DISTRIBUICAO INTO REG-SRTD
                  AT END
                     MOVE 'S'          TO WSS-FIM-SORT
               END-RETURN
           END-IF.
      *
           RETURN SRT-CLIENTES         INTO REG-SRTC
              AT END
                 MOVE 'S'              TO WSS-FIM-SORT
           END-RETURN.
      *
           PERFORM UNTIL WSS-FIM-SORT  EQUAL 'S' or 's'
              PERFORM RT-GERA-RELATORIO
                                       THRU RT-GERA-RELATORIOX
           END-PERFORM.
      *
       RT-OUTPUT-SORT-CLIENTEX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Geração do Relatório de CLIENTES
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
                    IF WSS-T-FILTRO    EQUAL 3
                       RETURN SRT-DISTRIBUICAO
                                       INTO REG-SRTD
                          AT END
                             MOVE 'S'  TO WSS-FIM-SORT
                       END-RETURN
                    ELSE
                       RETURN SRT-CLIENTES
                                       INTO REG-SRTC
                          AT END
                             MOVE 'S'  TO WSS-FIM-SORT
                       END-RETURN
                    END-IF
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
       RT-FINALIZAR                    SECTION.
      *
           PERFORM VARYING WSS-IND     FROM WSS-REL-NLIN   BY 1
                UNTIL WSS-IND          GREATER  54
                WRITE REG-REL-CLIENTES FROM LINHA-BRANCO   AFTER 1
           END-PERFORM.
      *
           PERFORM RT-IMPRIME-REL-RDP  THRU RT-IMPRIME-REL-RDPX.
      *
           MOVE ZEROS                  TO WSS-REL-NLIN.
      *
           PERFORM RT-FECHAR-ARQUIVOS  THRU RT-FECHAR-ARQUIVOSX.
      *
           GOBACK.
      *
       RT-FINALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Leitura arquivo CLIENTES
      *----------------------------------------------------------------*
       RT-LER-CLIENTES                 SECTION.
      *
           READ ARQ-CLIENTES
                NEXT
                INTO REG-WSC
             AT END
                MOVE 'S'               TO WSS-FIM-ARQ
                PERFORM RT-INPUT-SORT-CLIENTE
           END-READ.
      *
           IF FS-CLIENTES              EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 10                  TO LKS-RETORNO
              MOVE 'ERRO NA LEITURA DO ARQUIVO CLIENTES'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-LER-CLIENTESX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Leitura arquivo DISTRIBUICAO
      *----------------------------------------------------------------*
       RT-LER-DISTRIBUICAO             SECTION.
      *
           READ ARQ-DISTRIBUIR         INTO REG-WSD
             AT END
                MOVE 'S'               TO WSS-FIM-ARQ
                PERFORM RT-INPUT-SORT-DIST
           END-READ.
      *
           IF FS-DISTRIBUIR            EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 10                  TO LKS-RETORNO
              MOVE 'ERRO NA LEITURA DO ARQUIVO DISTRIBUICAO'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-LER-DISTRIBUICAOX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Fechamento de arquivos
      *----------------------------------------------------------------*
       RT-FECHAR-ARQUIVOS              SECTION.
      *
           CLOSE ARQ-CLIENTES.
      *
           IF FS-CLIENTES              EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO LKS-RETORNO
              MOVE 'ERRO NA ABERTURA DO ARQUIVO CLIENTES'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
           CLOSE REL-CLIENTES.
      *
           IF FS-CLI-REL               EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 14                  TO LKS-RETORNO
              MOVE 'ERRO NO FECHAMENTO DO ARQUIVO DE RELATORIO'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-FECHAR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Formatação de CNPJ
      *----------------------------------------------------------------*
       RT-CNPJ                         SECTION.
      *
           MOVE WSS-CNPJ-AUX(01:2)     TO WSS-CNPJ-EDT-1.
           MOVE WSS-CNPJ-AUX(03:3)     TO WSS-CNPJ-EDT-2.
           MOVE WSS-CNPJ-AUX(06:3)     TO WSS-CNPJ-EDT-3.
           MOVE WSS-CNPJ-AUX(09:4)     TO WSS-CNPJ-EDT-4.
           MOVE WSS-CNPJ-AUX(13:2)     TO WSS-CNPJ-EDT-5.
      *
       RT-CNPJX.
           EXIT.
      *
      *----------------------------------------------------------------*
      * Grava cabecalho relatório
      *----------------------------------------------------------------*
       RT-IMPRIME-REL-CAB              SECTION.
      *
           MOVE 'C L I E N T E S'      TO RLC-TITULO.
      *
           IF WSS-REL-NLIN             EQUAL ZEROS
              WRITE REG-REL-CLIENTES   FROM LINHA-TRACO AFTER 1
           ELSE
              WRITE REG-REL-CLIENTES   AFTER ADVANCING PAGE
              WRITE REG-REL-CLIENTES   FROM LINHA-TRACO AFTER 1
           END-IF.
      *
           WRITE REG-REL-CLIENTES      FROM CAB-L1 AFTER 2.
           WRITE REG-REL-CLIENTES      FROM CAB-L2 AFTER 1.
           WRITE REG-REL-CLIENTES      FROM LINHA-BRANCO AFTER 1.
           WRITE REG-REL-CLIENTES      FROM CAB-L3 AFTER 1.
           WRITE REG-REL-CLIENTES      FROM LINHA-TRACO AFTER 1.
      *
           IF WSS-T-FILTRO             EQUAL 3
              MOVE SRTD-CVENDEDOR      TO RLD-VCODIGO
              MOVE SRTD-NOME           TO RLD-VNOME
              WRITE REG-REL-CLIENTES   FROM DETALHE-VENDEDOR AFTER 2
              WRITE REG-REL-CLIENTES   FROM LINHA-BRANCO AFTER 1
           END-IF.
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
           IF  WSS-T-FILTRO            EQUAL 3
               MOVE SRTD-CCLIENTE      TO CODIGO-FDC
               PERFORM RT-BUSCA-CLIENTES
                                       THRU RT-BUSCA-CLIENTES
               MOVE CODIGO-WSC         TO RLD-CODIGO
      *
               MOVE CNPJ-WSC           TO WSS-CNPJ-AUX
               PERFORM RT-CNPJ         THRU RT-CNPJX
      *
               MOVE WSS-CNPJ-EDITADO   TO RLD-CNPJ
               MOVE RSOCIAL-WSC        TO RLD-RSOCIAL
               MOVE LATITUDE-WSC       TO RLD-LATITUDE
               MOVE LONGITUDE-WSC      TO RLD-LONGITUDE
           ELSE
               MOVE SRTC-CODIGO        TO RLD-CODIGO
      *
               MOVE SRTC-CNPJ          TO WSS-CNPJ-AUX
               PERFORM RT-CNPJ         THRU RT-CNPJX
      *
               MOVE WSS-CNPJ-EDITADO   TO RLD-CNPJ
               MOVE SRTC-RSOCIAL       TO RLD-RSOCIAL
               MOVE SRTC-LATITUDE      TO RLD-LATITUDE
               MOVE SRTC-LONGITUDE     TO RLD-LONGITUDE
           END-IF.
      *
           WRITE REG-REL-CLIENTES      FROM DETALHE-CLIENTE AFTER 1.
      *
           IF FS-CLI-REL               EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 13                  TO LKS-RETORNO
              MOVE 'ERRO AO GRAVAR DETALHE NO REL DE CLIENTES'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
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
           WRITE REG-REL-CLIENTES      FROM LINHA-TRACO AFTER 1.
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
           WRITE REG-REL-CLIENTES      FROM RODAPE AFTER 1.
      *
           IF FS-CLI-REL               EQUAL ZEROS
              MOVE ZEROS               TO WSS-REL-NLIN
           ELSE
              MOVE 13                  TO LKS-RETORNO
              MOVE 'ERRO AO GRAVAR DETALHE NO REL DE CLIENTES'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-IMPRIME-REL-RDPX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    BUSCA DIRETA PELO CLIENTE DO VENDEDOR
      *----------------------------------------------------------------*
       RT-BUSCA-CLIENTES               SECTION.
      *----------------------------------------------------------------*
      *
           READ ARQ-CLIENTES           INTO REG-WSC
                                       KEY IS CODIGO-FDC
                INVALID KEY
                   MOVE 'Vendedor sem clientes'
                                       TO LKS-MENSAGEM
           END-READ.

           IF FS-CLIENTES              EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 13                  TO LKS-RETORNO
              MOVE 'REGISTRO NAO ENCONTRADO 2'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-BUSCA-CLIENTESX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                   F I M  D O  P R O G R A M A
      *----------------------------------------------------------------*
