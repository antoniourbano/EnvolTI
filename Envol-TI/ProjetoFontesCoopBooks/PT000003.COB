      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     PT000003.
       AUTHOR.                         AmarildoMB.
       DATE-WRITTEN.                   09 MAR 2020.
       DATE-COMPILED.
      *----------------------------------------------------------------*
      *REMARKS.
      *     *----------------------------------------------------------*
      *     *#NOME     : PT000003 ---> DISTRIBUIR - CLIENTES/VENDEDOR  *
      *     *----------------------------------------------------------*
      *     *#TIPO     : COBOL - MICRO FOCUS - NET EXPRESS 5           *
      *     *----------------------------------------------------------*
      *     *#ANALISTA : AMARILDO MBORGES                              *
      *     *----------------------------------------------------------*
      *     *#FUNÇÃO   : ESTE PROGRAMA TEM COMO FUNCAO O CALCULO  E    *
      *     *            DISTRIBUICAO DE CLIENTE PARA VENDEDOR, CON-   *
      *     *            SIDERANDO A MENOR DISTANCIA ENTRE LATITUDE    *
      *     *            E LONGITUDE DE AMBOS.                         *   
      *     *----------------------------------------------------------*
      *     * VERSÃO 01    -    Amarildo M Borges      -    09.03.2020 *
      *     *----------------------------------------------------------*
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
      *  ---> Arquivo VENDEDORES
           COPY 'SEL-VENDEDOR.CPY'     REPLACING ==::== BY == -FDV ==.
      *
      *  ---> Arquivo DISTRIBUICAO
           SELECT ARQ-DISTRIBUIR       ASSIGN TO DISK                              
                  ORGANIZATION         IS LINE SEQUENTIAL
                  FILE STATUS          IS FS-DISTRIBUIR.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-CLIENTES                VALUE OF FILE-ID IS 'CLIENTES'
           LABEL RECORDS               IS STANDARD
           DATA RECORD                 IS REG-FDC.
       COPY 'FD-CLIENTES.CPY'          REPLACING ==::== BY == -FDC ==.
      *
       FD  ARQ-VENDEDOR                VALUE OF FILE-ID IS 'VENDEDOR'
           LABEL RECORDS               IS STANDARD
           DATA RECORD                 IS REG-FDV.
       COPY 'FD-VENDEDOR.CPY'          REPLACING ==::== BY == -FDV ==.
      *
       FD  ARQ-DISTRIBUIR              VALUE OF FILE-ID IS
                                                        'DISTRIBUICAO'
           LABEL RECORDS               IS STANDARD
           DATA RECORD                 IS REG-FDD.
       COPY 'FD-DISTRIBUIR.CPY'        REPLACING ==::== BY == -FDD ==.  .
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(032)         VALUE
           'III  WORKING STORAGE SECTION III'.
      *
       77  WSS-SON                     PIC  X(001)         VALUE 'N'.
           88 SON                      VALUE 'S' 's' 'N' 'n'.
      *
       77  WSS-PVENDEDOR               PIC  X(001)         VALUE 'N'.
           88 PVENDEDOR                VALUE 'S' 's' 'N' 'n'.
      *
       77  WSS-ERRO                    PIC  X(001)         VALUE 'N'.
       77  WSS-FIM-ARQ-CLI             PIC  X(001)         VALUE 'N'.
       77  WSS-FIM-ARQ-VEN             PIC  X(001)         VALUE 'N'.
       77  WSS-FIM-ARQ-DIS             PIC  X(001)         VALUE 'N'.
       77  WSS-LER-KEY-P               PIC  X(001)         VALUE 'N'.
      *----------------------------------------------------------------*
      *    VARIAVEIS - AUXILIARES
      *----------------------------------------------------------------*
       01  WSS-AUXILIARES.
           03  WSS-ENT                 PIC  X(001)         VALUE SPACES.
           03  WSS-FUN                 PIC  9(001)         VALUE ZEROS.
           03  WSS-TITULO              PIC  X(012)         VALUE SPACES.
           03  WSS-TIT-MAN             PIC  X(026)         VALUE SPACES.
           03  WSS-MENSAGEM            PIC  X(067)         VALUE SPACES.
           03  WSS-ACHOU               PIC  9(001)         VALUE ZEROS.
           03  WSS-PONTO-ERRO          PIC  9(002)         VALUE ZEROS.
           03  WSS-DESCRICAO           PIC  X(012)         VALUE SPACES.
           03  WSS-LINHA               PIC  9(003) COMP-3  VALUE ZEROS.
           03  WSS-LIMPA-LINHA         PIC  X(080)         VALUE SPACES.
           03  WSS-PROXIMO             PIC  9(007)         VALUE ZEROS.
           03  WSS-ARQUIVO             PIC  X(008)         VALUE SPACES.
           03  WSS-FSTATUS             PIC  X(002)         VALUE SPACES.
           03  WSS-PROGRAM             PIC  X(001)         VALUE SPACES.
           03  WSS-PARAM               PIC  X(001)         VALUE SPACES.
           03  WSS-START               PIC  9(003)         VALUE ZEROS.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - FILE STATUS
      *----------------------------------------------------------------*
           03  FS-CLIENTES             PIC  X(002)         VALUE SPACES.
           03  FS-VENDEDOR             PIC  X(002)         VALUE SPACES.
           03  FS-DISTRIBUIR           PIC  X(002)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - CALCULO DA DISTANCIA
      *----------------------------------------------------------------*
       01  WSS-CALCULO-DIST.
           03  WS-MENOR-DISTANCIA      PIC  9(009)V9(002)  VALUE
                                                             999999999.
           03  WS-CALC-DIST            PIC  9(009)V9(002)  VALUE ZEROS.
           03  WS-LAT-CLI              PIC S9(003)V9(008)  VALUE ZEROS.
           03  WS-LON-CLI              PIC S9(003)V9(008)  VALUE ZEROS.
           03  WS-LAT-VEN              PIC S9(003)V9(008)  VALUE ZEROS.
           03  WS-LON-VEN              PIC S9(003)V9(008)  VALUE ZEROS.
           03  WS-DLA                  PIC S9(003)V9(008)  VALUE ZEROS.
           03  WS-DLO                  PIC S9(003)V9(008)  VALUE ZEROS.
           03  WS-A                    PIC S9(003)V9(008)  VALUE ZEROS.
           03  WS-B                    PIC S9(003)V9(008)  VALUE ZEROS.

      *----------------------------------------------------------------*
      *    AREA DE ARQUIVOS NA WORKING
      *----------------------------------------------------------------*
      *
      *  ---> CLIENTES
       COPY 'FD-CLIENTES.CPY'          REPLACING ==::== BY == -WSC ==.
      *
      *  ---> VENDEDOR
       COPY 'FD-VENDEDOR.CPY'          REPLACING ==::== BY == -WSV ==.
      *
      *  ---> DISTRIBUIR
       COPY 'FD-DISTRIBUIR.CPY'        REPLACING ==::== BY == -WSD ==.
      *
      *----------------------------------------------------------------*
      *    AREAS DE COMUNICAÇÃO COM OUTROS PROGRAMAS
      *----------------------------------------------------------------*
      *
      *  ---> Formata e cria arquivo DISTRIBUIR.CSV
       01  WSPT0903                    PIC X(08)           VALUE
                                                            'PT000903'.
      *
      *----------------------------------------------------------------*
      *    Parametros para programas chamados
      *----------------------------------------------------------------*
      *
           COPY 'SUB-PRGS.CPY'         REPLACING ==::== BY == SBP ==.
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

           COPY SUB-PRGS               REPLACING ==::== BY == LKS ==.
      *
      *----------------------------------------------------------------*
      *    AREA DE TELAS
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *
      *  ---> Tela DISTRIBUIR
       01  TELA-DISTRIBUIR.
           03 F   LINE 05 COL 22       VALUE
                       ' Clientes x Vendedor - DISTRIBUICAO ' BLINK.
           03 F   LINE 10 COL 25       VALUE
                       ' Processando DISTRIBUIR...'.
           03 F   LINE 13 COL 21       VALUE 'Cliente: ' .
           03 F   LINE 13 COL 41       VALUE 'Vendedor: '.
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
       PROCEDURE                       DIVISION USING LKS-PARM.
      *================================================================*
       RT-PRINCIPAL                    SECTION.
      *  ---> Processamento principal
      *----------------------------------------------------------------*
      *
           PERFORM RT-INICIALIZAR      THRU RT-INICIALIZARX.
      *
           PERFORM
              UNTIL WSS-FIM-ARQ-CLI    EQUAL 'S' OR 's'
              PERFORM RT-PROCESSAR     THRU RT-PROCESSARX
           END-PERFORM.
      *
           PERFORM RT-FINALIZAR        THRU RT-FINALIZARX.
      *
       RT-PRINCIPALX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-INICIALIZAR                  SECTION.
      *  ---> Inicializa areas, variaveis e abre arquivos
      *----------------------------------------------------------------*
      *
           INITIALIZE WSS-AUXILIARES
                      REG-FDC
                      REG-FDV
                      REG-WSC
                      REG-WSV
                      SBP-PARM
                      LKS-PARM
              REPLACING ALPHANUMERIC   BY SPACES
                        NUMERIC        BY ZEROS.
      *
           MOVE '00'                   TO LKS-RETORNO.
      *
           PERFORM RT-ABRIR-ARQUIVOS   THRU RT-ABRIR-ARQUIVOSX.
      *
       RT-INICIALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-ABRIR-ARQUIVOS               SECTION.
      *  ---> Abrir arquivos
      *----------------------------------------------------------------*
      *
      *  ---> arquivo CLIENTES
           OPEN INPUT ARQ-CLIENTES.
      *
           IF FS-CLIENTES              EQUAL '00'
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao abrir arquivo CLIENTES'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
      *  ---> arquivo VENDEDOR
           OPEN INPUT ARQ-VENDEDOR.
      *
           IF FS-VENDEDOR              EQUAL '00'
              MOVE 'S'                 TO WSS-PVENDEDOR
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao abrir arquivo VENDEDOR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.

      *  ---> arquivo DISTRIBUICAO
           OPEN OUTPUT ARQ-DISTRIBUIR.
      *
           IF FS-DISTRIBUIR            EQUAL '00'
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao abrir arquivo DISTRIBUIR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-ABRIR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-PROCESSAR                    SECTION.
      *  ---> Processamento
      *----------------------------------------------------------------*
      *
           DISPLAY TELA-DISTRIBUIR.
      *
      *  ---> Primeira leitura
           PERFORM RT-LER-CLIENTES     THRU RT-LER-CLIENTESX.
      *
           PERFORM
              UNTIL WSS-FIM-ARQ-CLI    EQUAL 'S' OR 's'
              PERFORM RT-TRATAR-CLIENTES
                                       THRU RT-TRATAR-CLIENTESX
           END-PERFORM.
      *
           PERFORM RT-FECHAR-ARQUIVOS.
      *
           PERFORM RT-GERAR-RELATORIO.
      *
       RT-PROCESSARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-LER-CLIENTES                 SECTION.
      *  ---> Leitura arquivo CLIENTES
      *----------------------------------------------------------------*
      *
           READ ARQ-CLIENTES
                NEXT
                INTO REG-WSC
             AT END
                MOVE 'S'               TO WSS-FIM-ARQ-CLI
                GO                     TO RT-LER-CLIENTESX
           END-READ.
      *
           IF FS-CLIENTES              EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao ler arquivo CLIENTES'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-LER-CLIENTESX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-TRATAR-CLIENTES              SECTION.
      *  ---> Leitura arquivo CLIENTES
      *----------------------------------------------------------------*
      *
      *  ---> Exibe na tela, codigo cliente em processamento
           DISPLAY CODIGO-WSC          LINE 13 COL 30.
      *
      *  ---> Move dados do cliente para DISTRIBUIR
           MOVE CODIGO-WSC             TO CCLIENTE-WSD.
           MOVE RSOCIAL-WSC            TO RSOCIAL-WSD.
      *
      *  ---> Leituta para carregar primeiro vendedor
           PERFORM RT-LER-VENDEDOR.
      *
      *  ---> Trata vendedor
           PERFORM
              UNTIL WSS-FIM-ARQ-VEN    EQUAL 'S' OR 's'
              PERFORM RT-TRATAR-VENDEDOR
                                       THRU RT-TRATAR-VENDEDORX
           END-PERFORM.
      *
      *  ---> Move menor distancia e inicializa variavel
           MOVE  WS-MENOR-DISTANCIA    TO DISTANCIA-WSD
           MOVE  99999999999           TO WS-MENOR-DISTANCIA
      *
      *  ---> Grava registro na DISTRIBUIR
           PERFORM RT-GRAVAR-DISTRIBUICAO.
      *
      *  ---> Posiciona para ler proximo vendedor
           PERFORM RT-POSICIONA-VENDEDOR.
      *
      *  ---> Le proximo cliente
           PERFORM RT-LER-CLIENTES.

       RT-TRATAR-CLIENTESX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-POSICIONA-VENDEDOR           SECTION.
      *  ---> Posiciona no primeiro registo arquivo vendedor
      *----------------------------------------------------------------*
      *
      *  ---> Carrega valor para campo chave
           MOVE WSS-START              TO CODIGO-FDV.
      *
      *  ---> Inicializa area e variavel para proxima leitura
           MOVE 'N'                    TO WSS-FIM-ARQ-VEN.
           INITIALIZE                  REG-WSV.
      *
      *  ---> Posiciona no primeiro registo
           START ARQ-VENDEDOR
              KEY IS                   EQUAL CODIGO-FDV
              INVALID KEY
                 MOVE '09'             TO LKS-RETORNO
                 MOVE 'Erro ao posicionar arquivo VENDEDOR'
                                       TO LKS-MENSAGEM
                 PERFORM RT-FINALIZAR  THRU RT-FINALIZARX
           END-START.
      *
       RT-POSICIONA-VENDEDORX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-LER-VENDEDOR                 SECTION.
      *  ---> Leitura arquivo vendedor
      *----------------------------------------------------------------*

           READ ARQ-VENDEDOR
                NEXT
                INTO REG-WSV
             AT END
                MOVE 'S'               TO WSS-FIM-ARQ-VEN
                GO                     TO RT-LER-VENDEDORX
           END-READ.
      *
           IF FS-VENDEDOR              EQUAL ZEROS
      *  ---> Verifica primeira leitura arquivo vendedor
              IF WSS-PVENDEDOR         EQUAL 'S' OR 's'
      *  ---> Guarda primeiro registro para posicionamento futuro
                 MOVE CODIGO-WSV       TO WSS-START
      *  ---> Muda flag para dar continuidade a leitura
                 MOVE 'N'              TO WSS-PVENDEDOR
              END-IF
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao ler arquivo VENDEDOR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-LER-VENDEDORX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-TRATAR-VENDEDOR              SECTION.
      *  ---> Trata vendedor
      *  ---> Calcula menor distancia entre cliente e vendedor
      *  ---> Formula de Harbesine
      *----------------------------------------------------------------*
      *
      *  ---> Exibe na tela, codigo vendedor em processamento
           DISPLAY CODIGO-WSV LINE 13 COL 51.
      *
      *  ---> Calcula latitude Cliente e Vendedor
           COMPUTE WS-LAT-CLI  = LATITUDE-WSC  * FUNCTION PI / 180
      *
           COMPUTE WS-LAT-VEN  = LATITUDE-WSV  * FUNCTION PI / 180
      *
      *  ---> Calcula longitude Cliente e Vendedor
           COMPUTE WS-LON-CLI  = LONGITUDE-WSC * FUNCTION PI / 180
      *
           COMPUTE WS-LON-VEN  = LONGITUDE-WSV * FUNCTION PI / 180
      *
      *  ---> Calcula menor distancia Cliente e Vendedor
      *
      *    -> DLA -> Distancia Latitudinal
           COMPUTE WS-DLA = WS-LAT-VEN - (WS-LAT-CLI)
      *
      *    -> DLO -> Distancia Longitudinal
           COMPUTE WS-DLO = WS-LON-VEN - (WS-LON-CLI)
      *
      *  ---> Calcula Senos ao quadrado, cossenos e senos ao quadrado.
           COMPUTE WS-A = FUNCTION SIN(WS-DLA / 2)
                        * FUNCTION SIN(WS-DLA / 2)
                        + FUNCTION COS(WS-LAT-CLI)
                        * FUNCTION COS(WS-LAT-VEN)
                        * FUNCTION SIN(WS-DLO / 2)
                        * FUNCTION SIN(WS-DLO / 2)
      *
      *  ---> Raiz Quadrada
           COMPUTE WS-B = 2 * FUNCTION ATAN(FUNCTION SQRT(WS-A) /
                                            FUNCTION SQRT(1 - WS-A))
      *
      *  ---> Menor distancia em Metros
           COMPUTE WS-CALC-DIST = 6371 * WS-B * 1000
      *
      *  ---> Compara menor distancia
           IF WS-CALC-DIST             LESS WS-MENOR-DISTANCIA
              MOVE WS-CALC-DIST        TO WS-MENOR-DISTANCIA
              MOVE CODIGO-WSV          TO CVENDEDOR-WSD
              MOVE NOME-WSV            TO NOME-WSD
           END-IF.
      *
      *  ---> Leitura para carregar proximo vendedor
           PERFORM RT-LER-VENDEDOR.
      *
       RT-TRATAR-VENDEDORX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-GRAVAR-DISTRIBUICAO          SECTION.
      *  ---> Gravar registro arquivo DISTRIBUICAO
      *----------------------------------------------------------------*
      *
           WRITE REG-FDD               FROM REG-WSD.
      *
           IF FS-DISTRIBUIR            EQUAL '00' OR '22'
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao gravar arquivo DISTRIBUIR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-GRAVAR-DISTRIBUICAOX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-GERAR-RELATORIO              SECTION.
      *  ---> Chama subprograma para gerar relatorio DISTRIBUICAO.CSV
      *----------------------------------------------------------------*
      *
           CALL WSPT0903               USING SBP-PARAM.
      *
           MOVE LKS-RETORNO            TO SBP-RETORNO.
      *
           IF SBP-RETORNO              EQUAL '00'
              CONTINUE
           ELSE
              MOVE '09'                TO SBP-RETORNO
              MOVE "Erro de processamento programa PT000903."
                                       TO SBP-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-GERAR-RELATORIOX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-FINALIZAR                    SECTION.
      *  ---> Finaliza programa.
      *----------------------------------------------------------------*
      *
           GOBACK.
      *
       RT-FINALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-FECHAR-ARQUIVOS              SECTION.
      *  ---> Fecha arquivos
      *----------------------------------------------------------------*
      *
      *  ---> Fecha CLIENTES
           CLOSE ARQ-CLIENTES.
      *
           IF FS-CLIENTES              EQUAL '00'
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao fechar arquivo CLIENTES'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
      *  ---> Fecha VENDEDOR
           CLOSE ARQ-VENDEDOR.
      *
           IF FS-VENDEDOR              EQUAL '00'
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao fechar arquivo VENDEDOR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
      *  ---> Fecha DISTRIBUICAO
           CLOSE ARQ-DISTRIBUIR.
      *
           IF FS-DISTRIBUIR            EQUAL '00'
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao fechar arquivo DISTRIBUIR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZARX
           END-IF.
      *
       RT-FECHAR-ARQUIVOSX.
           EXIT.

      *----------------------------------------------------------------*
      *                   F I M  D O  P R O G R A M A
      *----------------------------------------------------------------*
