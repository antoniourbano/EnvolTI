      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     PT000903.
       AUTHOR.                         AmarildoMB.
       DATE-WRITTEN.                   09 MAR 2020.
       DATE-COMPILED.
      *----------------------------------------------------------------*
      *REMARKS.
      *     *----------------------------------------------------------*
      *     *#NOME     : PT000903 ---> RELATORIO -> DISTRIBUIR.CSV     *
      *     *----------------------------------------------------------*
      *     *#TIPO     : COBOL - MICRO FOCUS - NET EXPRESS 5           *
      *     *----------------------------------------------------------*
      *     *#ANALISTA : AMARILDO MBORGES                              *
      *     *----------------------------------------------------------*
      *     *#FUNÇÃO   : IMPRIMIR RELATARIO COM DADOS DA DISTRIBUICAO  *
      *     *            DE CLIENTES POR VENDEDOR FORMATANDO A  SAIDA  *
      *     *            PARA O ARQUIVO DISTRIBUIR.CSV.                *
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
      *  ---> Arquivo DISTRIBUICAO
           SELECT ARQ-DISTRIBUIR       ASSIGN TO DISK
                  ORGANIZATION         IS LINE SEQUENTIAL
                  FILE STATUS          IS FS-DISTRIBUIR.
      *
           SELECT REL-DISTRIBUIR       ASSIGN TO 'DISTRIBUIR.CSV'
                  ORGANIZATION         IS LINE SEQUENTIAL
                  FILE STATUS          IS FS-RELATO.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-DISTRIBUIR              VALUE OF FILE-ID IS
                                                       'DISTRIBUICAO'
           LABEL RECORDS               IS STANDARD
           DATA RECORD                 IS REG-FDD.
       COPY 'FD-DISTRIBUIR.CPY'        REPLACING ==::== BY == -FDD ==.
      *
       FD  REL-DISTRIBUIR.
       01  REG-RELATO                  PIC X(120).
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(032)         VALUE
           'III  WORKING STORAGE SECTION III'.
      *
       77  WSS-FIM-ARQ                 PIC  X(001)         VALUE 'N'.
      *
       01  WSS-AUXILIARES.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - FILE STATUS
      *----------------------------------------------------------------*
           03  FS-CLIENTES             PIC  X(002)         VALUE SPACES.
           03  FS-VENDEDOR             PIC  X(002)         VALUE SPACES.
           03  FS-DISTRIBUIR           PIC  X(002)         VALUE SPACES.
           03  FS-RELATO               PIC  X(002)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *    AREA DE ARQUIVOS NA WORKING
      *----------------------------------------------------------------*
      *  ---> DISTRIBUIR
       COPY 'FD-DISTRIBUIR.CPY'        REPLACING ==::== BY == -WSD ==.
      *
      *----------------------------------------------------------------*
      *    AREA LAY-OUT PARA RELATORIO CSV
      *----------------------------------------------------------------*
      *
      *  ---> CABECALHO
       01  CAB-CSV.
           03  FILLER                  PIC  X(012)         VALUE
                                                        'COD CLIENTE;'.
           03  FILLER                  PIC  X(012)         VALUE
                                                        'RAZAO SOCIAL'.
           03  FILLER                  PIC  X(028)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE ';'.
           03  FILLER                  PIC  X(013)         VALUE
                                                        'COD VENDEDOR;'.
           03  FILLER                  PIC  X(013)         VALUE
                                                        'NOME VENDEDOR'.
           03  FILLER                  PIC  X(027)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE ';'.
           03  FILLER                  PIC  X(003)         VALUE SPACES.
           03  FILLER                  PIC  X(014)         VALUE
                                                        'DISTANCIA;'.
      *
      *  ---> DETALHE
       01  DET-CSV.
           03  FILLER                  PIC  X(004)         VALUE SPACES.
           03  CODIGO-CLIENTE-CSV      PIC  9(007)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ';'.
           03  RSOCIAL-CSV             PIC  X(040)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE ';'.
           03  FILLER                  PIC  X(005)         VALUE SPACES.
           03  CODIGO-VENDEDOR-CSV     PIC  9(007)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ';'.
           03  NOME-CSV                PIC  X(040)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE ';'.
           03  DISTANCIA-CSV           PIC  ZZZZZZZZ9,99   VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE ';'.
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  LKS-PARM.
           03 FILLER                   PIC S9(004) COMP.
           03 LKS-RETORNO              PIC  X(002).
           03 LKS-MENSAGEM             PIC  X(050).
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
           PERFORM RT-PROCESSAR        THRU RT-PROCESSAR.
      *
           PERFORM RT-FINALIZAR        THRU RT-FINALIZAR.
      *
       RT-PRINCIPALX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-INICIALIZAR                  SECTION.
      *  ---> Inicializa areas e variaveis
      *----------------------------------------------------------------*
      *
           INITIALIZE WSS-AUXILIARES
                      REG-WSD
              REPLACING ALPHANUMERIC   BY SPACES
                        NUMERIC        BY ZEROS.
      *
           MOVE '00'                   TO LKS-RETORNO
      *
           PERFORM RT-ABRIR-ARQUIVOS   THRU RT-ABRIR-ARQUIVOSX.
      *
           PERFORM RT-GRAVAR-CABECALHO THRU RT-GRAVAR-CABECALHOX.
      *
       RT-INICIALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-ABRIR-ARQUIVOS               SECTION.
      *  ---> Abrir arquivos
      *----------------------------------------------------------------*
      *
      *  ---> arquivo DISTRIBUIR
           OPEN INPUT ARQ-DISTRIBUIR.

           IF FS-DISTRIBUIR            EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao abrir arquivo DISTRIBUIR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZAR
           END-IF.
      *
      *  ---> arquivo DISTRIBUIR - Relatorio - CSV
           OPEN OUTPUT REL-DISTRIBUIR.
      *
           IF FS-RELATO                EQUAL '00'
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao abrir arquivo DISTRIBUIR.CSV'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZAR
           END-IF.
      *
       RT-ABRIR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-GRAVAR-CABECALHO             SECTION.
      *  ---> Grava cabecalho do relatorio
      *----------------------------------------------------------------*
      *
           WRITE REG-RELATO            FROM CAB-CSV AFTER 1 LINE.
      *
       RT-GRAVAR-CABECALHOX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-PROCESSAR                    SECTION.
      *  ---> Processamento
      *----------------------------------------------------------------*
      *
           PERFORM RT-LER-DISTRIBUIR   THRU RT-LER-DISTRIBUIRX.
      *
           PERFORM
              UNTIL WSS-FIM-ARQ        EQUAL 'S' OR 's'
              PERFORM RT-TRATAR-ARQUIVO
                                       THRU RT-TRATAR-ARQUIVOX
           END-PERFORM.
      *
           PERFORM RT-FECHAR-ARQUIVOS  THRU RT-FECHAR-ARQUIVOSX.
      *
       RT-PROCESSARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-LER-DISTRIBUIR               SECTION.
      *  ---> Leitura arquivo DISTRIBUIR
      *----------------------------------------------------------------*
      *
           READ ARQ-DISTRIBUIR
                NEXT
                INTO REG-WSD
             AT END
                MOVE 'S'               TO WSS-FIM-ARQ
                PERFORM RT-PROCESSAR
           END-READ.
      *
           IF FS-DISTRIBUIR            EQUAL ZEROS OR '10'
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao ler arquivo DISTRIBUIR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZAR
           END-IF.
      *
       RT-LER-DISTRIBUIRX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-TRATAR-ARQUIVO               SECTION.
      *  ---> Grava linhas detalhe no relatorio
      *  ---> Pega proximo registro DISTRIBUIR
      *----------------------------------------------------------------*
      *
           PERFORM RT-GRAVAR-DETALHE   THRU RT-GRAVAR-DETALHEX.
           PERFORM RT-LER-DISTRIBUIR   THRU RT-LER-DISTRIBUIRX.
      *
       RT-TRATAR-ARQUIVOX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-GRAVAR-DETALHE               SECTION.
      *  ---> Movimenta e grava campos para linha detalhe
      *----------------------------------------------------------------*
      *
      *  ---> Carrega linha detalhe
           MOVE CCLIENTE-WSD           TO CODIGO-CLIENTE-CSV.
           MOVE RSOCIAL-WSD            TO RSOCIAL-CSV.
           MOVE CVENDEDOR-WSD          TO CODIGO-VENDEDOR-CSV.
           MOVE NOME-WSD               TO NOME-CSV.
           MOVE DISTANCIA-WSD          TO DISTANCIA-CSV.
      *
      *  ---> Grava linha detalhe
           WRITE REG-RELATO            FROM DET-CSV AFTER 1 LINE.
      *
       RT-GRAVAR-DETALHEX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-FECHAR-ARQUIVOS              SECTION.
      *  ---> Fechar arquivos
      *----------------------------------------------------------------*
      *
           CLOSE ARQ-DISTRIBUIR.
      *
           IF FS-DISTRIBUIR            EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao fechar arquivo DISTRIBUIR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZAR
           END-IF.
      *
           CLOSE REL-DISTRIBUIR.
      *
           IF FS-RELATO                EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE '09'                TO LKS-RETORNO
              MOVE 'Erro ao fechar arquivo RELATORIO DISTRIBUIR'
                                       TO LKS-MENSAGEM
              PERFORM RT-FINALIZAR     THRU RT-FINALIZAR
           END-IF.
      *
       RT-FECHAR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-FINALIZAR                    SECTION.
      *  ---> Finaliza programa
      *----------------------------------------------------------------*
      *
           GOBACK.
      *
       RT-FINALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                   F I M  D O  P R O G R A M A
      *----------------------------------------------------------------*
