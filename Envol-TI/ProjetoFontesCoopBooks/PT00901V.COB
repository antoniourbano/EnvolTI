      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     PRV0901.
       AUTHOR.                         AmarildoMB.
       DATE-WRITTEN.                   09 MAR 2020.
       DATE-COMPILED.
      *----------------------------------------------------------------*
      *REMARKS.
      *     *----------------------------------------------------------*
      *     *#NOME     : PRV0901 ---> IMPORTAÇÃO -> CLIENTES/VENDEDORES*
      *     *----------------------------------------------------------*
      *     *#TIPO     : COBOL - MICRO FOCUS - NET EXPRESS 5           *
      *     *----------------------------------------------------------*
      *     *#ANALISTA : AMARILDO MBORGES                              *
      *     *----------------------------------------------------------*
      *     *#FUNÇÃO   : ESTE PROGRAMA TEM COMO FUNÇÃO A IMPORTAÇÃO DE *
      *     *            DADOS PARA CLIENTES E VENDEDORES.             *
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
      *  ---> Registros arquivo VENDEDOR
           COPY 'SEL-VENDEDOR.CPY'     REPLACING ==::== BY == -FDV ==.
      *
      *  ---> Registros a serem importados
           SELECT  ARQ-IMPORT          ASSIGN TO WSS-ENDERECO-ARQ
                   ORGANIZATION        IS LINE SEQUENTIAL
                   ACCESS MODE         IS SEQUENTIAL
                   LOCK MODE           IS MANUAL
                   FILE STATUS         IS FS-IMPORT.
      *
      *  ---> Registros nao importados por inconsistencias
           SELECT  ARQ-RELATO          ASSIGN TO 'RELATO.TXT'
                   ORGANIZATION        IS LINE SEQUENTIAL
                   ACCESS MODE         IS SEQUENTIAL
                   LOCK MODE           IS MANUAL
                   FILE STATUS         IS FS-RELATO.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *
       FD  ARQ-VENDEDOR                VALUE OF FILE-ID IS 'VENDEDOR'
           LABEL RECORDS               IS STANDARD
           DATA RECORD                 IS REG-FDV.
       COPY 'FD-VENDEDOR.CPY'          REPLACING ==::== BY == -FDV ==.
      *
       FD  ARQ-IMPORT
           RECORDING MODE              IS F
           LABEL RECORD                IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  IMPORT-FD                   PIC  X(083).
      *
       FD  ARQ-RELATO
           RECORDING MODE              IS F
           LABEL RECORD                IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  REL-REG                     PIC  X(080).
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
       77  WSS-ERRO                    PIC  X(001)         VALUE 'N'.
           88 ERRO                     VALUE 'S' 's' 'N' 'n'.
      *
       77  WSS-FIM-IMPORT              PIC  X(001)         VALUE 'N'.
           88 FIMARQ                   VALUE 'S' 's' 'N' 'n'.
      *
       77  WSS-LER-KEY-COD             PIC  X(001)         VALUE 'N'.
           88 LERKEYP                  VALUE 'S' 's' 'N' 'n'.
      *
       77  WSS-GRAVA-CAB               PIC  X(001)         VALUE 'S'.
           88 IMPCAB                   VALUE 'S' 's' 'N' 'n'.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - AUXILIARES
      *----------------------------------------------------------------*
       01  WSS-AUXILIARES.
           03  WSS-ENT                 PIC  X(001)         VALUE SPACES.
           03  WSS-IND                 PIC  9(005)         VALUE ZEROS.
           03  WSS-TITULO              PIC  X(012)         VALUE SPACES.
           03  WSS-MENSAGEM            PIC  X(068)         VALUE SPACES.
           03  WSS-ACHOU               PIC  9(001)         VALUE ZEROS.
           03  WSS-PONTO-ERRO          PIC  9(002)         VALUE ZEROS.
           03  WSS-DESCRICAO           PIC  X(012)         VALUE SPACES.
           03  WSS-LINHA               PIC  9(003) COMP-3  VALUE ZEROS.
           03  WSS-LIMPA-LINHA         PIC  X(080)         VALUE SPACES.
           03  WSS-ARQUIVO             PIC  X(008)         VALUE SPACES.
           03  WSS-FSTATUS             PIC  X(002)         VALUE SPACES.
           03  WSS-STRING              PIC  X(060)         VALUE SPACES.
           03  WSS-PARAM               PIC  X(001)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - CONTADORES
      *----------------------------------------------------------------*
           03  WSS-REG-LIDOS           PIC  9(005)         VALUE ZEROS.
           03  WSS-REG-GRAVADOS        PIC  9(005)         VALUE ZEROS.
           03  WSS-REG-DESPREZADOS     PIC  9(005)         VALUE ZEROS.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - FILE STATUS
      *----------------------------------------------------------------*
           03  FS-VENDEDOR             PIC  X(002)         VALUE SPACES.
           03  FS-IMPORT               PIC  X(002)         VALUE SPACES.
           03  FS-RELATO               PIC  X(002)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - AUXILIARES PARA REGISTROS VENDEDOR / VENDEDORES
      *----------------------------------------------------------------
       01  WSS-CLIVEN.
           03  WSS-CODIGO              PIC  9(007).
           03  WSS-CPF                PIC  9(014).
           03  WSS-CPF                 PIC  9(011).
           03  WSS-NOME             PIC  X(040).
           03  WSS-LATITUDE            PIC S9(003)V9(008).
           03  WSS-LONGITUDE           PIC S9(003)V9(008).
      *
      *----------------------------------------------------------------*
      *    AREA DO ARQUIVO VENDEDOR NA WORKING
      *----------------------------------------------------------------*
       COPY 'FD-VENDEDOR.CPY'          REPLACING ==::== BY == -WSV ==.
      *
      *----------------------------------------------------------------*
      *    AREA DO ARQUIVO RELATO NA WORKING
      *----------------------------------------------------------------*
       01  WSS-REL-REG.
           03 FILLER                   PIC  X(003).
           03 WSS-REL-REGISTRO         PIC  X(005).
           03 FILLER                   PIC  X(001).
           03 WSS-REL-CAMPO            PIC  X(013).
           03 FILLER                   PIC  X(001).
           03 WSS-REL-DESCRICAO        PIC  X(050).

      *----------------------------------------------------------------*
      *    AREA PARA CHAMADA A SUBPROGRAMAS
      *----------------------------------------------------------------*       
      *    Area para chamada ao subprograma valida CPF / CNPJ          *
      *----------------------------------------------------------------*
      *
       01  WPT00900                    PIC X(08)           VALUE
                                                             'PT000900'.
      *
           COPY 'CPF-CNPJ.CPY'         REPLACING ==::== BY == 0900 ==.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)        VALUE
           'FFF  FIM DA WORKING-STORAGE  FFF'.
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  LKS-PARM.
           03 FILLER                   PIC S9(004)             COMP.
           03 LKS-PARAM                PIC  X(001).
           03 LKS-RETORNO              PIC  X(002).
           03 LKS-MENSAGEM             PIC  X(050).
      *
      *----------------------------------------------------------------*
      *    AREA DE TELAS
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *    TELA IMPORT VENDEDOR - CADASTRO
      *----------------------------------------------------------------*
       01  TELA-IMPORT-CLIENTES.
           03 F LINE 05 COL 28         VALUE ' VENDEDOR -'        BLINK.
           03 F LINE 05 COL 39         PIC X(12) USING WSS-TITULO BLINK.
      *
       01  TELA-IMPORT-VENDEDOR.
           03 F LINE 05 COL 28         VALUE ' Vendedores -'      BLINK.
           03 F LINE 05 COL 41         PIC X(12) USING WSS-TITULO BLINK.
      *
       01  TELA-IMPORT-ENDERECO.
           03 F LINE 08 COL 17         VALUE
                ' Informe nome e endereco do arquivo a ser importado. '.
           03 F LINE 13 COL 05         VALUE ' Endereco: '.
      *
      *----------------------------------------------------------------*
      *    TELA MENSAGEMS
      *----------------------------------------------------------------* 
       01  TELA-MENSAGENS.
      *----> Linha Limpa Mensagem
           03 LL-M LINE 24 COL 12      BLANK LINE.
      *
      *----> Linha Exibe Label
           03 LE-L LINE 24 COL 01      VALUE " Mensagem:" BLINK.
      *
      *----> Linha Exibe Mensagem
           03 LE-M LINE 24 COL 12      PIC  X(068) USING WSS-MENSAGEM.
      *
      *================================================================*
       PROCEDURE                       DIVISION USING LKS-PARM.  
      *================================================================*
       RT-PRINCIPAL                    SECTION.
      *  ---> Processamento inicial.
      *----------------------------------------------------------------*
      *
           PERFORM RT-INICIALIZAR      THRU RT-INICIALIZARX.
      *
           PERFORM
              UNTIL WSS-FIM-IMPORT     EQUAL 'S' OR
                    WSS-ENDERECO-ARQ   EQUAL 'S'
              PERFORM RT-PROCESSAR     THRU RT-PROCESSARX
           END-PERFORM.
      *
           PERFORM RT-FINALIZAR        THRU RT-FINALIZARX.
      *
           GOBACK.
      *
       RT-PRINCIPALX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-INICIALIZAR                  SECTION.
      *  ---> Inicializa variaveis diversas e abre arquivos.
      *----------------------------------------------------------------*
      *
           INITIALIZE WSS-AUXILIARES
                      REG-WSV
                      LKS-PARM
                      WSS-CLIVEN
                      WSS-ENDERECO-ARQ
              REPLACING ALPHANUMERIC   BY SPACES
                        NUMERIC        BY ZEROS.
      *
           MOVE LKS-PARAM              TO WSS-PARAM.
      *
           PERFORM RT-ABRIR-ARQUIVOS   THRU RT-ABRIR-ARQUIVOSX.
      *
       RT-INICIALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-PROCESSAR                    SECTION.
      *  ---> Processamento principal.
      *----------------------------------------------------------------*
      *
           PERFORM RT-LIMPAR-TELA      THRU RT-LIMPAR-TELAX.
      *
           MOVE '  IMPORTACAO'         TO WSS-TITULO.
      *
           DISPLAY TELA-IMPORT-VENDEDOR
                   TELA-IMPORT-ENDERECO.
      *
      *    DISPLAY TELA-IMPORT-ENDERECO.
      *
           MOVE 'Informe <S> para sair sem importar dados.'
                                       TO WSS-MENSAGEM.
           DISPLAY TELA-MENSAGENS.
      *
           ACCEPT WSS-ENDERECO-ARQ     LINE 13 COL 16
                                            WITH PROMPT AUTO-SKIP.
      *
      *  ---> Abre arquivo IMPORT ou Sai do programa.
           IF WSS-ENDERECO-ARQ         EQUAL 'S' OR 's'
              PERFORM RT-FINALIZAR
           ELSE
              PERFORM RT-ABRIR-IMPORT  THRU RT-ABRIR-IMPORTX
           END-IF.
      *
      *  ---> Executa ate achar o final do arquivo IMPORT.
           PERFORM
              UNTIL WSS-FIM-IMPORT     EQUAL'S' OR 's'
              MOVE 'N'                 TO WSS-ERRO
              PERFORM RT-LER-IMPORT    THRU RT-LER-IMPORTX
           END-PERFORM.
      *
       RT-PROCESSARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-ABRIR-ARQUIVOS               SECTION.
      * ---> Abre arquivos
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQ-VENDEDOR.
      *
           IF FS-VENDEDOR              EQUAL '00'
              CONTINUE
           ELSE
              MOVE 'abrir'             TO WSS-DESCRICAO
              MOVE 'VENDEDOR'          TO WSS-ARQUIVO
              MOVE FS-VENDEDOR         TO WSS-FSTATUS
              MOVE 1                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
           OPEN OUTPUT ARQ-RELATO.
      *
           IF FS-RELATO                EQUAL '00'
              CONTINUE
           ELSE
              MOVE 'abrir'             TO WSS-DESCRICAO
              MOVE 'RELATO'            TO WSS-ARQUIVO
              MOVE FS-RELATO           TO WSS-FSTATUS
              MOVE 2                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-ABRIR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-ABRIR-IMPORT                 SECTION.
      * ---> Abre arquivo com dados para importacao
      *----------------------------------------------------------------*
      *
           OPEN INPUT ARQ-IMPORT.
      *
           IF FS-IMPORT                EQUAL '00'
              CONTINUE
           ELSE
              MOVE 'abrir'             TO WSS-DESCRICAO
              MOVE 'IMPORT'            TO WSS-ARQUIVO
              MOVE FS-IMPORT           TO WSS-FSTATUS
              MOVE 3                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-ABRIR-IMPORTX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-LER-VENDEDOR                 SECTION.
      *  ---> Leitura do arquivo VENDEDOR
      *----------------------------------------------------------------*
      *
      *  ---> Busca registro por Codigo.
           IF WSS-LER-KEY-COD          EQUAL 'S' OR 's'
              READ ARQ-VENDEDOR        INTO REG-WSV
                                       KEY IS CODIGO-FDV
                   INVALID KEY
                       MOVE ZEROS      TO WSS-ACHOU
              END-READ
           ELSE
      *  ---> Busca registro por CPF.
              READ ARQ-VENDEDOR        INTO REG-WSV
                                       KEY IS CPF-FDV
                   INVALID KEY
                       MOVE ZEROS      TO WSS-ACHOU
              END-READ
           END-IF.
      *
      *  ---> Testa File Status da leitura.
           EVALUATE FS-VENDEDOR
               WHEN '00'
                    ADD 1              TO WSS-ACHOU
               WHEN '23'
                    CONTINUE
               WHEN OTHER
                    MOVE 'na leitura ' TO WSS-DESCRICAO
                    MOVE 'VENDEDOR'    TO WSS-ARQUIVO
                    MOVE FS-VENDEDOR   TO WSS-FSTATUS
                    MOVE 4             TO WSS-PONTO-ERRO
                    PERFORM RT-ERROS   THRU RT-ERROSX
           END-EVALUATE.
      *
       RT-LER-VENDEDORX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-LER-IMPORT               SECTION.
      *  ---> Leitura do arquivo com dados para importar
      *----------------------------------------------------------------*
      *
      *  ---> Leitura sequencial do arquivo.
           READ ARQ-IMPORT
                NEXT
                INTO REG-WSV
             AT END
                MOVE 'S'               TO WSS-FIM-IMPORT
                PERFORM RT-FINALIZAR
           END-READ
      *
      *  ---> Testa File Status da leitura.
           IF FS-IMPORT                EQUAL '00'
              ADD 1                    TO WSS-REG-LIDOS
              PERFORM RT-CONSISTE-CAMPOS
                                       THRU RT-CONSISTE-CAMPOS
           ELSE
              MOVE 'leitura'           TO WSS-DESCRICAO
              MOVE 'IMPORT'            TO WSS-ARQUIVO
              MOVE FS-IMPORT           TO WSS-FSTATUS
              MOVE 5                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-LER-IMPORTX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-CONSISTE-CAMPOS              SECTION.
      *  ---> Verificacao de campos e seu conteudo
      *----------------------------------------------------------------*
      *
      *  ---> Cabecalho para arquivo relatorio de inconsistencias
           IF WSS-GRAVA-CAB            EQUAL 'S' OR 's'
              MOVE 'REGISTRO CAMPO         DESCRICAO'
                                       TO REL-REG
              WRITE REL-REG
              MOVE 'N'                 TO WSS-GRAVA-CAB
           END-IF.
      *
      *  ---> Registro inconsistente
           MOVE WSS-REG-LIDOS          TO WSS-REL-REGISTRO.
      *
      *  ---> Verifica se codigo e valido - (Diferente de zeros)
           MOVE CODIGO-WSV             TO WSS-CODIGO.
      *
           IF WSS-CODIGO               EQUAL ZEROS
              ADD 1                    TO WSS-REG-DESPREZADOS
              MOVE 'CODIGO'            TO WSS-REL-CAMPO
              MOVE 'IGUAL A ZEROS'     TO WSS-REL-DESCRICAO
              PERFORM RT-GRAVAR-RELATO THRU RT-GRAVAR-RELATOX
              PERFORM RT-LER-IMPORT
           ELSE
      *  ---> Verifica se codigo ja esta cadastrado
              MOVE 'S'                 TO WSS-LER-KEY-COD
              MOVE WSS-CODIGO          TO CODIGO-FDV
              PERFORM RT-LER-VENDEDOR  THRU RT-LER-VENDEDORX
           END-IF.
      *
           IF WSS-ACHOU                EQUAL ZEROS
              CONTINUE
           ELSE
              ADD 1                    TO WSS-REG-DESPREZADOS
              MOVE 'CODIGO'            TO WSS-REL-CAMPO
              MOVE 'JA CADASTRADO'     TO WSS-REL-DESCRICAO
              PERFORM RT-GRAVAR-RELATO THRU RT-GRAVAR-RELATOX
              PERFORM RT-LER-IMPORT
           END-IF.
      *
      *  ---> Validar CNPJ / CPF - (Quanto ao formato)
           MOVE 'N'                    TO WSS-ERRO
           MOVE ZEROS                  TO RETORNO-0900
      *
           MOVE CPF-WSV               TO NUMERO-0900
           MOVE 'CNPJ'                 TO TIPO-CALCULO-0900
      *
           PERFORM RT-VALIDAR-CPF-CPF THRU RT-VALIDAR-CPF-CPFX
      *
           IF WSS-ERRO                 EQUAL 'N'
              CONTINUE
           ELSE
              ADD 1                    TO WSS-REG-DESPREZADOS
              MOVE 'CNPJ'              TO WSS-REL-CAMPO
              MOVE 'INVALIDO'          TO WSS-REL-DESCRICAO
              PERFORM RT-GRAVAR-RELATO THRU RT-GRAVAR-RELATOX
              PERFORM RT-LER-IMPORT
           END-IF.
      *
      *   ---> Verifica se CNPJ / CFP ja esta cadastrado
           MOVE 'N'                    TO WSS-LER-KEY-COD.
      *
           MOVE CPF-WSV               TO CPF-FDV.
           PERFORM RT-LER-VENDEDOR     THRU RT-LER-VENDEDORX.
      *
           IF WSS-ACHOU                EQUAL ZEROS
              CONTINUE
           ELSE
              ADD 1                    TO WSS-REG-DESPREZADOS
              MOVE 'CNPJ'              TO WSS-REL-CAMPO
              MOVE 'JA CADASTRADO'     TO WSS-REL-DESCRICAO
              PERFORM RT-GRAVAR-RELATO THRU RT-GRAVAR-RELATOX
              PERFORM RT-LER-IMPORT
           END-IF.
      *
      *  ---> Verifica se Razao Social e valida - Diferente de espacos
           IF NOME-WSV              EQUAL SPACES
              ADD 1                    TO WSS-REG-DESPREZADOS
              MOVE 'Nome'              TO WSS-REL-CAMPO
              MOVE 'IGUAL A ESPACOS'   TO WSS-REL-DESCRICAO
              PERFORM RT-GRAVAR-RELATO THRU RT-GRAVAR-RELATOX
              PERFORM RT-LER-IMPORT
           END-IF.
      *
      *  ---> Verifica se Latitude e valida - Diferente de Zeros
           IF LATITUDE-WSV              EQUAL ZEROS
              ADD 1                    TO WSS-REG-DESPREZADOS
              MOVE 'LATITUDE'          TO WSS-REL-CAMPO
              MOVE 'IGUAL A ZEROS'     TO WSS-REL-DESCRICAO
              PERFORM RT-GRAVAR-RELATO THRU RT-GRAVAR-RELATOX
              PERFORM RT-LER-IMPORT
           END-IF.
      *
      *  ---> Verifica se Longitude e valida - Diferente de Zeros
           IF LONGITUDE-WSV            EQUAL ZEROS
              ADD 1                    TO WSS-REG-DESPREZADOS
              MOVE 'LONGITUDE'         TO WSS-REL-CAMPO
              MOVE 'IGUAL A ZEROS'     TO WSS-REL-DESCRICAO
              PERFORM RT-GRAVAR-RELATO THRU RT-GRAVAR-RELATOX
              PERFORM RT-LER-IMPORT
           END-IF.
      *
           PERFORM RT-GRAVAR-REGISTROS  THRU RT-GRAVAR-REGISTROSX.
      *
       RT-CONSISTE-CAMPOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-VALIDAR-CPF-CPF             SECTION.
      *  ---> Verifica se CNPJ e valido.
      *----------------------------------------------------------------*
      *
           CALL WPT00900               USING REG-0900.
      *
           EVALUATE RETORNO-0900
               WHEN 0
                    CONTINUE
               WHEN 1
                    MOVE 'S'           TO WSS-ERRO
           END-EVALUATE.
      *
       RT-VALIDAR-CPF-CPFX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-GRAVAR-REGISTROS              SECTION.
      *  ---> Gravar registro no arquivo VENDEDOR / vendedores
      *----------------------------------------------------------------*
      *
           WRITE REG-FDV               FROM REG-WSV
      *
           IF FS-VENDEDOR              EQUAL '00'
              ADD 1                    TO WSS-REG-GRAVADOS
           ELSE
              MOVE 'gravar'            TO WSS-DESCRICAO
              MOVE 'VENDEDOR'          TO WSS-ARQUIVO
              MOVE FS-VENDEDOR         TO WSS-FSTATUS
              MOVE 6                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-GRAVAR-REGISTROSX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-GRAVAR-RELATO                SECTION.
      *  ---> Gravar registro no arquivo relato - inconsistencias
      *----------------------------------------------------------------*
      *
           WRITE REL-REG               FROM WSS-REL-REG.
      *
           IF FS-RELATO                EQUAL '00'
              CONTINUE
           ELSE
              MOVE 'Gravar'            TO WSS-DESCRICAO
              MOVE 'RELATO'            TO WSS-ARQUIVO
              MOVE FS-RELATO           TO WSS-FSTATUS
              MOVE 7                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-GRAVAR-RELATOX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-FECHAR-ARQUIVOS              SECTION.
      * ---> Fechar arquivos
      *----------------------------------------------------------------*
      *
           CLOSE ARQ-VENDEDOR
      *
           IF FS-VENDEDOR              EQUAL '00'
              CONTINUE
           ELSE
              MOVE 'fechar'            TO WSS-DESCRICAO
              MOVE 'VENDEDOR'          TO WSS-ARQUIVO
              MOVE FS-VENDEDOR         TO WSS-FSTATUS
              MOVE 8                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
           CLOSE ARQ-IMPORT.
      *
           IF WSS-ENDERECO-ARQ(1:1)    EQUAL 'S' OR 's'
              CONTINUE
           ELSE
               IF FS-IMPORT                EQUAL '00'
                   CONTINUE
               ELSE
                   MOVE 'fechar'            TO WSS-DESCRICAO
                   MOVE 'IMPORT'            TO WSS-ARQUIVO
                   MOVE FS-IMPORT           TO WSS-FSTATUS
                   MOVE 9                   TO WSS-PONTO-ERRO
                   PERFORM RT-ERROS         THRU RT-ERROSX
               END-IF
           END-IF.
      *
           CLOSE ARQ-RELATO.
      *
           IF FS-RELATO                EQUAL '00'
              CONTINUE
           ELSE
              MOVE 'fechar'            TO WSS-DESCRICAO
              MOVE 'IMPORT'            TO WSS-ARQUIVO
              MOVE FS-IMPORT           TO WSS-FSTATUS
              MOVE 10                  TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-FECHAR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-LIMPAR-TELA                  SECTION.
      *  ---> Limpa a tela desde a linha 4 ate a 22.
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
       RT-ERROS                        SECTION.
      *  ---> Monta mensagens de erro.
      *----------------------------------------------------------------*
      *
           PERFORM RT-LIMPAR-TELA      THRU RT-LIMPAR-TELAX.
      *
           STRING 'Erro ' WSS-DESCRICAO 'arquivo ' WSS-ARQUIVO
                  ' - FS: ' WSS-FSTATUS ' Local: #' WSS-PONTO-ERRO '#'
              DELIMITED BY SIZE        INTO WSS-MENSAGEM
           END-STRING.
      *
           DISPLAY TELA-MENSAGENS.
           ACCEPT WSS-ENT              LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP.
           MOVE 1                      TO LKS-RETORNO.
      *
           PERFORM RT-FINALIZAR        THRU RT-FINALIZARX.
      *
       RT-ERROSX.     
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-EXIBIR-TOTAL                 SECTION.
      *  ---> Exibe totalizadores apos final da importacao de dados.
      *----------------------------------------------------------------*
      *
           PERFORM RT-LIMPAR-TELA      THRU RT-LIMPAR-TELAX.
      *
           DISPLAY TELA-IMPORT-VENDEDOR.
      *
           DISPLAY '-----------------------------------------'
                                       LINE 07 COL 20.
           DISPLAY '             - DADOS FINAIS -            '
                                       LINE 09 COL 20.
           DISPLAY '-----------------------------------------'
                                       LINE 11 COL 20.
      *
           STRING ' REGISTROS LIDOS PARA IMPORTAR...: '
                    WSS-REG-LIDOS
              DELIMITED BY SIZE        INTO WSS-STRING
           END-STRING.
      *
           DISPLAY WSS-STRING          LINE 13 COL 20.
      *
           STRING ' REGISTROS GRAVADOS .............: '
                    WSS-REG-GRAVADOS
              DELIMITED BY SIZE   INTO WSS-STRING
           END-STRING.
      *
           DISPLAY WSS-STRING          LINE 15 COL 20.
      *
           STRING ' REGISTROS DESPREZADOS...........: '
                    WSS-REG-DESPREZADOS
              DELIMITED BY SIZE   INTO WSS-STRING
           END-STRING.
      *
           DISPLAY WSS-STRING          LINE 17 COL 20.
      *
           DISPLAY '-----------------------------------------'
                                       LINE 19 COL 20.
      *
           MOVE " Pressione qualquer tecla para sair. "
                                       TO WSS-MENSAGEM
           DISPLAY TELA-MENSAGENS.
           ACCEPT WSS-ENT              LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP.
      *
       RT-EXIBIR-TOTALX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-FINALIZAR                    SECTION.
      *  ---> Finaliza o programa e retorna.
      *----------------------------------------------------------------*
      *
      *  ---> Fecha arquivos.
           PERFORM RT-FECHAR-ARQUIVOS  THRU RT-FECHAR-ARQUIVOSX.
      *
           MOVE 'N'                    TO WSS-FIM-IMPORT.
      *
      *  ---> Exibe totalizadores.
           IF WSS-ENDERECO-ARQ         EQUAL 'S' OR 's'
              CONTINUE
           ELSE
           PERFORM RT-EXIBIR-TOTAL     THRU RT-EXIBIR-TOTALX
           END-IF.
      *
           IF WSS-ERRO                 EQUAL 'N' OR 'n'
              MOVE '00'                TO LKS-RETORNO
           ELSE
              MOVE '01'                TO LKS-RETORNO
           END-IF.
      *
       RT-FINALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                   F I M  D O  P R O G R A M A
      *----------------------------------------------------------------*