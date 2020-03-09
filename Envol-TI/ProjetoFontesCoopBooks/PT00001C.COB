      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     PT000001.
       AUTHOR.                         AmarildoMB.
       DATE-WRITTEN.                   09 MAR 2020.
       DATE-COMPILED.
      *----------------------------------------------------------------*
      *REMARKS.
      *     *----------------------------------------------------------*
      *     *#NOME     : PT000001 ---> MANUTENÇÃO -> CLIENTES/VENDEDOR *
      *     *----------------------------------------------------------*
      *     *#TIPO     : COBOL - MICRO FOCUS - NET EXPRESS 5           *
      *     *----------------------------------------------------------*
      *     *#ANALISTA : AMARILDO MBORGES                              *
      *     *----------------------------------------------------------*
      *     *#FUNÇÃO   : MANUTENCAO DE CLIENTES E VENDEDORES           *
      *     *           (INCLUSÃO, ALTERAÇÃO, EXCLUSÃO, IMPORTACAO).   *
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
           COPY 'SEL-CLIENTES.CPY'     REPLACING ==::== BY == -FDC ==.
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
       77  WSS-FIM-ARQ                 PIC  X(001)         VALUE 'N'.
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
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - FILE STATUS
      *----------------------------------------------------------------*
           03  FS-CLIENTES             PIC  X(002)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *    TABELA DE OPÇÕES PARA EXIBIR FUNÇÕES DE MENU
      *----------------------------------------------------------------*
       01  WSS-TAB-OPC.
           03 F                        PIC X(12) VALUE ' Inclusao   '.
           03 F                        PIC X(12) VALUE ' Alteracao  '.
           03 F                        PIC X(12) VALUE ' Exclusao   '.
           03 F                        PIC X(12) VALUE ' Importacao '.
       01  F                           REDEFINES WSS-TAB-OPC.
           03 WSS-TIT-TAB              PIC X(12) OCCURS 4.
      *
      *----------------------------------------------------------------*
      *    VARIAVEIS - AUXILIARES PARA REGISTROS CLIENTES / VENDEDORES
      *----------------------------------------------------------------
       01  WSS-CLIVEN.
           03  WSS-CODIGO              PIC  9(007).
           03  WSS-CNPJ                PIC  9(014).
           03  WSS-CPF                 PIC  9(011).
           03  WSS-RSOC-NOM            PIC  X(040).
           03  WSS-LATITUDE            PIC S9(003)V9(008).
           03  WSS-LONGITUDE           PIC S9(003)V9(008).
      *
      *----------------------------------------------------------------*
      *    AREA DO ARQUIVO CLIENTES NA WORKING
      *----------------------------------------------------------------*
       COPY 'FD-CLIENTES.CPY'          REPLACING ==::== BY == -WSC ==.
      *
      *----------------------------------------------------------------*
      *    AREA PARA CHAMADA A SUBPROGRAMAS
      *----------------------------------------------------------------*       
      *    AREA PARA CHAMADA AO SUBPROGRAMA VALIDA CPF / CNPJ          *
      *----------------------------------------------------------------*
      *
       01  WPT00900                    PIC X(08)           VALUE
                                                             'PT000900'.
      *
           COPY 'CPF-CNPJ.CPY'         REPLACING ==::== BY == 0900 ==.
      *
      *----------------------------------------------------------------*       
      *    AREA PARA CHAMADA AO SUBPROGRAMA IMPORTAR CLIENTES          *
      *----------------------------------------------------------------*
      *
       01  WPT0901C                    PIC X(08)           VALUE
                                                             'PT00901C'.
      *
      *----------------------------------------------------------------*
      *    Parametros para programas chamados
      *----------------------------------------------------------------*
      *
           COPY 'SUB-PRGS.CPY'         REPLACING ==::== BY == SBP ==.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           'FFF  FIM DA WORKING-STORAGE  FFF'.
      *
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
      *
           COPY 'SUB-PRGS.CPY'         REPLACING ==::== BY == LKS ==.
      *
      *----------------------------------------------------------------*
      *    AREA DE TELAS
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      *    TELA MANUTENCAO - OPÇÕES PARA MANUTENÇÃO DE REGISTROS
      *----------------------------------------------------------------*
       01  TELA-MANUTENCAO.
           03 F LINE 05 COL 28        PIC X(26) USING WSS-TIT-MAN BLINk.
           03 F LINE 08 COL 28        VALUE " 1 - Inclusao   ".
           03 F LINE 10 COL 28        VALUE " 2 - Alteracao  ".
           03 F LINE 12 COL 28        VALUE " 3 - Exclusao   ".
           03 F LINE 14 COL 28        VALUE " 4 - Importacao ".
           03 F LINE 16 COL 28        VALUE " 9 - Sair       ".
           03 F LINE 18 COL 28        VALUE " Opcao:[ ]      ".
      *
      *----------------------------------------------------------------*
      *    TELA LABEL CLIENTES - CADASTRO
      *----------------------------------------------------------------*
       01  TELA-LABEL-CLIENTES.
           03 F LINE 05 COL 28        VALUE " Clientes -"       BLINK.
           03 F LINE 05 COL 39        PIC X(12) USING WSS-TITULO BLINK.
           03 F LINE 08 COL 24        VALUE " Codigo......: ".
           03 F LINE 10 COL 24        VALUE " CNPJ........: ".
           03 F LINE 12 COL 24        VALUE " Razao Social: ".
           03 F LINE 14 COL 24        VALUE " Latitude....: ".
           03 F LINE 16 COL 24        VALUE " Longitude...: ".
      *
      *----------------------------------------------------------------*
      *    TELA DADOS CLIENTES - CADASTRO
      *----------------------------------------------------------------*
       01  TELA-DADOS-CLIENTES-U.
           03 U1C LINE 08 COL 39 PIC  ZZZZZZ9       USING CODIGO-WSC.
           03 U2C LINE 10 COL 39 PIC  9(014)        USING CNPJ-WSC.
           03 U3C LINE 12 COL 39 PIC  X(040)        USING RSOCIAL-WSC.
           03 U4C LINE 14 COL 39 PIC -ZZ9,99999999  USING LATITUDE-WSC.
           03 U5C LINE 16 COL 39 PIC -ZZ9,99999999  USING LONGITUDE-WSC.
      *
       01  TELA-DADOS-CLIENTES-T.
           03 T1C LINE 08 COL 39 PIC  ZZZZZZ9       TO    CODIGO-WSC.
           03 T2C LINE 10 COL 39 PIC  9(014)        TO    CNPJ-WSC.
           03 T3C LINE 12 COL 39 PIC  X(040)        TO    RSOCIAL-WSC.
           03 T4C LINE 14 COL 39 PIC -ZZ9,99999999  TO    LATITUDE-WSC.
           03 T5C LINE 16 COL 39 PIC -ZZ9,99999999  TO    LONGITUDE-WSC.
      *
      *----------------------------------------------------------------*
      *    TELA DIVERSOS MENSAGEMS
      *----------------------------------------------------------------* 
       01  TELA-MENSAGENS.
      * ---> Linha Limpa Mensagem
           03 LL-M LINE 24 COL 01      BLANK LINE.
      *
      * ---> Linha Exibe Label
           03 LE-L LINE 24 COL 01      VALUE " Mensagem:" BLINK.
      *
      * ---> Linha Exibe Mensagem
           03 LE-M LINE 24 COL 13      PIC  X(067) USING WSS-MENSAGEM.
      *
      *================================================================*
       PROCEDURE                       DIVISION USING LKS-PARM.  
      *================================================================*
       RT-PRINCIPAL                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM RT-INICIALIZAR      THRU RT-INICIALIZARX.
      *
           PERFORM UNTIL WSS-FUN       EQUAL 9
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
      *----------------------------------------------------------------*
      *
      *  ---> Inicializ areas e variaveis
           INITIALIZE WSS-AUXILIARES
                      REG-FDC
                      REG-WSC
                      WSS-CLIVEN
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
      *----------------------------------------------------------------*
      *
           PERFORM RT-LIMPAR-TELA      THRU RT-LIMPAR-TELAX.
      *
           MOVE ZEROS                  TO WSS-FUN.
      *
           MOVE "Informe opcao desejada."
                                       TO WSS-MENSAGEM.
      *
           MOVE '   ..... CLIENTES .....   '
                                       TO WSS-TIT-MAN.
      *
           DISPLAY TELA-MANUTENCAO
                   TELA-MENSAGENS.
      *            
           ACCEPT WSS-FUN              LINE 18 COL 36
                                            WITH PROMPT AUTO-SKIP.
      *
           EVALUATE WSS-FUN
               WHEN 1
               WHEN 2
               WHEN 3
                    PERFORM RT-COMUM   THRU RT-COMUMX
               WHEN 4
                    PERFORM RT-IMPORTACAO
                                       THRU RT-IMPORTACAOX
               WHEN 9
                    CONTINUE
               WHEN OTHER
                    MOVE "Opcao invalida!!! Tente novamente."
                                       TO WSS-MENSAGEM
                    DISPLAY TELA-MENSAGENS
                    ACCEPT WSS-ENT     LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP

           END-EVALUATE.
      *
       RT-PROCESSARX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-COMUM                        SECTION.
      *----------------------------------------------------------------*
      *  ---> Recebe campos e consiste valores.
      *----------------------------------------------------------------*
       REC-COD.
      *
           PERFORM RT-LIMPAR-TELA      THRU RT-LIMPAR-TELAX.
      *
           MOVE WSS-TIT-TAB(WSS-FUN)   TO WSS-TITULO.
      *
           DISPLAY TELA-LABEL-CLIENTES.
      *
      *  ---> Inclusao
           IF WSS-FUN                  EQUAL 1
      *
      *  ---> Monta valor para o campo codigo
              IF WSS-PROXIMO           EQUAL ZEROS
                 PERFORM RT-AUTOINCREMENTO
                                       THRU RT-AUTOINCREMENTOX
              ELSE
                 ADD 1                 TO WSS-PROXIMO
              END-IF
      *
              MOVE WSS-PROXIMO         TO CODIGO-WSC
                                          WSS-CODIGO
              DISPLAY TELA-DADOS-CLIENTES-T
                      U1C
      *
      *  ---> Alteracao e Exclusao
           ELSE
              MOVE
                 'Informe codigo do cliente ou digite ZEROS para sair.'
                                       TO WSS-MENSAGEM
              DISPLAY TELA-MENSAGENS
      *
              IF CODIGO-WSC            EQUAL ZEROS
                 ACCEPT T1C
              ELSE
                 ACCEPT U1C
              END-IF
              MOVE CODIGO-WSC          TO WSS-CODIGO
      *
      *  ---> Volta para o menu de funcoes.
              IF WSS-CODIGO            EQUAL ZEROS
                 PERFORM RT-PROCESSAR
              END-IF
      *
      *  ---> Verifica se codigo ja foi cadastrado.
      *  ---> Leitura por chave primaria - Codigo.
              MOVE 'S'                 TO WSS-LER-KEY-P
              MOVE CODIGO-WSC          TO CODIGO-FDC
              PERFORM RT-LER-CLIENTES  THRU RT-LER-CLIENTESX
      *
              IF WSS-ACHOU             EQUAL ZEROS
                 MOVE "Cliente nao cadastrado"
                                       TO WSS-MENSAGEM
                 DISPLAY TELA-MENSAGENS
                 ACCEPT WSS-ENT        LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
                 PERFORM REC-COD
              ELSE
      *  ---> Exibe dados ja cadastrados
                 DISPLAY TELA-DADOS-CLIENTES-U
              END-IF
           END-IF.
      *
      *  ---> Exclusao
           IF WSS-FUN                  EQUAL 3
              PERFORM RT-EXCLUSAO      THRU RT-EXCLUSAOX
              PERFORM REC-COD
           END-IF.
      *
      *----------------------------------------------------------------*
      *  ---> Receber CNPJ na tela
      *----------------------------------------------------------------*
       REC-CNPJ-CPF.
      *
           MOVE 'Informe o CNPJ do cliente ou digite ZEROS para sair.'
                                       TO WSS-MENSAGEM.
           DISPLAY TELA-MENSAGENS.
      *
           IF CNPJ-WSC                 EQUAL ZEROS
              ACCEPT T2C
           ELSE
              ACCEPT U2C
           END-IF
      *
      *  ---> Volta para o menu de funcoes.
           IF CNPJ-WSC                 EQUAL ZEROS
              PERFORM RT-PROCESSAR
           END-IF
      *
      *  ---> Validar CNPJ.
           IF WSS-FUN                  EQUAL 1 OR
              CNPJ-WSC                 NOT EQUAL CNPJ-FDC
              MOVE 'N'                 TO WSS-ERRO
              MOVE ZEROS               TO RETORNO-0900
              MOVE CNPJ-WSC            TO NUMERO-0900
              MOVE 'CNPJ'              TO TIPO-CALCULO-0900
              PERFORM RT-VALIDAR-CPF-CNPJ
                                       THRU RT-VALIDAR-CPF-CNPJX
           END-IF
      *
      *  ---> Volta para receber CNPJ.
      *  ---> So sai quando for zeros ou valido.
           IF WSS-ERRO                 EQUAL 'S' OR 's'
              PERFORM REC-CNPJ-CPF
           ELSE
      *  ---> Verificar se CNPJ ja esta cadastrado.
      *  ---> Leitura por chave alternativa - CNPJ / CPF.
              MOVE 'N'                 TO WSS-LER-KEY-P
              MOVE CNPJ-WSC            TO CNPJ-FDC
              PERFORM RT-LER-CLIENTES  THRU RT-LER-CLIENTESX
           END-IF.
      *
           IF WSS-ACHOU                GREATER ZEROS AND
              WSS-FUN                  EQUAL 1
              MOVE 'CNPJ ja cadastrado. Pressione ENTER para voltar.'
                                       TO WSS-MENSAGEM
              DISPLAY TELA-DADOS-CLIENTES-U
      *
              DISPLAY TELA-MENSAGENS
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
      *
              MOVE WSS-PROXIMO         TO CODIGO-WSC
      *
              PERFORM REC-CNPJ-CPF
           END-IF.
      *
      *  ---> Consiste demais campos para inclusao ou alteracao.
           IF WSS-FUN                  EQUAL 1 OR 2
              PERFORM RT-CONSISTENCIA  THRU RT-CONSISTENCIAX
           END-IF.
      *
      *  ---> Gravar registros no arquivo.
           IF WSS-FUN                  EQUAL 1
              PERFORM RT-GRAVAR-REGISTROS
                                       THRU RT-GRAVAR-REGISTROSX
           ELSE
              PERFORM RT-ALTERACAO     THRU RT-ALTERACAOX
           END-IF.
      *
           INITIALIZE                  REG-FDC
                                       REG-WSC
              REPLACING ALPHANUMERIC   BY SPACES
                        NUMERIC        BY ZEROS.
      *
           PERFORM RT-COMUM            THRU RT-COMUMX.
      *
       RT-COMUMX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-INCLUSAO                     SECTION.
      *----------------------------------------------------------------*
           MOVE '  inclusao'           TO WSS-DESCRICAO.
           PERFORM RT-CONFIRMA         THRU RT-CONFIRMAX.
                                       
      *  ---> Gravar registros no arquivo.
           IF WSS-SON                  EQUAL 'S' OR 's'
              PERFORM RT-GRAVAR-REGISTROS
                                       THRU RT-GRAVAR-REGISTROSX
           END-IF.
      *
       RT-INCLUSAOX. 
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-ALTERACAO                    SECTION.
      *----------------------------------------------------------------*
           MOVE ' alteracao'           TO WSS-DESCRICAO.
           PERFORM RT-CONFIRMA         THRU RT-CONFIRMAX.
                                       
      *  ---> Gravar registros no arquivo.
           IF WSS-SON                  EQUAL 'S' OR 's'
              PERFORM RT-GRAVAR-REGISTROS
                                       THRU RT-GRAVAR-REGISTROSX
           END-IF.
      *
       RT-ALTERACAOX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-EXCLUSAO                     SECTION.
      *----------------------------------------------------------------*
           MOVE '  exclusao'           TO WSS-DESCRICAO.
           PERFORM RT-CONFIRMA         THRU RT-CONFIRMAX.

           IF WSS-SON                  EQUAL 'S' OR 's'
              PERFORM RT-EXCLUIR-REGISTROS
                                       THRU RT-EXCLUIR-REGISTROSX
           END-IF.
      *
       RT-EXCLUSAOX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-IMPORTACAO                   SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'N'                    TO WSS-ERRO
      *
           CALL WPT0901C               USING LKS-PARM.
      *
           IF LKS-RETORNO              EQUAL '00'
              CONTINUE
           ELSE
              MOVE 'Erro de processamento programa 901'
                                       TO WSS-MENSAGEM
              DISPLAY TELA-MENSAGENS
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
           END-IF.
      *
       RT-IMPORTACAOX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-CONSISTENCIA                 SECTION.
      *----------------------------------------------------------------*
      *  ---> Receber Razao Social na tela
      *----------------------------------------------------------------*
       REC-RSOC-NOM.
      *
           MOVE "Informe a Razao Social do cliente."
                                       TO WSS-MENSAGEM

           DISPLAY TELA-MENSAGENS.
      *
           IF RSOCIAL-WSC              EQUAL SPACES
              ACCEPT T3C
           ELSE
              ACCEPT U3C
           END-IF
      *
           IF RSOCIAL-WSC              EQUAL SPACES
              MOVE "Valor invalido para Razao Social."
                                       TO WSS-MENSAGEM
              DISPLAY TELA-MENSAGENS
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
              PERFORM REC-RSOC-NOM
           END-IF.
      *
      *----------------------------------------------------------------*
      *  ---> Receber Latitude na tela
      *----------------------------------------------------------------*
       REC-LATI.
      *
           MOVE "Informe a Latitude."  TO WSS-MENSAGEM.
           DISPLAY TELA-MENSAGENS.
      *
           IF LATITUDE-WSC          EQUAL ZEROS
              ACCEPT T4C
           ELSE
              ACCEPT U4C
           END-IF
      *
           IF LATITUDE-WSC             EQUAL ZEROS
              MOVE "Valor invalido para Latitude."
                                       TO WSS-MENSAGEM
              DISPLAY TELA-MENSAGENS
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
              PERFORM REC-LATI
           END-IF.
      *
      *----------------------------------------------------------------*
      *  ---> Receber Longitude na tela
      *----------------------------------------------------------------*
       REC-LONG.
      *
           MOVE "Informe a Longitude." TO WSS-MENSAGEM.
           DISPLAY TELA-MENSAGENS.
      *
           IF LONGITUDE-WSC            EQUAL ZEROS
              ACCEPT T5C
           ELSE
              ACCEPT U5C
           END-IF
      *
           IF LONGITUDE-WSC            EQUAL ZEROS
              MOVE "Valor invalido para Longitude."
                                       TO WSS-MENSAGEM
              DISPLAY TELA-MENSAGENS
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
              PERFORM REC-LONG
           END-IF.
      *
       RT-CONSISTENCIAX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    ABRIR ARQUIVO CLIENTES / VENDEDOR
      *----------------------------------------------------------------*
       RT-ABRIR-ARQUIVOS               SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQ-CLIENTES.
      *
           EVALUATE FS-CLIENTES
               WHEN '00'
                    CONTINUE
               WHEN '05'
                    MOVE
                       'Arquivo inexistente. Deseja cria-lo (S/N): '
                                       TO WSS-MENSAGEM
                    DISPLAY TELA-MENSAGENS
                    MOVE 'N'           TO WSS-SON
                    ACCEPT WSS-SON     LINE 24 COL 56
                                            WITH PROMPT AUTO-SKIP
                    IF WSS-SON         EQUAL 'S' OR 's'
                       PERFORM RT-CRIAR-ARQUIVOS
                                       THRU RT-CRIAR-ARQUIVOSX
                       PERFORM RT-ABRIR-ARQUIVOS
                                       THRU RT-ABRIR-ARQUIVOSX
                    ELSE
                       PERFORM RT-PROCESSAR
                                       THRU RT-PROCESSARX
                    END-IF
               WHEN OTHER
                    MOVE ' ao abrir  ' TO WSS-DESCRICAO
                    MOVE 'CLIENTES'    TO WSS-ARQUIVO
                    MOVE FS-CLIENTES   TO WSS-FSTATUS
                    MOVE 1             TO WSS-PONTO-ERRO
                    PERFORM RT-ERROS   THRU RT-ERROSX
           END-EVALUATE.
      *
       RT-ABRIR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    CRIAR ARQUIVOS
      *----------------------------------------------------------------*
       RT-CRIAR-ARQUIVOS               SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM RT-FECHAR-ARQUIVOS  THRU RT-FECHAR-ARQUIVOSX.
      *
           OPEN OUTPUT ARQ-CLIENTES.
      *
           IF FS-CLIENTES              EQUAL '00'
              PERFORM RT-FECHAR-ARQUIVOS
                                       THRU RT-FECHAR-ARQUIVOSX
           ELSE
              MOVE ' ao criar  '       TO WSS-DESCRICAO
              MOVE 'CLIENTES'          TO WSS-ARQUIVO
              MOVE FS-CLIENTES         TO WSS-FSTATUS
              MOVE 2                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-CRIAR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    LEITURA ARQUIVO CLIENTES
      *----------------------------------------------------------------*
       RT-LER-CLIENTES                 SECTION.
      *----------------------------------------------------------------*
      *
      *  ---> Busca registro por Codigo.
           IF WSS-LER-KEY-P            EQUAL 'S' OR 's'
              READ ARQ-CLIENTES        INTO REG-WSC
                                       KEY IS CODIGO-FDC
                   INVALID KEY
                       MOVE ZEROS      TO WSS-ACHOU
              END-READ
           ELSE
      *  ---> Busca registro por CNPJ.
              READ ARQ-CLIENTES        INTO REG-WSC
                                       KEY IS CNPJ-FDC
                   INVALID KEY
                       MOVE ZEROS      TO WSS-ACHOU
              END-READ
           END-IF.
      *
           EVALUATE FS-CLIENTES
               WHEN '00'
                    ADD 1              TO WSS-ACHOU
               WHEN '23'
                    CONTINUE
               WHEN OTHER
                    MOVE 'na leitura ' TO WSS-DESCRICAO
                    MOVE 'CLIENTES'    TO WSS-ARQUIVO
                    MOVE FS-CLIENTES   TO WSS-FSTATUS
                    MOVE 3             TO WSS-PONTO-ERRO
                    PERFORM RT-ERROS   THRU RT-ERROSX
           END-EVALUATE.
      *
       RT-LER-CLIENTESX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVAR REGISTRO NO ARQUIVO CLIENTES
      *----------------------------------------------------------------*
       RT-GRAVAR-REGISTROS             SECTION.
      *----------------------------------------------------------------*
      *
           IF WSS-FUN                  EQUAL 1
              WRITE REG-FDC            FROM REG-WSC
           ELSE 
              REWRITE REG-FDC          FROM REG-WSC
           END-IF.
      *
           IF FS-CLIENTES              EQUAL '00'
              IF WSS-FUN               EQUAL 1
                 MOVE "Cliente gravado com sucesso!"
                                       TO WSS-MENSAGEM
              ELSE
                 MOVE "Cliente alterado com sucesso!"
                                       TO WSS-MENSAGEM
              END-IF
              DISPLAY TELA-MENSAGENS
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
           ELSE
              MOVE 'ao gravar  '       TO WSS-DESCRICAO
              MOVE 'CLIENTES'          TO WSS-ARQUIVO
              MOVE FS-CLIENTES         TO WSS-FSTATUS
              MOVE 4                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-GRAVAR-REGISTROSX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA EXCLUIR REGISTROS ARQUIVO CLIENTES / VENDEDOR
      *----------------------------------------------------------------*
       RT-EXCLUIR-REGISTROS            SECTION.
      *----------------------------------------------------------------*
      *
           DELETE ARQ-CLIENTES.
      *
           IF FS-CLIENTES              EQUAL '00'
              MOVE "Cliente excluido com sucesso!"
                                       TO WSS-MENSAGEM
              DISPLAY TELA-MENSAGENS
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
           ELSE
              MOVE 'ao excluir '       TO WSS-DESCRICAO
              MOVE 'CLIENTES'          TO WSS-ARQUIVO
              MOVE FS-CLIENTES         TO WSS-FSTATUS
              MOVE 5                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-EXCLUIR-REGISTROSX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA FECHAR ARQUIVOS
      *----------------------------------------------------------------*
       RT-FECHAR-ARQUIVOS              SECTION.
      *----------------------------------------------------------------*
      *
           CLOSE ARQ-CLIENTES.
      *
           IF FS-CLIENTES              EQUAL '00'
              CONTINUE
           ELSE
              MOVE 'ao fechar  '       TO WSS-DESCRICAO
              MOVE 'CLIENTES'          TO WSS-ARQUIVO
              MOVE FS-CLIENTES         TO WSS-FSTATUS
              MOVE 6                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-FECHAR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-LIMPAR-TELA                  SECTION.
      *----------------------------------------------------------------*
      *
      *  ---> Limpa area da tela: linha 4 ate linha 22.
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
       RT-AUTOINCREMENTO               SECTION.
      *----------------------------------------------------------------*
      *
      *  ---> Busca o ultimo registro do arquivo
           PERFORM UNTIL WSS-FIM-ARQ   EQUAL 'S ' OR 's'
              PERFORM RT-LER-SEQUENCIAL
                                       THRU RT-LER-SEQUENCIALX
           END-PERFORM.
      *
      *  ---> Calcula o proximo valor para codigo.
           COMPUTE WSS-PROXIMO         EQUAL CODIGO-WSC + 1.
      *
           INITIALIZE REG-FDC
                      REG-WSC
              REPLACING ALPHANUMERIC   BY SPACES
                        NUMERIC        BY ZEROS.
      *
       RT-AUTOINCREMENTOX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-LER-SEQUENCIAL               SECTION.
      *----------------------------------------------------------------*
      *
      *  ---> Leitura sequencial em arquivo indexado.
              READ ARQ-CLIENTES
                   NEXT
                   INTO REG-WSC
                AT END
                   MOVE 'S'             TO WSS-FIM-ARQ
              END-READ.
      *
       RT-LER-SEQUENCIALX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-ERROS                        SECTION.
      *----------------------------------------------------------------*
      *
      *  ---> Tratamento de erros.
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
           GOBACK.
      *
       RT-ERROSX.     
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-CONFIRMA                     SECTION.
      *----------------------------------------------------------------*
      *
      *  ---> Confirmacao de inclusao, alteracao e exclusao de dados.
      *
           MOVE SPACES                 TO WSS-MENSAGEM.
      *
           STRING 'Confirma'
                   WSS-DESCRICAO
                  'de dados do cliente? (S/N): '
              DELIMITED BY SIZE        INTO WSS-MENSAGEM
           END-STRING.
      *
           DISPLAY TELA-MENSAGENS.
      *
           MOVE 'N'                    TO WSS-SON.
           ACCEPT WSS-SON              LINE 24 COL 61
                                            WITH PROMPT AUTO-SKIP.
      *

           IF NOT SON
              MOVE " Opcao invalida."  TO WSS-MENSAGEM
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
           END-IF.
      *
       RT-CONFIRMAX.     
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-VALIDAR-CPF-CNPJ             SECTION.
      *----------------------------------------------------------------*
      *
      *  ---> Verifica se CNPJ e valido.

           CALL WPT00900               USING REG-0900.
      *
           EVALUATE RETORNO-0900
               WHEN 0
                    MOVE 'N'           TO WSS-LER-KEY-P
               WHEN 1
                    MOVE "CNPJ verificado e invalido."
                                       TO WSS-MENSAGEM
                    DISPLAY TELA-MENSAGENS
                    ACCEPT WSS-ENT     LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
                    MOVE 'S'           TO WSS-ERRO
           END-EVALUATE.
      *
       RT-VALIDAR-CPF-CNPJX.
           EXIT.
      *
      *----------------------------------------------------------------*
       RT-FINALIZAR                    SECTION.
      *----------------------------------------------------------------*
      *
      *  ---> Rotina de finalizacao do programa.

           EVALUATE WSS-PONTO-ERRO
               WHEN 0
                    MOVE ZEROS         TO LKS-RETORNO
                    PERFORM RT-FECHAR-ARQUIVOS
                                       THRU RT-FECHAR-ARQUIVOSX
               WHEN 6
                    STOP RUN
               WHEN OTHER
                    PERFORM RT-FECHAR-ARQUIVOS
                                       THRU RT-FECHAR-ARQUIVOSX
                    STOP RUN
           END-EVALUATE.
      *
       RT-FINALIZARX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *                   F I M  D O  P R O G R A M A
      *----------------------------------------------------------------*