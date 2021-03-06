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
      *     *#NOME     : PT000001 ---> MANUTEN��O -> CLIENTES/VENDEDOR *
      *     *----------------------------------------------------------*
      *     *#TIPO     : COBOL - MICRO FOCUS - NET EXPRESS 5           *
      *     *----------------------------------------------------------*
      *     *#ANALISTA : AMARILDO MBORGES                              *
      *     *----------------------------------------------------------*
      *     *#FUN��O   : MANUTENCAO DE CLIENTES E VENDEDORES           *
      *     *           (INCLUS�O, ALTERA��O, EXCLUS�O, IMPORTA��O).   *
      *     *----------------------------------------------------------*
      *     * VERS�O 01    -    Amarildo M Borges      -    09.03.2020 *
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
           COPY 'SEL-VENDEDOR.CPY'     REPLACING ==::== BY == -FDV ==.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-VENDEDOR                VALUE OF FILE-ID IS 'VENDEDOR'
           LABEL RECORDS               IS STANDARD
           DATA RECORD                 IS REG-FDV.
       COPY 'FD-VENDEDOR.CPY'          REPLACING ==::== BY == -FDV ==.
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
           03  FS-VENDEDOR             PIC  X(002)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
      *    TABELA DE OP��ES PARA EXIBIR FUN��ES DE MENU
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
      *    VARIAVEIS - AUXILIARES PARA REGISTROS VENDEDOR / VENDEDORES
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
      *    AREA DO ARQUIVO VENDEDOR NA WORKING
      *----------------------------------------------------------------*
       COPY 'FD-VENDEDOR.CPY'          REPLACING ==::== BY == -WSV ==.
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
      *    AREA PARA CHAMADA AO SUBPROGRAMA IMPORTAR VENDEDOR          *
      *----------------------------------------------------------------*
      *
       01  WPT0901V                    PIC X(08)           VALUE
                                                             'PT00901V'.
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
      *    TELA MANUTENCAO - OP��ES PARA MANUTEN��O DE REGISTROS
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
      *    TELA LABEL VENDEDOR - CADASTRO
      *----------------------------------------------------------------*
       01  TELA-LABEL-VENDEDOR.
           03 F LINE 05 COL 28        VALUE " VENDEDOR -"       BLINK.
           03 F LINE 05 COL 39        PIC X(12) USING WSS-TITULO BLINK.
           03 F LINE 08 COL 24        VALUE " Codigo......: ".
           03 F LINE 10 COL 24        VALUE " CPF.........: ".
           03 F LINE 12 COL 24        VALUE " Nome........: ".
           03 F LINE 14 COL 24        VALUE " Latitude....: ".
           03 F LINE 16 COL 24        VALUE " Longitude...: ".
      *
      *----------------------------------------------------------------*
      *    TELA DADOS VENDEDOR - CADASTRO
      *----------------------------------------------------------------*
       01  TELA-DADOS-VENDEDOR-U.
           03 U1C LINE 08 COL 39 PIC  ZZZZZZ9       USING CODIGO-WSV.
           03 U2C LINE 10 COL 39 PIC  9(011)        USING CPF-WSV.
           03 U3C LINE 12 COL 39 PIC  X(040)        USING NOME-WSV.
           03 U4C LINE 14 COL 39 PIC -ZZ9,99999999  USING LATITUDE-WSV.
           03 U5C LINE 16 COL 39 PIC -ZZ9,99999999  USING LONGITUDE-WSV.
      *
       01  TELA-DADOS-VENDEDOR-T.
           03 T1C LINE 08 COL 39 PIC  ZZZZZZ9       TO    CODIGO-WSV.
           03 T2C LINE 10 COL 39 PIC  9(011)        TO    CPF-WSV.
           03 T3C LINE 12 COL 39 PIC  X(040)        TO    NOME-WSV.
           03 T4C LINE 14 COL 39 PIC -ZZ9,99999999  TO    LATITUDE-WSV.
           03 T5C LINE 16 COL 39 PIC -ZZ9,99999999  TO    LONGITUDE-WSV.
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
                      REG-FDV
                      REG-WSV
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
           MOVE '   ..... VENDEDOR .....   '
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
      *    MOVE ZEROS                  TO CODIGO-WSV.
      *
           MOVE WSS-TIT-TAB(WSS-FUN)   TO WSS-TITULO.
      *
           DISPLAY TELA-LABEL-VENDEDOR.
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
              MOVE WSS-PROXIMO         TO CODIGO-WSV
                                          WSS-CODIGO
              DISPLAY TELA-DADOS-VENDEDOR-T
                      U1C
      *
      *  ---> Alteracao e Exclusao
           ELSE
              MOVE
                 'Informe codigo do vendedor ou digite ZEROS para sair.'
                                       TO WSS-MENSAGEM
              DISPLAY TELA-MENSAGENS
      *
              IF CODIGO-WSV            EQUAL ZEROS
                 ACCEPT T1C
              ELSE
                 ACCEPT U1C
              END-IF
              MOVE CODIGO-WSV          TO WSS-CODIGO
      *
      *  ---> Volta para o menu de funcoes.
              IF WSS-CODIGO            EQUAL ZEROS
                 PERFORM RT-PROCESSAR
              END-IF
      *
      *  ---> Verifica se codigo ja foi cadastrado.
      *  ---> Leitura por chave primaria - Codigo.
              MOVE 'S'                 TO WSS-LER-KEY-P
              MOVE CODIGO-WSV          TO CODIGO-FDV
              PERFORM RT-LER-VENDEDOR  THRU RT-LER-VENDEDORX
      *
              IF WSS-ACHOU             EQUAL ZEROS
                 MOVE "Vendedor nao cadastrado"
                                       TO WSS-MENSAGEM
                 DISPLAY TELA-MENSAGENS
                 ACCEPT WSS-ENT        LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
                 PERFORM REC-COD
              ELSE
      *  ---> Exibe dados ja cadastrados
                 DISPLAY TELA-DADOS-VENDEDOR-U
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
       REC-CPF-CPF.
      *
           MOVE 'Informe o CPF do vendedor ou digite ZEROS para sair.'
                                       TO WSS-MENSAGEM.
           DISPLAY TELA-MENSAGENS.
      *
           IF CPF-WSV                 EQUAL ZEROS
              ACCEPT T2C
           ELSE
              ACCEPT U2C
           END-IF
      *
      *  ---> Volta para o menu de funcoes.
           IF CPF-WSV                 EQUAL ZEROS
              PERFORM RT-PROCESSAR
           END-IF
      *
      *  ---> Validar CPF.
           IF WSS-FUN                  EQUAL 1 OR
              CPF-WSV                 NOT EQUAL CPF-FDV
              MOVE 'N'                 TO WSS-ERRO
              MOVE ZEROS               TO RETORNO-0900
              MOVE CPF-WSV            TO NUMERO-0900
              MOVE 'CPF'              TO TIPO-CALCULO-0900
              PERFORM RT-VALIDAR-CPF-CNPJ
                                       THRU RT-VALIDAR-CPF-CNPJX
           END-IF
      *
      *  ---> Volta para receber CNPJ.
      *  ---> So sai quando for zeros ou valido.
           IF WSS-ERRO                 EQUAL 'S' OR 's'
              PERFORM REC-CPF-CPF
           ELSE
      *  ---> Verificar se CNPJ ja esta cadastrado.
      *  ---> Leitura por chave alternativa - CNPJ / CPF.
              MOVE 'N'                 TO WSS-LER-KEY-P
              MOVE CPF-WSV            TO CPF-FDV
              PERFORM RT-LER-VENDEDOR  THRU RT-LER-VENDEDORX
           END-IF.
      *
           IF WSS-ACHOU                GREATER ZEROS AND
              WSS-FUN                  EQUAL 1
              MOVE 'CPF ja cadastrado. Pressione ENTER para voltar.'
                                       TO WSS-MENSAGEM
              DISPLAY TELA-DADOS-VENDEDOR-U
      *
              DISPLAY TELA-MENSAGENS
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
      *
              MOVE WSS-PROXIMO         TO CODIGO-WSV
      *
              PERFORM REC-CPF-CPF
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
           INITIALIZE                  REG-FDV
                                       REG-WSV
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
           CALL WPT0901V               USING LKS-PARM.
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
           MOVE "Informe a Razao Social do vendedor."
                                       TO WSS-MENSAGEM

           DISPLAY TELA-MENSAGENS.
      *
           IF NOME-WSV              EQUAL SPACES
              ACCEPT T3C
           ELSE
              ACCEPT U3C
           END-IF
      *
           IF NOME-WSV              EQUAL SPACES
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
           IF LATITUDE-WSV          EQUAL ZEROS
              ACCEPT T4C
           ELSE
              ACCEPT U4C
           END-IF
      *
           IF LATITUDE-WSV             EQUAL ZEROS
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
           IF LONGITUDE-WSV            EQUAL ZEROS
              ACCEPT T5C
           ELSE
              ACCEPT U5C
           END-IF
      *
           IF LONGITUDE-WSV            EQUAL ZEROS
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
      *    ABRIR ARQUIVO VENDEDOR / VENDEDOR
      *----------------------------------------------------------------*
       RT-ABRIR-ARQUIVOS               SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQ-VENDEDOR.
      *
           EVALUATE FS-VENDEDOR
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
                    MOVE 'VENDEDOR'    TO WSS-ARQUIVO
                    MOVE FS-VENDEDOR   TO WSS-FSTATUS
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
           OPEN OUTPUT ARQ-VENDEDOR.
      *
           IF FS-VENDEDOR              EQUAL '00'
              PERFORM RT-FECHAR-ARQUIVOS
                                       THRU RT-FECHAR-ARQUIVOSX
           ELSE
              MOVE ' ao criar  '       TO WSS-DESCRICAO
              MOVE 'VENDEDOR'          TO WSS-ARQUIVO
              MOVE FS-VENDEDOR         TO WSS-FSTATUS
              MOVE 2                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-CRIAR-ARQUIVOSX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    LEITURA ARQUIVO VENDEDOR
      *----------------------------------------------------------------*
       RT-LER-VENDEDOR                 SECTION.
      *----------------------------------------------------------------*
      *
      *  ---> Busca registro por Codigo.
           IF WSS-LER-KEY-P            EQUAL 'S' OR 's'
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
           EVALUATE FS-VENDEDOR
               WHEN '00'
                    ADD 1              TO WSS-ACHOU
               WHEN '23'
                    CONTINUE
               WHEN OTHER
                    MOVE 'na leitura ' TO WSS-DESCRICAO
                    MOVE 'VENDEDOR'    TO WSS-ARQUIVO
                    MOVE FS-VENDEDOR   TO WSS-FSTATUS
                    MOVE 3             TO WSS-PONTO-ERRO
                    PERFORM RT-ERROS   THRU RT-ERROSX
           END-EVALUATE.
      *
       RT-LER-VENDEDORX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVAR REGISTRO NO ARQUIVO VENDEDOR
      *----------------------------------------------------------------*
       RT-GRAVAR-REGISTROS             SECTION.
      *----------------------------------------------------------------*
      *
           IF WSS-FUN                  EQUAL 1
              WRITE REG-FDV            FROM REG-WSV
           ELSE 
              REWRITE REG-FDV          FROM REG-WSV
           END-IF.
      *
           IF FS-VENDEDOR              EQUAL '00'
              IF WSS-FUN               EQUAL 1
                 MOVE "Vendedor gravado com sucesso!"
                                       TO WSS-MENSAGEM
              ELSE
                 MOVE "Vendedor alterado com sucesso!"
                                       TO WSS-MENSAGEM
              END-IF
              DISPLAY TELA-MENSAGENS
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
           ELSE
              MOVE 'ao gravar  '       TO WSS-DESCRICAO
              MOVE 'VENDEDOR'          TO WSS-ARQUIVO
              MOVE FS-VENDEDOR         TO WSS-FSTATUS
              MOVE 4                   TO WSS-PONTO-ERRO
              PERFORM RT-ERROS         THRU RT-ERROSX
           END-IF.
      *
       RT-GRAVAR-REGISTROSX.
           EXIT.
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA EXCLUIR REGISTROS ARQUIVO VENDEDOR / VENDEDOR
      *----------------------------------------------------------------*
       RT-EXCLUIR-REGISTROS            SECTION.
      *----------------------------------------------------------------*
      *
           DELETE ARQ-VENDEDOR.
      *
           IF FS-VENDEDOR              EQUAL '00'
              MOVE "Vendedor excluido com sucesso!"
                                       TO WSS-MENSAGEM
              DISPLAY TELA-MENSAGENS
              ACCEPT WSS-ENT           LINE 24 COL 80
                                            WITH PROMPT AUTO-SKIP
           ELSE
              MOVE 'ao excluir '       TO WSS-DESCRICAO
              MOVE 'VENDEDOR'          TO WSS-ARQUIVO
              MOVE FS-VENDEDOR         TO WSS-FSTATUS
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
           CLOSE ARQ-VENDEDOR.
      *
           IF FS-VENDEDOR              EQUAL '00'
              CONTINUE
           ELSE
              MOVE 'ao fechar  '       TO WSS-DESCRICAO
              MOVE 'VENDEDOR'          TO WSS-ARQUIVO
              MOVE FS-VENDEDOR         TO WSS-FSTATUS
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
           COMPUTE WSS-PROXIMO         EQUAL CODIGO-WSV + 1.
      *
           INITIALIZE REG-FDV
                      REG-WSV
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
              READ ARQ-VENDEDOR
                   NEXT
                   INTO REG-WSV
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
                  'de dados do vendedor? (S/N): '
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
                    MOVE "CPF verificado e invalido."
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