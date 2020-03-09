      *----------------------------------------------------------------*
      *    COPYBOOK - DECLARACAO SELECT ARQUIVO CLIENTES                  
      *----------------------------------------------------------------*
      *
           SELECT  ARQ-CLIENTES        ASSIGN TO DISK
                   ORGANIZATION        IS INDEXED
                   ACCESS MODE         IS DYNAMIC
                   RECORD KEY          IS CODIGO::
                   ALTERNATE KEY       IS CNPJ::      WITH DUPLICATES
                   ALTERNATE KEY       IS RSOCIAL::   WITH DUPLICATES
                   LOCK MODE           IS MANUAL
                   FILE STATUS         IS FS-CLIENTES.