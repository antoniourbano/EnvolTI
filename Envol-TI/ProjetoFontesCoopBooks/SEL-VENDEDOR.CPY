      *----------------------------------------------------------------*
      *    COPYBOOK - DECLARACAO SELECT ARQUIVO VENDEDOR                  
      *----------------------------------------------------------------*
      *
           SELECT  ARQ-VENDEDOR        ASSIGN TO DISK
                   ORGANIZATION        IS INDEXED
                   ACCESS MODE         IS DYNAMIC
                   RECORD KEY          IS CODIGO::
                   ALTERNATE KEY       IS CPF::       WITH DUPLICATES
                   ALTERNATE KEY       IS NOME::      WITH DUPLICATES
                   LOCK MODE           IS MANUAL
                   FILE STATUS         IS FS-VENDEDOR.