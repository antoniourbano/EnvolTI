      *----------------------------------------------------------------*
      *    COPYBOOK DISTRIB - LAYOUT ARQUIVO DISTRIBUICAO
      *----------------------------------------------------------------*
      *
           SELECT ARQ-DISTRIBUIR       ASSIGN TO DISK
                  ORGANIZATION         IS INDEXED
                  ACCESS MODE          IS DYNAMIC
                  RECORD KEY           IS CCLIENTE::
                  ALTERNATE RECORD KEY IS CVENDEDOR:: WITH DUPLICATES
                  LOCK MODE            IS MANUAL
                  FILE STATUS          IS FS-DISTRIBUIR.