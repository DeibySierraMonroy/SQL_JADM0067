 --**********************************************************************************************************
--** NOMBRE SCRIPT        : TADM0280
--** OBJETIVO             : tabla que permite el seguimiento del envio del archivo concatenado.
--** ESQUEMA              : adm
--** NOMBRE               : SEG_FIRMA_CTO_CONCATENADO
--** AUTOR                : DEIBY SIERRA
--** FECHA MODIFICACION   : 16/03/2023
--**********************************************************************************************************

CREATE TABLE "ADM"."SEG_FIRMA_CTO_CONCATENADO" 
   (	"SCC_ID" NUMBER(9,0), 
	"SCC_URL_ENVIADA" CLOB, 
	"SCC_RESPUESTA" CLOB, 
	"LIB_CONSECUTIVO" NUMBER(12,0), 
	"SCC_INTENTO" NUMBER(12,0), 
	"SCC_TIPO" VARCHAR2(30 BYTE), 
	"AUD_FECHA" DATE, 
	"AUD_USUARIO" VARCHAR2(30 BYTE), 
	"SCC_APLICATIVO" VARCHAR2(2000 BYTE)
   ) SEGMENT CREATION IMMEDIATE 
  PCTFREE 10 PCTUSED 40 INITRANS 1 MAXTRANS 255 
 NOCOMPRESS LOGGING
  STORAGE(INITIAL 65536 NEXT 1048576 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1
  BUFFER_POOL DEFAULT FLASH_CACHE DEFAULT CELL_FLASH_CACHE DEFAULT)
  TABLESPACE "USER_DATA_OLD" 
 LOB ("SCC_URL_ENVIADA") STORE AS BASICFILE (
  TABLESPACE "USER_DATA_OLD" ENABLE STORAGE IN ROW CHUNK 8192 RETENTION 
  NOCACHE LOGGING 
  STORAGE(INITIAL 65536 NEXT 1048576 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1
  BUFFER_POOL DEFAULT FLASH_CACHE DEFAULT CELL_FLASH_CACHE DEFAULT)) 
 LOB ("SCC_RESPUESTA") STORE AS BASICFILE (
  TABLESPACE "USER_DATA_OLD" ENABLE STORAGE IN ROW CHUNK 8192 RETENTION 
  NOCACHE LOGGING 
  STORAGE(INITIAL 65536 NEXT 1048576 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1
   BUFFER_POOL DEFAULT FLASH_CACHE DEFAULT CELL_FLASH_CACHE DEFAULT)) ;
  
  /
  GRANT FLASHBACK ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  GRANT DEBUG ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  GRANT QUERY REWRITE ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  GRANT ON COMMIT REFRESH ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  GRANT REFERENCES ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  GRANT UPDATE ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  GRANT SELECT ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  GRANT INSERT ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  GRANT INDEX ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  GRANT DELETE ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  GRANT ALTER ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" TO PUBLIC;
  
  /
  CREATE UNIQUE INDEX "ADM"."SCC_ID_PK" ON "ADM"."SEG_FIRMA_CTO_CONCATENADO" ("SCC_ID") 
  PCTFREE 10 INITRANS 2 MAXTRANS 255 COMPUTE STATISTICS 
  STORAGE(INITIAL 65536 NEXT 1048576 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1
  BUFFER_POOL DEFAULT FLASH_CACHE DEFAULT CELL_FLASH_CACHE DEFAULT)
  TABLESPACE "USER_DATA_OLD" ;
  
  /
  CREATE SEQUENCE adm.seq_seg_firma_cto_concatenado  start with 1 increment by 1
  maxvalue 99999
  minvalue 1
  NOCACHE ;
  /
  CREATE OR REPLACE TRIGGER "ADM"."DB_SEG_FIRMA_CTO_CONCATENADO" BEFORE
    INSERT ON adm.seg_firma_cto_concatenado
    REFERENCING
    NEW AS new
    FOR EACH ROW
    BEGIN SELECT  adm.seq_seg_firma_cto_concatenado.nextval INTO :new.scc_id FROM dual;      
    EXCEPTION
    WHEN OTHERS THEN
    raise_application_error(-20001, 'Error secuencia db_seg_firma_cto_concatenado :' || sqlerrm);
END;
/
ALTER TRIGGER "ADM"."DB_SEG_FIRMA_CTO_CONCATENADO" ENABLE;
/
  ALTER TABLE "ADM"."SEG_FIRMA_CTO_CONCATENADO" ADD CONSTRAINT "SCC_ID_PK" PRIMARY KEY ("SCC_ID")
  USING INDEX PCTFREE 10 INITRANS 2 MAXTRANS 255 COMPUTE STATISTICS 
  STORAGE(INITIAL 65536 NEXT 1048576 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1
  BUFFER_POOL DEFAULT FLASH_CACHE DEFAULT CELL_FLASH_CACHE DEFAULT)
  TABLESPACE "USER_DATA_OLD"  ENABLE;

