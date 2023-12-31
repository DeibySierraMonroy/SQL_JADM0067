 --**********************************************************************************************************
--** NOMBRE SCRIPT        : TADM0279
--** OBJETIVO             : tabla que permite el seguimiento de la firma de mesa de contratos.
--** ESQUEMA              : RHU
--** NOMBRE               : SEG_FIRMA_CTO_MESA 
--** AUTOR                : DEIBY SIERRA
--** FECHA MODIFICACION   : 16/03/2023
--**********************************************************************************************************

 CREATE TABLE "RHU"."SEG_FIRMA_CTO_MESA" 
   (	"SEG_ID" NUMBER(9,0), 
	"LIB_CONSECUTIVO" NUMBER(12,0), 
	"SEG_FIRMADO" VARCHAR2(2 BYTE) DEFAULT 'N', 
	"AUD_FECHA" DATE, 
	"AUD_USUARIO" VARCHAR2(30 BYTE), 
	"SEG_ESTADO" VARCHAR2(3 BYTE), 
	"SEG_FECHA" VARCHAR2(100 BYTE), 
	"CTO_NUMERO" NUMBER(12,0)
   ) SEGMENT CREATION IMMEDIATE 
  PCTFREE 10 PCTUSED 40 INITRANS 1 MAXTRANS 255 
 NOCOMPRESS LOGGING
  STORAGE(INITIAL 65536 NEXT 1048576 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1
  BUFFER_POOL DEFAULT FLASH_CACHE DEFAULT CELL_FLASH_CACHE DEFAULT)
  TABLESPACE "TAB06_AUT" ;
  
  /
    GRANT FLASHBACK ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  GRANT DEBUG ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  GRANT QUERY REWRITE ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  GRANT ON COMMIT REFRESH ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  GRANT REFERENCES ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  GRANT UPDATE ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  GRANT SELECT ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  GRANT INSERT ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  GRANT INDEX ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  GRANT DELETE ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  GRANT ALTER ON "RHU"."SEG_FIRMA_CTO_MESA" TO PUBLIC;
  
  /
   CREATE UNIQUE INDEX "RHU"."SEG_ID_MESA_PK" ON "RHU"."SEG_FIRMA_CTO_MESA" ("SEG_ID") 
  PCTFREE 10 INITRANS 2 MAXTRANS 255 COMPUTE STATISTICS 
  STORAGE(INITIAL 65536 NEXT 1048576 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1
  BUFFER_POOL DEFAULT FLASH_CACHE DEFAULT CELL_FLASH_CACHE DEFAULT)
  TABLESPACE "TAB06_AUT" ;
  
  /
  CREATE SEQUENCE rhu.seq_seg_firma_cto_mesa  start with 1 increment by 1
  maxvalue 99999
  minvalue 1
  NOCACHE ;
  /
  CREATE OR REPLACE TRIGGER "RHU"."DB_RHU_SEG_FIRMA_CTO_MESA" BEFORE
    INSERT ON rhu.seg_firma_cto_mesa
    REFERENCING
    NEW AS new
    FOR EACH ROW
    BEGIN SELECT rhu.seq_seg_firma_cto_mesa.nextval INTO :new.seg_id FROM dual;	  
    EXCEPTION
    WHEN OTHERS THEN
    raise_application_error(-20001, 'Error secuencia seg_firma_cto :' || sqlerrm);
END;
/
ALTER TRIGGER "RHU"."DB_RHU_SEG_FIRMA_CTO_MESA" ENABLE;
/
 ALTER TABLE "RHU"."SEG_FIRMA_CTO_MESA" ADD CONSTRAINT "SEG_ID_MESA_PK" PRIMARY KEY ("SEG_ID")
  USING INDEX PCTFREE 10 INITRANS 2 MAXTRANS 255 COMPUTE STATISTICS 
  STORAGE(INITIAL 65536 NEXT 1048576 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1
  BUFFER_POOL DEFAULT FLASH_CACHE DEFAULT CELL_FLASH_CACHE DEFAULT)
  TABLESPACE "TAB06_AUT"  ENABLE;
/
  ALTER TABLE "RHU"."SEG_FIRMA_CTO_MESA" ADD FOREIGN KEY ("LIB_CONSECUTIVO")
	  REFERENCES "RHU"."LIBROINGRESO" ("LIB_CONSECUTIVO") ENABLE;
  ALTER TABLE "RHU"."SEG_FIRMA_CTO_MESA" ADD FOREIGN KEY ("SEG_FIRMADO")
	  REFERENCES "RHU"."SEG_FIRMA_ESTADO" ("SEG_FIRMADO") ENABLE;
  ALTER TABLE "RHU"."SEG_FIRMA_CTO_MESA" ADD FOREIGN KEY ("SEG_ESTADO")
	  REFERENCES "RHU"."SEG_PERFIL_ESTADO_CTO" ("SEG_ESTADO") ENABLE;

