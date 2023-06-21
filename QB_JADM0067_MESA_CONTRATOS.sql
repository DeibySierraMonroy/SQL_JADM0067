CREATE OR REPLACE PACKAGE  ADM.qb_JADM0067_mesa_contratos  IS
--**********************************************************************************************************
--** NOMBRE SCRIPT        : QADM0186.sql
--** OBJETIVO             : logica Mesa de contratos
--** ESQUEMA              : ADM
--** NOMBRE               : adm.qb_JADM0067_mesa_contratos 
--** AUTOR                : DESIERRA
--** FECHA CREACION       : 12/09/2022
--**********************************************************************************************************

TYPE csCursor                IS REF CURSOR;     

--{Pl_consulta_cedulas
PROCEDURE Pl_consulta_cedulas(VCSTDOESTADO         IN VARCHAR2
                             ,csconsulta           OUT csCursor
                             ,vcestadoproceso      OUT VARCHAR2
                             ,vcmensajeproceso     OUT VARCHAR2
                             );    
--}Pl_consulta_cedulas

--}Pl_consulta_datos                        
PROCEDURE Pl_consulta_datos(nmordenCTO            IN NUMBER                   
                           ,csconsulta           OUT csCursor
                           ,vcestadoproceso      OUT VARCHAR2
                           ,vcmensajeproceso     OUT VARCHAR2
                             ); 


--}Pl_consulta_datos     

--}pl_almacenar_cedula_peritaje 
PROCEDURE pl_almacenar_cedula_peritaje(vcresponsable  IN VARCHAR2  
                            ,vcestadoResponsable IN VARCHAR2 
                            ,nmlibconsecutivo IN NUMBER 
                            ,vcestadoproceso      OUT VARCHAR2
                           ,vcmensajeproceso     OUT VARCHAR2
                             );   
--}pl_almacenar_cedula_peritaje 


--}pl_consultar_estado_peritaje 
PROCEDURE pl_consultar_estado_peritaje(vcresponsable  IN VARCHAR2  
                             ,vcestadoproceso      OUT VARCHAR2
                            ,nmlibconsecutivo     OUT NUMBER
                             );  


 --}pl_actualizar_documento 
PROCEDURE pl_actualizar_documento(nmLdoId       NUMBER
                                ,vcLdoEstado   VARCHAR2) ;                        

 --}pl_actualizar_documento  


 --}pl_consultar_causales 
PROCEDURE pl_consultar_causales( 
                            csconsulta           OUT csCursor
                           ,vcestadoproceso      OUT VARCHAR2
                           ,vcmensajeproceso     OUT VARCHAR2);

--pl_consultar_causales   
--pl_finalizar_proceso   
PROCEDURE pl_finalizar_proceso(
                           NMLIBCONSECUTIVO IN NUMBER,
                           VCSTDOESTADO     IN VARCHAR2,
                           VCOBSERVACION    IN VARCHAR2,
                           VCUSUARIO     IN VARCHAR2,
                           NMCAUSAL         IN NUMBER,
                           VCESTADOPROCESO  OUT VARCHAR2,
                           VCMENSAJEPROCESO OUT VARCHAR2
);   

--pl_finalizar_proceso  

--pl_seguimiento_mesa  
PROCEDURE pl_seguimiento_mesa(
                            csconsulta           OUT csCursor
                             ,vcestadoproceso      OUT VARCHAR2
                             ,vcmensajeproceso     OUT VARCHAR2
);   

--pl_seguimiento_mesa  

--pl_estadistica_analista  
PROCEDURE pl_estadistica_analista(
                             vcresponsable   in VARCHAR2
                             ,nmaprobados       OUT number
                            ,nmrechazados      OUT number
                            ,nmtotal           OUT number
                            ,vcestadoproceso      OUT VARCHAR2
                            ,vcmensajeproceso     OUT VARCHAR2
);   
--pl_estadistica_analista  

--pl_consultar_observacion  
PROCEDURE pl_consultar_observacion (
                              NMLIBCONSECUTIVO IN NUMBER
                             ,VCSTDOESTADO     IN VARCHAR2
                             ,VCRESULTADO OUT VARCHAR2
                             ,vcestadoproceso      OUT VARCHAR2
                             ,vcmensajeproceso     OUT VARCHAR2
);



--pl_consultar_observacion  


--pl_consultar_docsEnvAFirmar

  PROCEDURE pl_consultar_docsEnvAFirmar(nmlibconsecutivo IN NUMBER,
                                              csconsulta           OUT csCursor
                                             ,vcestadoproceso      OUT VARCHAR2
                                             ,vcmensajeproceso     OUT VARCHAR2);  

   --pl_consultar_docsEnvAFirmar                                             


--pl_seguimiento_firma_contrato
PROCEDURE pl_seguimiento_firma_contrato (VCTDC_TD IN VARCHAR2,
                                        NMEMP_ND IN NUMBER,
                                        NMCTO_NUMERO IN NUMBER,
                                        VCF_PREACTIVADO IN VARCHAR2,
                                        NMLIB_CONSECUTIVO IN NUMBER,
                                        VCF_FIRMADO IN VARCHAR2,
                                        AUD_USUARIO IN VARCHAR2,
                                        AUD_FECHA IN DATE,
                                        VCUSUARIO IN VARCHAR2,
                                        vcestadoproceso  OUT VARCHAR2,
                                        vcmensajeproceso   OUT VARCHAR2);

 --pl_seguimiento_firma_contrato


 PROCEDURE pl_insertar_auditoria       (VCLOG IN VARCHAR2,
                                        VCTYPE_ERROR IN VARCHAR2,
                                        VCAPLICATIVO IN VARCHAR2,
                                        VCTRANSACCION IN VARCHAR2,
                                        VCESTADOTRANSACCION IN VARCHAR2,
                                        vcestadoproceso  OUT VARCHAR2,
                                        vcmensajeproceso   OUT VARCHAR2);


PROCEDURE pl_insertar_envio_concatenado (VCURL_ENVIADA IN VARCHAR2,
                                         VCRESPUESTA IN VARCHAR2,
                                         VCAPLICATIVO IN VARCHAR2,
                                         NMLIB_CONSECUTIVO IN NUMBER,
                                         NMINTENTO IN NUMBER,
                                         VCESTADOTRANSACCION IN VARCHAR2,
                                         vcestadoproceso  OUT VARCHAR2,
                                         vcmensajeproceso   OUT VARCHAR2);                                      

 --pl_seguimiento_firma_contrato

 PROCEDURE pl_inserta_tiempo_cto (lib_consecutivo IN  NUMBER,
                                  seg_fecha       IN  VARCHAR2,
                                  vcestadoproceso  OUT VARCHAR2,
                                  vcmensajeproceso   OUT VARCHAR2);

 PROCEDURE pl_valida_tiempo_cto (nmlib_consecutivo IN  NUMBER,
                                  seg_fecha       OUT VARCHAR2,
                                  vcestadoproceso  OUT VARCHAR2,
                                  vcmensajeproceso   OUT VARCHAR2);                                  



 end  qb_JADM0067_mesa_contratos;

/


CREATE OR REPLACE PACKAGE BODY   ADM.qb_JADM0067_mesa_contratos  IS
---**********************************************************************************************************
--** NOMBRE SCRIPT        : QADM0186.sql
--** OBJETIVO             : logica Mesa de contratos
--** ESQUEMA              : ADM
--** NOMBRE               : adm.qb_JADM0067_mesa_contratos 
--** AUTOR                : DESIERRA
--** FECHA CREACION       : 12/09/2022
--**********************************************************************************************************

--{Pl_consulta_cedulas
PROCEDURE Pl_consulta_cedulas(VCSTDOESTADO         IN VARCHAR2
                             ,csconsulta           OUT csCursor
                             ,vcestadoproceso      OUT VARCHAR2
                             ,vcmensajeproceso     OUT VARCHAR2
                             ) IS

     exSalir	     EXCEPTION;
     vcquery       VARCHAR2(4000);
BEGIN


OPEN csconsulta FOR
     SELECT
    lib.tdc_td_epl,
    lib.epl_nd,
    lib.lib_consecutivo
FROM
    rhu.libroingreso lib,
    contrato         cto
WHERE
    lib.lib_consecutivo = cto.lib_consecutivo
    AND rec_codigo = 'CTO'
    AND cto.ect_sigla = 'PRE'
    AND LIB_ESTADO=VCSTDOESTADO
    order by REC_FECHA ASC;

     vcestadoproceso :='S';
     vcmensajeproceso:='Proceso terminado con exito.';
EXCEPTION
WHEN exsalir THEN
     vcestadoproceso  :='N';
WHEN OTHERS THEN
     vcestadoproceso  :='N';
     vcmensajeproceso :='Error causado en Pl_consulta_cedulas causado por'||sqlerrm;
END Pl_consulta_cedulas;
--}Pl_consulta_cedulas

--}Pl_consulta_datos
PROCEDURE Pl_consulta_datos(nmordenCTO            IN NUMBER  
                             ,csconsulta           OUT csCursor
                             ,vcestadoproceso      OUT VARCHAR2
                             ,vcmensajeproceso     OUT VARCHAR2
                             ) IS

     exSalir	     EXCEPTION;
     vcquery       VARCHAR2(4000);
BEGIN

  IF nmordenCTO IS NULL THEN
          vcmensajeproceso :='Orden de contratacion no encontrada';
          RAISE exSalir;
 END IF;
     OPEN csconsulta FOR 
      SELECT     lib.epl_nd epl_nd
                           , lib.tdc_td_epl tdc_td_epl 
                           , lib.tdc_td_fil tdc_td
                           , lib.emp_nd_fil emp_nd
                           , rhu.fb_empresa(lib.tdc_td_fil,lib.emp_nd_fil) emp_nom
                           , lib.lib_CONSECUTIVO Lib_CONSECUTIVO
                           , FB_EMPLEADO_COLUMNA (lib.TDC_TD_EPL, lib.EPL_ND ,'NOMBRE') nombre_empleado
                           , cno.cno_nombre cargo_empleado
                           , rhu.FB_CORREO_EMPLEADO(lib.TDC_TD_EPL, lib.EPL_ND) correo_empleado
                           , lib.cto_numero cto_numero
                     FROM rhu.libroingreso lib, cno cno
                     WHERE cno.cno_codigo = lib.cno_codigo
                     AND LIB.LIB_CONSECUTIVO=nmordenCTO;

     vcestadoproceso :='S';
     vcmensajeproceso:='Proceso terminado con exito.';
EXCEPTION
WHEN exsalir THEN
     vcestadoproceso  :='N';
WHEN OTHERS THEN
     vcestadoproceso  :='N';
     vcmensajeproceso :='Error causado en Pl_consulta_cedulas causado por'||sqlerrm;
END Pl_consulta_datos;
--}Pl_consulta_datos


--}pl_almacenar_cedula_peritaje
PROCEDURE pl_almacenar_cedula_peritaje(vcresponsable  IN VARCHAR2  
                            ,vcestadoResponsable IN VARCHAR2 
                            ,nmlibconsecutivo IN NUMBER 
                            ,vcestadoproceso      OUT VARCHAR2
                            ,vcmensajeproceso     OUT VARCHAR2
                             )
    IS 
     exSalir	     EXCEPTION;
     nmRegistros    NUMBER;  
     BEGIN
     IF nmlibconsecutivo IS NULL THEN
          vcmensajeproceso :='Orden de contratacion no encontrada';
          RAISE exSalir;
    END IF;

    SELECT COUNT(*) 
     into nmRegistros
     FROM seg_mesa_contratos 
     where lib_consecutivo = nmlibconsecutivo
     and smc_estado_responsable ='V';

     IF nmRegistros >0 THEN
      vcmensajeproceso :='El candidato ya se encuentra asignado , intente con otra cedula.';
          RAISE exSalir;
     END IF;

     INSERT INTO ADM.seg_mesa_contratos (
                                   SMC_ID,
                                   SMC_RESPONSABLE,
                                   SMC_ESTADO_RESPONSABLE,
                                   LIB_CONSECUTIVO,
                                   AUD_FECHA,
                                   AUD_USUARIO)
                                   VALUES(
                                   1,
                                   vcresponsable,
                                   vcestadoResponsable,
                                   nmlibconsecutivo,
                                   SYSDATE,
                                   USER
                                   );
 UPDATE rhu.libroingreso SET lib_estado       = 'RMC',
                              REC_USUARIO      = USER,
                              LIB_FECHA_ESTADO = SYSDATE
 WHERE LIB_CONSECUTIVO = nmlibconsecutivo;

 COMMIT;

     vcestadoproceso :='S';
     vcmensajeproceso:='Proceso terminado con exito.';
EXCEPTION

WHEN exsalir THEN
     vcestadoproceso  :='N';
     vcmensajeproceso :='Error causado en  pl_almacenar_cedula_peritaje causado por'||sqlerrm;

WHEN OTHERS THEN
     vcestadoproceso  :='N';
     vcmensajeproceso :='Error causado en pl_almacenar_cedula_peritaje causado por'||sqlerrm;

END pl_almacenar_cedula_peritaje;
--}pl_almacenar_cedula_peritaje

--}pl_consultar_estado_peritaje
PROCEDURE pl_consultar_estado_peritaje(vcresponsable  IN VARCHAR2  
                             ,vcestadoproceso      OUT VARCHAR2
                            ,nmlibconsecutivo     OUT NUMBER
                             )
                             IS
                             exSalir	     EXCEPTION;
                             nmConsecutivo   NUMBER;

BEGIN
    SELECT lib_consecutivo
     into nmConsecutivo
     FROM seg_mesa_contratos 
     where smc_responsable = vcresponsable
     and smc_estado_responsable ='V';

     IF nmConsecutivo IS NULL THEN
     nmlibconsecutivo:=0;
     END IF;


  vcestadoproceso :='V';
  nmlibconsecutivo:=nmConsecutivo;

EXCEPTION

WHEN exsalir THEN
     vcestadoproceso  :='N';

WHEN OTHERS THEN
     vcestadoproceso :='Error causado en pl_consultar_estado_peritaje causado por'||sqlerrm;

END pl_consultar_estado_peritaje;
--}pl_consultar_estado_peritaje

 --}pl_actualizar_documento 
 PROCEDURE pl_actualizar_documento(nmLdoId       NUMBER
                                ,vcLdoEstado   VARCHAR2)                           
 IS
  BEGIN
     UPDATE rhu.libroIngreso_documento
        SET LDO_ESTADO_MESA = vcLdoEstado
      WHERE ldo_id = nmLdoId;
  EXCEPTION
  WHEN OTHERS THEN
     raise_application_error(-20001, 
     'ERROR no controlado en ADM.QB_APP_JADM0065_LOGIC.PL_ACTUALIZAR_DOCUMENTO, causada por: '||SQLERRM||' -- Linea: ' || dbms_utility.format_error_backtrace());
  END pl_actualizar_documento;

  --}pl_actualizar_documento 



  --}pl_consultar_causales 
  PROCEDURE pl_consultar_causales( 
                            csconsulta           OUT csCursor
                           ,vcestadoproceso      OUT VARCHAR2
                           ,vcmensajeproceso     OUT VARCHAR2)
                           IS
                           exSalir	     EXCEPTION;
    BEGIN
    OPEN csconsulta FOR 
    SELECT  cal.codigo_causal,
        UPPER(cal.descripcion_causal)descripcion_causal ,
        ecl.ecl_secuencia
    FROM PAR.causal_lingreso cal ,  
        estado_causal_libingreso ecl
    WHERE cal.codigo_causal = ecl.codigo_causal 
    AND ecl.lib_estado = 'RMC';

   vcestadoproceso :='S';
   vcmensajeproceso:='Proceso terminado con exito.';

   EXCEPTION

   WHEN OTHERS THEN
     vcestadoproceso  :='N';
     vcmensajeproceso :='Error causado en pl_almacenar_cedula_peritaje causado por'||sqlerrm;

  END pl_consultar_causales;
    --}pl_consultar_causales 

    --}pl_finalizar_proceso 
  PROCEDURE pl_finalizar_proceso(
                           NMLIBCONSECUTIVO IN NUMBER,
                           VCSTDOESTADO     IN VARCHAR2,
                           VCOBSERVACION    IN VARCHAR2,
                           VCUSUARIO     IN VARCHAR2,
                           NMCAUSAL         IN NUMBER,
                           VCESTADOPROCESO  OUT VARCHAR2,
                           VCMENSAJEPROCESO OUT VARCHAR2)
                           IS
     vcTitulo          VARCHAR2(4000);  
     vcTexto           VARCHAR2(4000);  
     vcnombre          VARCHAR2(4000);  
     vctdc_td_epl      VARCHAR2(4000);  
     nmepl_nd          NUMBER;
     vctdc_td_fil      VARCHAR2(4000);  
     nmemp_nd_fil      NUMBER;
     nmacuerdocliente  NUMBER;
     vcusuarionotilider   VARCHAR2(4000); 
     vcusuarionotiaux    VARCHAR2(4000); 
     vcincfechanue DATE ;
     EXSALIR  EXCEPTION;

   BEGIN

   --Cambiamos el estado del libro.
   IF NMLIBCONSECUTIVO IS NOT NULL THEN
   UPDATE rhu.libroingreso SET lib_estado      = VCSTDOESTADO,
                              REC_USUARIO      = VCUSUARIO,
                              LIB_FECHA_ESTADO = SYSDATE
   WHERE LIB_CONSECUTIVO = NMLIBCONSECUTIVO;
      COMMIT;
   END IF;

   -- Insertamos la observacion final del proceso.
   IF VCSTDOESTADO IS NOT NULL THEN
   INSERT INTO RHU.observacion_lingreso
          (LIB_CONSECUTIVO, LIB_ESTADO, OBS_DESCRIPCION, OBS_FECHA, OBS_USUARIO, CODIGO_CAUSAL)
   VALUES (NMLIBCONSECUTIVO,VCSTDOESTADO,VCOBSERVACION,SYSDATE,VCUSUARIO,NMCAUSAL);
      COMMIT;
   END IF;

   -- Actualizamos el estado del Analista de mesa de contratos.
   IF VCUSUARIO IS NOT NULL THEN
   ADM.pb_seguimiento('Usuario' ,VCUSUARIO );
   UPDATE ADM.SEG_MESA_CONTRATOS
   SET SMC_ESTADO_RESPONSABLE = 'F',
   est_final=VCSTDOESTADO,
   fecha_final=SYSDATE 
   WHERE SMC_RESPONSABLE = VCUSUARIO 
   AND LIB_CONSECUTIVO = NMLIBCONSECUTIVO;
   COMMIT;
   END IF;

    --Traemos la informacion del candidato para enviar la notificacion
     BEGIN
              SELECT lib.TDC_TD_EPL, lib.EPL_ND,lib.TDC_TD_FIL, lib.EMP_ND_FIL , emp.emp_acu_codigo
              INTO vctdc_td_epl, nmepl_nd, vctdc_td_fil, nmemp_nd_fil , nmacuerdocliente
              FROM rhu.libroingreso lib , ACR.empresa_acuerdo emp
              WHERE lib_consecutivo = NMLIBCONSECUTIVO
              and emp.emp_nd_fil =  lib.emp_nd_fil
              and emp.tdc_td_fil = lib.tdc_td_fil;

          EXCEPTION
          WHEN OTHERS THEN
               VCESTADOPROCESO := 'S';
               VCMENSAJEPROCESO := 'Registro actualizado correctamente, No se pudo enviar notificacion Automatica';
               RETURN;
          END;

    --Se construye la notificacion
      vcnombre     := Fb_Epl_nombres(vctdc_td_epl, nmepl_nd);
      vcTitulo     := 'Candidato pasado a '||VCSTDOESTADO;
      vcTexto      := 'El candidato '||vcnombre||' con documento de identidad '||vctdc_td_epl||'-'||nmepl_nd ||'ha finalizado su proceso de peritaje mesa de contratos.';

    EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_DATE_FORMAT = ''DD-MONTH-YYYY HH24:MI:SS''';

    -- se consulta la informacion del auxiliar de contratos que paso al candidato a un estado EMC
    BEGIN
    SELECT  MAX(inc_fecha_nue) , inc_usuario_nue
    INTO vcincfechanue , vcusuarionotiaux
    FROM aud_estado_li 
    WHERE lib_consecutivo = NMLIBCONSECUTIVO 
    and (EPL_ND=nmepl_nd) 
    AND inc_estado_nue = 'EMC' 
    AND inc_fecha_nue IS NOT NULL
    GROUP BY (inc_usuario_nue);
    END;

   --ADM.pb_seguimiento_long('USU_AUX' , vcusuarionotiaux);


    -- Notificacion al auxiliar de contratos.
    adm.qb_JADM0053_POP_UP.pl_ins_burbuja(vcusuarionotiaux    
                                            ,vcTitulo      
                                            ,vcTexto
                                            ,'INFO'
                                            ,VCUSUARIO
                                            ,'MESA DE CONTRATOS'
                                            ,VCESTADOPROCESO  
                                            ,VCMENSAJEPROCESO);  

    -- Buscamos al responsable interno registrado en los acuerdos de la empresa.
       SELECT REPLACE(REGEXP_SUBSTR(FB_USUARIO_USUUSUARIO(tdc_td,epl_nd,'USU_USUARIO',''),'[^-]+'),' ' ,'') USU_USUARIO
       INTO vcusuarionotilider
       FROM ACR.responsable_interno 
       WHERE tca_codigo = 5  -- ID DE CONTRATOS
       AND emp_acu_codigo=nmacuerdocliente;

        --ADM.pb_seguimiento_long('vcusuarionotilider' , vcusuarionotilider);
        -- Notificacion al responsable interno.
       adm.qb_JADM0053_POP_UP.pl_ins_burbuja(vcusuarionotilider    
                                            ,vcTitulo      
                                            ,vcTexto
                                            ,'INFO'
                                            ,VCUSUARIO
                                            ,'MESA DE CONTRATOS'
                                            ,VCESTADOPROCESO  
                                            ,VCMENSAJEPROCESO);  
    vcestadoproceso :='S';
    vcmensajeproceso:='Proceso terminado con exito. Mesa Contratos';
    ADM.PB_SEGUIMIENTO_LONG('estado',vcestadoproceso);
    EXCEPTION

    WHEN OTHERS THEN
    vcestadoproceso  :='N';
    vcmensajeproceso :='Error causado en pl_finalizar_proceso causado por'||sqlerrm;
       ADM.PB_SEGUIMIENTO_LONG('estado',vcestadoproceso || vcmensajeproceso );


  END pl_finalizar_proceso;
   --}pl_finalizar_proceso 

     --}pl_seguimiento_mesa 
   PROCEDURE pl_seguimiento_mesa(
                             csconsulta           OUT csCursor
                             ,vcestadoproceso      OUT VARCHAR2
                             ,vcmensajeproceso     OUT VARCHAR2)
                             IS

 BEGIN 

 OPEN csconsulta FOR
 SELECT
  MAX(aud.inc_fecha_nue) fecha ,
  aud.inc_usuario_nue,
  cto.epl_nd,
  rhu.fb_empresa(lib.tdc_td_fil,lib.emp_nd_fil) empresa,
  cto.tdc_td_epl,
  lib.lib_consecutivo,
  adm.FB_CAL_TIEM_FECHAS(lib.lib_fecha_estado,sysdate) TIEMPO_TRANCURRIDO
FROM
    rhu.libroingreso lib,
    contrato         cto,
    aud_estado_li    aud
WHERE
    lib.lib_consecutivo = cto.lib_consecutivo
    and lib.lib_consecutivo = aud.lib_consecutivo
    and aud.inc_estado_nue = 'EMC' 
    AND inc_fecha_nue IS NOT NULL
    AND lib.rec_codigo = 'CTO'
    AND cto.ect_sigla = 'PRE'
    AND lib.LIB_ESTADO='EMC'
    GROUP BY (aud.inc_usuario_nue,cto.epl_nd,
    rhu.fb_empresa(lib.tdc_td_fil,lib.emp_nd_fil),
    cto.tdc_td_epl,lib.lib_consecutivo, 
    adm.FB_CAL_TIEM_FECHAS(lib.lib_fecha_estado,sysdate))
    ORDER BY fecha ASC;

    vcestadoproceso :='S';
    vcmensajeproceso:='Proceso terminado con exito. Mesa Contratos';

    EXCEPTION

    WHEN OTHERS THEN
    vcestadoproceso  :='N';
    vcmensajeproceso :='Error causado en pl_seguimiento_mesa causado por'||sqlerrm;

  END pl_seguimiento_mesa;
     --}pl_seguimiento_mesa 

          --}pl_estadistica_analista 
  PROCEDURE pl_estadistica_analista(
                             vcresponsable   in VARCHAR2
                              ,nmaprobados       OUT number
                            ,nmrechazados      OUT number
                            ,nmtotal           OUT number
                             ,vcestadoproceso      OUT VARCHAR2
                             ,vcmensajeproceso     OUT VARCHAR2
  )IS

  BEGIN
   SELECT ap.APROBADOS , an.RECHAZADOS , tot.TOTAL
      INTO nmaprobados , nmrechazados,nmtotal
      FROM
     (SELECT COUNT(est_final) APROBADOS
     FROM seg_mesa_contratos
     WHERE est_final ='PFC'
     AND  smc_responsable=vcresponsable) AP ,

    (SELECT COUNT(est_final) RECHAZADOS
     FROM seg_mesa_contratos
     WHERE est_final ='NAM'
     AND  smc_responsable=vcresponsable) AN,

    (SELECT
    count(lib.lib_consecutivo)  TOTAL
    FROM
    rhu.libroingreso lib,
    contrato         cto
    WHERE
    lib.lib_consecutivo = cto.lib_consecutivo
    AND rec_codigo = 'CTO'
    AND cto.ect_sigla = 'PRE'
    AND LIB_ESTADO='EMC'
    order by REC_FECHA desc) TOT;

    vcestadoproceso :='S';
    vcmensajeproceso:='Proceso terminado con exito. Mesa Contratos';

    EXCEPTION
    WHEN OTHERS THEN
    vcestadoproceso  :='N';
    vcmensajeproceso :='Error causado en pl_seguimiento_mesa causado por'||sqlerrm;

  END pl_estadistica_analista;
--}pl_estadistica_analista 


--}pl_consultar_observacion 
PROCEDURE pl_consultar_observacion (
                              NMLIBCONSECUTIVO IN NUMBER
                             ,VCSTDOESTADO     IN VARCHAR2
                             ,VCRESULTADO OUT VARCHAR2
                             ,vcestadoproceso      OUT VARCHAR2
                             ,vcmensajeproceso     OUT VARCHAR2
)
IS 
  EXSALIR  EXCEPTION;
  VCOBSERVACION observacion_lingreso.obs_descripcion%TYPE;
  VCFECHA  observacion_lingreso.obs_fecha%TYPE;
  VCUSUARIO observacion_lingreso.obs_usuario%TYPE;
  BEGIN
  IF NMLIBCONSECUTIVO IS NULL THEN
  RAISE EXSALIR;
  END IF;

    EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_DATE_FORMAT = ''DD-MONTH-YYYY HH24:MI:SS''';
   SELECT   MAX(obs_fecha) Fecha , obs_descripcion , obs_usuario
                       INTO VCFECHA,VCOBSERVACION,VCUSUARIO
                       FROM observacion_lingreso 
                       WHERE lib_estado='EMC' -- puede ser reemplazado por VCSTDOESTADO
                       AND lib_consecutivo =NMLIBCONSECUTIVO
                       GROUP BY (obs_descripcion,obs_fecha,obs_usuario )
                       ORDER BY obs_fecha DESC 
                       FETCH FIRST ROW ONLY;

                       VCRESULTADO := VCFECHA ||chr(13)
                                               || VCOBSERVACION || chr(13)
                                               || VCUSUARIO;
                       vcestadoproceso :='S';
                       vcmensajeproceso:='Proceso terminado con exito. Mesa Contratos';

  EXCEPTION
  WHEN EXSALIR THEN
  vcestadoproceso := 'N';
  vcmensajeproceso := 'El numero de libro esta vacio';
  WHEN OTHERS THEN
  vcestadoproceso :='N';
  vcmensajeproceso := 'Error al obtener la observacion para pl_consultar_observacion debido a : ' || sqlerrm;
  END pl_consultar_observacion;

--pl_consultar_observacion
 PROCEDURE pl_consultar_docsEnvAFirmar(nmlibconsecutivo IN NUMBER,
                                              csconsulta           OUT csCursor
                                             ,vcestadoproceso      OUT VARCHAR2
                                             ,vcmensajeproceso     OUT VARCHAR2)IS

         BEGIN
         OPEN csconsulta FOR
        SELECT tpd.tpd_descripcion descripcion,
       (SELECT b.azd_codigo_cli FROM ADM.data_erp_az a , ADM.az_digital b WHERE a.azd_codigo = b.azd_codigo
        and a.dea_codigo = LID.dea_codigo ) AS AZD_CODIGO_CLI
        FROM libroIngreso_documento lid , ADM.tipo_documento tpd 
        WHERE lid.tpd_codigo = tpd.tpd_codigo
        AND lid.lib_consecutivo = nmlibconsecutivo
         AND lid.LDO_FIRMA_DOCUMENTO = 'ACEPTADO';

   vcestadoproceso :='S';
   vcmensajeproceso:='Proceso terminado con exito.';

   EXCEPTION

   WHEN OTHERS THEN
     vcestadoproceso  :='N';
     vcmensajeproceso :='Error causado en pl_consultar_docsEnvAFirmar causado por'||sqlerrm;

END pl_consultar_docsEnvAFirmar;


PROCEDURE pl_seguimiento_firma_contrato(VCTDC_TD IN VARCHAR2,
                                        NMEMP_ND IN NUMBER,
                                        NMCTO_NUMERO IN NUMBER,
                                        VCF_PREACTIVADO IN VARCHAR2,
                                        NMLIB_CONSECUTIVO IN NUMBER,
                                        VCF_FIRMADO IN VARCHAR2,
                                        AUD_USUARIO IN VARCHAR2,
                                        AUD_FECHA IN DATE,
                                        VCUSUARIO IN VARCHAR2,
                                        vcestadoproceso  OUT VARCHAR2,
                                        vcmensajeproceso   OUT VARCHAR2)
                                        IS
     vcTitulo          VARCHAR2(4000);  
     vcTexto           VARCHAR2(4000);  
     vcnombre          VARCHAR2(4000);  
     vctdc_td_epl      VARCHAR2(4000);  
     nmepl_nd          NUMBER;
     vctdc_td_fil      VARCHAR2(4000);  
     nmemp_nd_fil      NUMBER;
     nmacuerdocliente  NUMBER;
     vcusuarionotilider   VARCHAR2(4000); 
     vcusuarionotiaux    VARCHAR2(4000); 
     vcincfechanue DATE ;
     nmrequisicion requisicion.req_consecutivo%type;
     vccoordinador  requisicion.req_coordinador%type;
     EXSALIR  EXCEPTION;
BEGIN
--Se inserta el seguimiento del contrato en la tabla RHU.VALIDA_CTO_FIRMA
INSERT INTO RHU.VALIDA_CTO_FIRMA (VCF_CODIGO,
                                  TDC_TD,
                                  EMP_ND,
                                  CTO_NUMERO,
                                  VCF_PREACTIVADO,
                                  VCF_FIRMADO,
                                  AUD_USUARIO,
                                  AUD_FECHA) 
                          VALUES (
                                  NMCTO_NUMERO,
                                  VCTDC_TD,
                                  NMEMP_ND,
                                  NMCTO_NUMERO,
                                  'S',
                                  'N',
                                  USER,
                                  SYSDATE
                                  );
         COMMIT;                         
-- Se inserta 

    --Traemos la informacion del candidato para enviar la notificacion
     BEGIN
              SELECT lib.TDC_TD_EPL, lib.EPL_ND,lib.TDC_TD_FIL, lib.EMP_ND_FIL , emp.emp_acu_codigo
              INTO vctdc_td_epl, nmepl_nd, vctdc_td_fil, nmemp_nd_fil , nmacuerdocliente
              FROM rhu.libroingreso lib , ACR.empresa_acuerdo emp
              WHERE lib_consecutivo = NMLIB_CONSECUTIVO
              and emp.emp_nd_fil =  lib.emp_nd_fil
              and emp.tdc_td_fil = lib.tdc_td_fil;

          EXCEPTION
          WHEN OTHERS THEN
               VCESTADOPROCESO := 'S';
               VCMENSAJEPROCESO := 'Registro actualizado correctamente, No se pudo enviar notificacion Automatica';
               RETURN;
          END;
             --Se construye la notificacion
      vcnombre     := Fb_Epl_nombres(vctdc_td_epl, nmepl_nd);
      vcTitulo     := 'Candidato pasado a  PFC';
      vcTexto      := 'El candidato '||vcnombre||' con documento de identidad '||vctdc_td_epl||'-'||nmepl_nd ||'se envia a firmar contrato,metodo de envio firma digital.';

    EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_DATE_FORMAT = ''DD-MONTH-YYYY HH24:MI:SS''';

    -- se consulta la informacion del auxiliar de contratos que paso al candidato a un estado EMC
    BEGIN
    SELECT  MAX(inc_fecha_nue) , inc_usuario_nue
    INTO vcincfechanue , vcusuarionotiaux
    FROM aud_estado_li 
    WHERE lib_consecutivo = NMLIB_CONSECUTIVO 
    and (EPL_ND=nmepl_nd) 
    AND inc_estado_nue = 'EMC' 
    AND inc_fecha_nue IS NOT NULL
    GROUP BY (inc_usuario_nue);
    END;

    ADM.pb_seguimiento('USU_AUX' , vcusuarionotiaux);
    -- Notificacion al auxiliar de contratos.
    adm.qb_JADM0053_POP_UP.pl_ins_burbuja(vcusuarionotiaux    
                                            ,vcTitulo      
                                            ,vcTexto
                                            ,'INFO'
                                            ,VCUSUARIO
                                            ,'FIRMA DE CONTRATO'
                                            ,VCESTADOPROCESO  
                                            ,VCMENSAJEPROCESO);  
                                             -- Buscamos al responsable interno registrado en los acuerdos de la empresa.
       SELECT REPLACE(REGEXP_SUBSTR(FB_USUARIO_USUUSUARIO(tdc_td,epl_nd,'USU_USUARIO',''),'[^-]+'),' ' ,'') USU_USUARIO
       INTO vcusuarionotilider
       FROM ACR.responsable_interno 
       WHERE tca_codigo = 5  -- ID DE CONTRATOS
       AND emp_acu_codigo=nmacuerdocliente;

        ADM.pb_seguimiento('vcusuarionotilider' , vcusuarionotilider);
        -- Notificacion al responsable interno.
       adm.qb_JADM0053_POP_UP.pl_ins_burbuja(vcusuarionotilider    
                                            ,vcTitulo      
                                            ,vcTexto
                                            ,'INFO'
                                            ,VCUSUARIO
                                           ,'FIRMA DE CONTRATO'
                                            ,VCESTADOPROCESO  
                                            ,VCMENSAJEPROCESO);  

     --Validamos si el libro cuenta con requisicion para realizar la notificacion al responsable .
    select lib.nro_requisicion , req.req_coordinador
    into nmrequisicion,vccoordinador
    from rhu.libroingreso lib , par.requisicion req
    where req.req_consecutivo = lib.nro_requisicion
    and lib_consecutivo = NMLIB_CONSECUTIVO;     

    IF nmrequisicion IS NOT NULL THEN
      adm.qb_JADM0053_POP_UP.pl_ins_burbuja(vcusuarionotilider    
                                            ,vcTitulo      
                                            ,vcTexto
                                            ,'INFO'
                                            ,vccoordinador
                                            ,'FIRMA DE CONTRATO'
                                            ,VCESTADOPROCESO  
                                            ,VCMENSAJEPROCESO);  
    END IF;

    vcestadoproceso :='S';
    vcmensajeproceso:='Proceso terminado con exito. Mesa Contratos';

    EXCEPTION

    WHEN OTHERS THEN
    vcestadoproceso  :='N';
    vcmensajeproceso :='Error causado en pl_seguimiento_firma_contrato causado por'||sqlerrm;


END pl_seguimiento_firma_contrato;


PROCEDURE pl_insertar_auditoria       (VCLOG IN VARCHAR2,
                                        VCTYPE_ERROR IN VARCHAR2,
                                        VCAPLICATIVO IN VARCHAR2,
                                        VCTRANSACCION IN VARCHAR2,
                                        VCESTADOTRANSACCION IN VARCHAR2,
                                        vcestadoproceso  OUT VARCHAR2,
                                        vcmensajeproceso   OUT VARCHAR2)
                                        IS
 BEGIN

    INSERT INTO ADM.AUDITORIA_LOG_APP (
                                   ALP_ID,
                                   ALP_LOG,
                                   ALP_TYPE_ERROR,
                                   ALP_APLICATIVO,
                                   ALP_TRANSACCION,
                                   ALP_ESTADOTRANSACCION,
                                   AUD_FECHA,
                                   AUD_USUARIO)
                                   VALUES(
                                   10,
                                   VCLOG,
                                   VCTYPE_ERROR,
                                   VCAPLICATIVO,
                                   VCTRANSACCION,
                                   VCESTADOTRANSACCION,
                                   SYSDATE,
                                   USER
                                   );
  COMMIT;
    vcestadoproceso :='S';
    vcmensajeproceso:='Auditoria Almacenada.';

    EXCEPTION

    WHEN OTHERS THEN
    vcestadoproceso  :='N';
    vcmensajeproceso :='Error causado en pl_insertar_auditoria causado por'||sqlerrm;


END pl_insertar_auditoria;


PROCEDURE pl_insertar_envio_concatenado (VCURL_ENVIADA IN VARCHAR2,
                                         VCRESPUESTA IN VARCHAR2,
                                         VCAPLICATIVO IN VARCHAR2,
                                         NMLIB_CONSECUTIVO IN NUMBER,
                                         NMINTENTO IN NUMBER,
                                         VCESTADOTRANSACCION IN VARCHAR2,
                                         vcestadoproceso  OUT VARCHAR2,
                                         vcmensajeproceso   OUT VARCHAR2)IS
 NMREGISTROS NUMBER;
 VCFECHA   VARCHAR2(4000);
 VCORIGEN   VARCHAR2(4000);
 BEGIN

    SELECT COUNT(*) INTO NMREGISTROS FROM ADM.SEG_FIRMA_CTO_CONCATENADO WHERE lib_consecutivo = NMLIB_CONSECUTIVO;
    IF NMREGISTROS >0 THEN
    UPDATE ADM.SEG_FIRMA_CTO_CONCATENADO 
    SET scc_url_enviada = VCURL_ENVIADA , 
    SCC_RESPUESTA =VCRESPUESTA ,
    SCC_APLICATIVO=VCAPLICATIVO,
    SCC_INTENTO=NMINTENTO
    WHERE lib_consecutivo = NMLIB_CONSECUTIVO;
    ELSE
    INSERT INTO ADM.SEG_FIRMA_CTO_CONCATENADO (
                                   SCC_ID,
                                   SCC_URL_ENVIADA,
                                   SCC_RESPUESTA,
                                   LIB_CONSECUTIVO,
                                   SCC_INTENTO,
                                   SCC_TIPO,
                                   AUD_FECHA,
                                   AUD_USUARIO,
                                   SCC_APLICATIVO)
                                   VALUES(
                                   DEFAULT,
                                   VCURL_ENVIADA,
                                   VCRESPUESTA,
                                   NMLIB_CONSECUTIVO,
                                   NMINTENTO,
                                   VCESTADOTRANSACCION,
                                   SYSDATE,
                                   USER,
                                   VCAPLICATIVO
                                   );

   END IF;
     COMMIT;
    vcestadoproceso :='S';
    vcmensajeproceso:='Auditoria Almacenada.';

    EXCEPTION

    WHEN OTHERS THEN
    vcestadoproceso  :='N';
    vcmensajeproceso :='Error causado en pl_insertar_envio_concatenado causado por'||sqlerrm;


END pl_insertar_envio_concatenado;




 PROCEDURE pl_inserta_tiempo_cto (lib_consecutivo IN  NUMBER,
                                  seg_fecha       IN  VARCHAR2,
                                  vcestadoproceso  OUT VARCHAR2,
                                  vcmensajeproceso   OUT VARCHAR2)IS

 BEGIN

 INSERT INTO rhu.seg_firma_cto_mesa (seg_id,
                                    lib_consecutivo   ,
                                    seg_fecha         ,
                                    seg_firmado       ,
                                    seg_estado        ,    
                                    aud_fecha         ,
                                    aud_usuario       )
VALUES (DEFAULT, lib_consecutivo, seg_fecha, DEFAULT, 'INF', SYSDATE, USER);

COMMIT;
    vcestadoproceso :='S';
    vcmensajeproceso:='Tiempo Almacenado.';

 EXCEPTION

    WHEN OTHERS THEN
    vcestadoproceso  :='N';
    vcmensajeproceso :='Error causado en pl_insertar_auditoria causado por'||sqlerrm;

END pl_inserta_tiempo_cto; 



 PROCEDURE pl_valida_tiempo_cto ( nmlib_consecutivo IN  NUMBER,
                                  seg_fecha       OUT VARCHAR2,
                                  vcestadoproceso  OUT VARCHAR2,
                                  vcmensajeproceso   OUT VARCHAR2)IS

     MBLIB_CONSECUTIVO      VARCHAR2(4000);  
     VCSEG_FECHA          NUMBER;                                 

 BEGIN

PB_SEGUIMIENTO_LONG('pl_valida_tiempo_cto','lib_consecutivo:_'||nmlib_consecutivo);

 select COUNT(LIB_CONSECUTIVO),SEG_FECHA INTO MBLIB_CONSECUTIVO, VCSEG_FECHA 
 from rhu.seg_firma_cto_mesa
 where LIB_CONSECUTIVO=nmlib_consecutivo
 AND SEG_ESTADO='INF' group by SEG_FECHA,LIB_CONSECUTIVO ;


 IF(MBLIB_CONSECUTIVO>0)THEN
    seg_fecha       :=VCSEG_FECHA;

 ELSE 
    seg_fecha       :='0';

 END IF;
    vcestadoproceso :='S';
    vcmensajeproceso:='Tiempo Almacenado.';
 NULL;

 EXCEPTION

    WHEN OTHERS THEN
    vcestadoproceso  :='N';
    vcmensajeproceso :='Error causado en pl_insertar_auditoria causado por'||sqlerrm;

END pl_valida_tiempo_cto;  


end qb_JADM0067_mesa_contratos;
/
