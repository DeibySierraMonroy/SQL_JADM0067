create or replace PACKAGE      ACR.QB_ACUERDO_EMPRESA IS --**********************************************************************************************************
--** NOMBRE SCRIPT        : QACR0007.SQL
--** OBJETIVO             : LOGICA ACUERDO EMPRESA POR AREA ORGANIZACIONAL.
--** ESQUEMA              : ACR
--** NOMBRE               : ACR.QB_ACUERDO_EMPRESA
--** AUTOR                : DESIERRA
--** FECHA CREACION       : 19/09/2022
--**********************************************************************************************************
TYPE CSCURSOR IS REF CURSOR;

--{PL_CONSULTAR_EMPRESA
PROCEDURE PL_CONSULTAR_EMPRESA(
    VCTDC_TD IN VARCHAR2,
    VCEMP_ND IN VARCHAR2,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);

--{PL_CONSULTAR_EMPRESA

--{PL_CONSULTAR_REPONSABLES
PROCEDURE PL_CONSULTAR_REPONSABLES(
    NMEMP_ACU_CODIGO IN NUMBER,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);
--{PL_CONSULTAR_REPONSABLES

--{PL_CONSULTAR_SUCURSAL_EMPRESA
PROCEDURE PL_CONSULTAR_SUCURSAL_EMPRESA(
   NMEMP_ACU_CODIGO IN NUMBER,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);

--{PL_CONSULTAR_SUCURSAL_EMPRESA

--{PL_CONSULTAR_CONTACTO_SUCURSAL
PROCEDURE PL_CONSULTAR_CONTACTO_SUCURSAL(
    NMSUE_CODIGO IN NUMBER,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);

--{PL_CONSULTAR_CONTACTO_SUCURSAL

--{PL_CONSULTAR_EMPRESA_PRINCIPAL
PROCEDURE PL_CONSULTAR_EMPRESA_PRINCIPAL(
    NMEMP_ACU_CODIGO IN VARCHAR2,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);

--{PL_CONSULTAR_AGRUPACIONES
PROCEDURE PL_CONSULTAR_AGRUPACIONES(
    NMEMP_ACU_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);
--{PL_CONSULTAR_AGRUPACIONES

--{PL_CONSULTAR_ACUERDO_POR_AGRUPACION
PROCEDURE PL_CONSULTAR_ACUERDO_POR_AGRUP(
    NMEMP_ACU_CODIGO IN NUMBER ,
    NMAGA_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);
--{PL_CONSULTAR_ACUERDO_POR_AGRUPACION

--{PL_CONSULTAR_VALOR_ATRIBUTO
PROCEDURE PL_CONSULTAR_VALOR_ATRIBUTO(
    NMACC_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);


--{PL_CONSULTAR_VALOR_ATRIBUTO

 --{PL_CONSULTAR_OBSERVACIONES_GEN
PROCEDURE PL_CONSULTAR_OBSERVACIONES_GEN(
    NMEMP_ACU_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);

 --{PL_CONSULTAR_OBSERVACIONES_GEN

--{PL_CONSULTAR_NOTA_AGRUPACION
PROCEDURE PL_CONSULTAR_NOTA_AGRUPACION(
    NMAGA_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);
--{PL_CONSULTAR_NOTA_AGRUPACION

 --{PL_CONSULTAR_RESPONSABLES_EMP
 PROCEDURE PL_CONSULTAR_RESPONSABLES_EMP(
    NMEMP_ACU_CODIGO IN NUMBER ,
    NMAOR_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
    );
 --{PL_CONSULTAR_RESPONSABLES_EMP


--{PL_CONSULTAR_CONTACTO_RESPONSABLES
PROCEDURE PL_CONSULTAR_CONTACTO_RESPONSA(
    NMRCL_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
);
--{PL_CONSULTAR_CONTACTO_RESPONSABLES

END QB_ACUERDO_EMPRESA;
/
create or replace PACKAGE BODY        ACR.QB_ACUERDO_EMPRESA IS 
--**********************************************************************************************************
--** NOMBRE SCRIPT        : QACR0007.SQL
--** OBJETIVO             : LOGICA ACUERDO EMPRESA POR AREA ORGANIZACIONAL.
--** ESQUEMA              : ACR
--** NOMBRE               : ACR.QB_ACUERDO_EMPRESA
--** AUTOR                : DESIERRA
--** FECHA CREACION       : 19/09/2022
--**********************************************************************************************************

--}PL_CONSULTAR_EMPRESA
PROCEDURE PL_CONSULTAR_EMPRESA(
    VCTDC_TD IN VARCHAR2,
    VCEMP_ND IN VARCHAR2,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
) IS EXSALIR EXCEPTION;

BEGIN 
IF  VCEMP_ND IS NULL 
  THEN 
   RAISE EXSALIR;
    VCMENSAJEPROCESO := 'EL PARAMETRO DE ENTRADA' || VCEMP_ND || 'ESTA VACIO';
END IF;

IF  VCTDC_TD IS NULL 
  THEN 
   RAISE EXSALIR;
    VCMENSAJEPROCESO := 'EL PARAMETRO DE ENTRADA' || VCTDC_TD || 'ESTA VACIO';
END IF;

OPEN CSCONSULTA FOR
SELECT
    EMP_FECHA_CREACION_CLIENTE,
    EMP_ACU_CODIGO,
    TDC_TD_FIL,
    EMP_ND_FIL,
    EMP_DV,
    EMP_RAZON_SOCIAL,
    EMP_CIUDAD,
    EMP_TELEFONOS,
    EMP_CELULAR,
    AEM_CODIGO,
    EMP_DIRECCION_PRINCIPAL,
    EMP_EMAIL,
    EMP_TIPO_OPERACION,
    EMP_MEDIO_CONSULTA,
    EMP_AGRUPA_EMPRESARIAL,
    EMP_COD_AGRUPA_EMPRESARIAL,
    EMP_CODIGO_CIU,
    EMP_ACTIVIDAD_ECONOMICA,
    EMP_REPRESENTANTE_LEGAL,
    EMP_DOC_REPRESENTANTE_LEGAL,
    EMP_AUTORIDAD_RRHH,
    EMP_CARGO_AUTORIDAD_RRHH,
    EMP_CLI_SOLICITA_CELULAR,
    EMP_ENTREGA_PROFESIONGRAMA,
    EMP_CIU_PRESENCIA_FISICA,
    EMP_MANEJO_IND_ESPECIALES,
    EMP_CLI_PER_FUN_ARRA_OPERA,
    EMP_CLI_MOD_COMP_ORGA,
    EMP_CLI_ENT_COMP_ORG_INI_OPER,  
    EMP_CLI_COMP_GEN_CAR,
    EMP_APLIC_PARAM_OPERACION,
    AUD_FECHA,
    AUD_USUARIO,
    COD_POSTAL,
    NUM_MATRICULA_MERCANTIL
FROM
    EMPRESA_ACUERDO
WHERE
    TDC_TD_FIL = VCTDC_TD
AND 
    EMP_ND_FIL = VCEMP_ND;

 VCESTADOPROCESO :='S';
     VCMENSAJEPROCESO:='Proceso terminado con exito.';


EXCEPTION

WHEN EXSALIR THEN 
VCESTADOPROCESO := 'N';

WHEN OTHERS THEN
VCESTADOPROCESO := 'N';
VCMENSAJEPROCESO :='ERROR AL CONSULTAR LA EMPRESA EN PL_CONSULTAR_EMPRESA CAUSADO POR'||sqlerrm;

END PL_CONSULTAR_EMPRESA;
--}PL_CONSULTAR_EMPRESA

--}PL_CONSULTAR_REPONSABLES
PROCEDURE PL_CONSULTAR_REPONSABLES(
    NMEMP_ACU_CODIGO IN NUMBER,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
)
IS
EXSALIR EXCEPTION;
BEGIN

IF NMEMP_ACU_CODIGO IS  NULL THEN
   RAISE EXSALIR;
   VCMENSAJEPROCESO := 'EL PARAMETRO DE ENTRADA' || NMEMP_ACU_CODIGO ||  'ESTA VACIO';
END IF;

OPEN CSCONSULTA FOR
SELECT 
RIN_CODIGO,
EMP_ACU_CODIGO,
TCA_CODIGO,ORDEN,
RIN_NOMBRE,
RIN_MAIL,
RIN_TELEFONO,
AUD_FECHA,
AUD_USUARIO,
tca_descripcion
FROM (select R.*, T.ORDEN , t.tca_descripcion from RESPONSABLE_INTERNO R, TIPO_CARGO T
WHERE R.TCA_CODIGO = T.TCA_CODIGO) 
WHERE EMP_ACU_CODIGO=NMEMP_ACU_CODIGO
order by ORDEN;

 VCESTADOPROCESO :='S';
 VCMENSAJEPROCESO:='Proceso terminado con exito.';

EXCEPTION

WHEN EXSALIR THEN 
VCESTADOPROCESO :='N';

WHEN OTHERS THEN
VCESTADOPROCESO := 'N';
VCMENSAJEPROCESO :='ERROR AL CONSULTAR LOS RESPONSABLES EN PL_CONSULTAR_REPONSABLES CAUSADO POR'||sqlerrm;


END PL_CONSULTAR_REPONSABLES;
--}PL_CONSULTAR_REPONSABLES

--}PL_CONSULTAR_SUCURSAL_EMPRESA
PROCEDURE PL_CONSULTAR_SUCURSAL_EMPRESA(
   NMEMP_ACU_CODIGO IN NUMBER,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
)
IS EXSALIR EXCEPTION;
 BEGIN
 IF NMEMP_ACU_CODIGO IS NULL THEN  
  RAISE EXSALIR;
  VCMENSAJEPROCESO :='EL PARAMETRO DE ENTRADA' || NMEMP_ACU_CODIGO ||  'ESTA VACIO';
  END IF;

  OPEN CSCONSULTA FOR 

  SELECT SUE_CODIGO,
  EMP_ACU_CODIGO,
  SUE_NOMBRE,
  SUE_DIRECCION,
  CES_CODIGO,
  AUD_USUARIO,
  AUD_FECHA 
  FROM sucursal_empresa  
  WHERE emp_acu_codigo = NMEMP_ACU_CODIGO ;
  VCESTADOPROCESO :='S';
  VCMENSAJEPROCESO:='Proceso terminado con exito.';

  EXCEPTION  
  WHEN EXSALIR THEN 
  VCESTADOPROCESO := 'N'; 
  WHEN OTHERS THEN
  VCESTADOPROCESO := 'N';
  VCMENSAJEPROCESO :='ERROR AL CONSULTAR LOS SUCURSALES EN PL_CONSULTAR_SUCURSAL_EMPRESA CAUSADO POR'||sqlerrm;

END PL_CONSULTAR_SUCURSAL_EMPRESA;
--}PL_CONSULTAR_SUCURSAL_EMPRESA

--}PL_CONSULTAR_CONTACTO_SUCURSAL
PROCEDURE PL_CONSULTAR_CONTACTO_SUCURSAL(
    NMSUE_CODIGO IN NUMBER,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
)IS
EXSALIR EXCEPTION;

BEGIN
 IF NMSUE_CODIGO IS NULL THEN
 RAISE EXSALIR;
 VCMENSAJEPROCESO :='EL PARAMETRO DE ENTRADA' || NMSUE_CODIGO ||  'ESTA VACIO';
 END IF;

 OPEN CSCONSULTA FOR
 SELECT CSU_CODIGO,
       SUE_CODIGO,
       CSU_NOMBRE,
       TIC_CODIGO,
       CSU_DESCRIPCION,
       AUD_USUARIO,
       AUD_FECHA 
FROM CONTACTO_SUCURSAL 
WHERE (SUE_CODIGO=NMSUE_CODIGO) 
ORDER BY CSU_CODIGO asc;

  VCESTADOPROCESO :='S';
  VCMENSAJEPROCESO:='Proceso terminado con exito.';

EXCEPTION 
WHEN EXSALIR THEN
VCESTADOPROCESO := 'N';

WHEN OTHERS THEN
  VCESTADOPROCESO := 'N';
  VCMENSAJEPROCESO :='ERROR AL CONSULTAR LOS CONTACTOS SUCURSALES EN PL_CONSULTAR_CONTACTO_SUCURSAL CAUSADO POR'||sqlerrm;

END PL_CONSULTAR_CONTACTO_SUCURSAL;
--}PL_CONSULTAR_CONTACTO_SUCURSAL

--}PL_CONSULTAR_EMPRESA_PRINCIPAL
PROCEDURE PL_CONSULTAR_EMPRESA_PRINCIPAL(
    NMEMP_ACU_CODIGO IN VARCHAR2,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
) IS
EXSALIR EXCEPTION;
BEGIN 
IF NMEMP_ACU_CODIGO IS NULL THEN
RAISE EXSALIR;
VCMENSAJEPROCESO :='EL PARAMETRO DE ENTRADA' || NMEMP_ACU_CODIGO ||  'ESTA VACIO';
END IF;


OPEN CSCONSULTA FOR
select FB_NOM_EMPRESA( TDC_TD_PPAL , EMP_ND_PPAL ) EMPRESA 
from empresa_acu_principal 
where emp_acu_codigo=NMEMP_ACU_CODIGO;

 VCESTADOPROCESO :='S';
 VCMENSAJEPROCESO:='Proceso terminado con exito.';

EXCEPTION 
WHEN EXSALIR THEN
VCESTADOPROCESO := 'N';

WHEN OTHERS THEN
  VCESTADOPROCESO := 'N';
  VCMENSAJEPROCESO :='ERROR AL CONSULTAR LA EMPRESA PRINCIPAL EN PL_CONSULTAR_EMPRESA_PRINCIPAL CAUSADO POR'||sqlerrm;

END PL_CONSULTAR_EMPRESA_PRINCIPAL;
--}PL_CONSULTAR_EMPRESA_PRINCIPAL


--}PL_CONSULTAR_AGRUPACIONES

PROCEDURE PL_CONSULTAR_AGRUPACIONES(
    NMEMP_ACU_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
) IS
 EXSALIR EXCEPTION;
 VCODIGO area_organizacional.aor_codigo%TYPE;
 BEGIN 
 IF NMEMP_ACU_CODIGO IS NULL THEN
 RAISE EXSALIR;
 VCMENSAJEPROCESO :='EL PARAMETRO DE ENTRADA' || NMEMP_ACU_CODIGO ||  'ESTA VACIO';
 END IF;
 -- SE OBTIENE EL AOR_CODIGO PARA LA EMPRESA
 SELECT AOR_CODIGO
 INTO VCODIGO
 FROM area_organizacional
 where emp_acu_codigo =NMEMP_ACU_CODIGO and tao_codigo = 3; -- TAO_CODIGO = 3 ES EL ID DE RRHH;
  -- SE OBTIENE EL VCODIGO PARA LA EMPRESA
 IF VCODIGO IS NULL THEN
 RAISE EXSALIR;
 VCMENSAJEPROCESO :='ERROR TRAYENDO EL ' || VCODIGO ||  'PARA ESTA EMPRESA EN PL_CONSULTAR_AGRUPACIONES';
 END IF; 
 OPEN CSCONSULTA FOR
 select  agc.aga_codigo 
       ,ga.pac_codigo
       ,ga.aga_descripcion
       ,AOR_CODIGO
       from area_org_agrupa_acuerdo agc,  agrupacion_acuerdo ga
 where agc.aga_codigo=ga.aga_codigo
 and AOR_CODIGO = VCODIGO 
 order by agc.aga_codigo asc;
 VCESTADOPROCESO :='S';
 VCMENSAJEPROCESO:='Proceso terminado con exito.';
 EXCEPTION
 WHEN EXSALIR THEN 
 VCESTADOPROCESO := 'N';
 WHEN OTHERS THEN
 VCESTADOPROCESO := 'N';
VCMENSAJEPROCESO :='ERROR AL CONSULTAR LA AGRUPACION PARA LA EMPRESA  EN PL_CONSULTAR_AGRUPACIONES CAUSADO POR'||sqlerrm;
 END PL_CONSULTAR_AGRUPACIONES;
--}PL_CONSULTAR_AGRUPACIONES

--}PL_CONSULTAR_ACUERDO_POR_AGRUP
PROCEDURE PL_CONSULTAR_ACUERDO_POR_AGRUP(
    NMEMP_ACU_CODIGO IN NUMBER ,
    NMAGA_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
)IS
EXSALIR EXCEPTION;
BEGIN
IF NMEMP_ACU_CODIGO IS NULL THEN
RAISE EXSALIR;
VCMENSAJEPROCESO := 'ERROR TRAYENDO EL ' || NMEMP_ACU_CODIGO ||  'PARA ESTA EMPRESA EN PL_CONSULTAR_ACUERDO_POR_AGRUPACION';
END IF;
IF NMAGA_CODIGO IS NULL THEN
RAISE EXSALIR;
VCMENSAJEPROCESO := 'ERROR TRAYENDO EL ' || NMEMP_ACU_CODIGO ||  'PARA ESTA EMPRESA EN PL_CONSULTAR_ACUERDO_POR_AGRUPACION';
END IF;
OPEN CSCONSULTA FOR SELECT b.ORDEN
 ,b.AGA_CODIGO_REF
 ,b.ACC_TIPO_DETALLE
 ,b.ATR_NOMBRE
 ,b.COL_CODIGO
 ,b.ACC_CODIGO
 ,b.EMP_ACU_CODIGO
 ,b.AOR_CODIGO
 ,b.AGA_CODIGO
 ,b.ACC_VALOR_ATRIBUTO
 ,b.ACC_OBSERVACION
 ,b.ACC_ORDEN
 ,b.AUD_USUARIO
 ,b.AUD_FECHA 
 FROM ACR.V_ORDEN_ACUERDO_CLIENTE b
 WHERE b.AGA_CODIGO = NMAGA_CODIGO
 AND NMEMP_ACU_CODIGO = b.EMP_ACU_CODIGO
 order by ORDEN;
 VCESTADOPROCESO :='S';
 VCMENSAJEPROCESO:='Proceso terminado con exito.';
 EXCEPTION
 WHEN EXSALIR THEN
  VCESTADOPROCESO :='N';
 WHEN OTHERS THEN
  VCESTADOPROCESO := 'N';
  VCMENSAJEPROCESO :='ERROR AL CONSULTAR ACUERDO POR AGRUPACION PARA LA EMPRESA  EN PL_CONSULTAR_ACUERDO_POR_AGRUPACION CAUSADO POR'||sqlerrm;
END PL_CONSULTAR_ACUERDO_POR_AGRUP;

--}PL_CONSULTAR_ACUERDO_POR_AGRUP

--}PL_CONSULTAR_VALOR_ATRIBUTO
PROCEDURE PL_CONSULTAR_VALOR_ATRIBUTO(
    NMACC_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
) IS

EXSALIR EXCEPTION;

BEGIN
IF NMACC_CODIGO IS NULL THEN
RAISE EXSALIR;
VCMENSAJEPROCESO := 'ERROR TRAYENDO EL ' || NMACC_CODIGO ||  'PARA ESTA EMPRESA EN PL_CONSULTAR_VALOR_ATRIBUTO';
END IF;

OPEN CSCONSULTA FOR 
 SELECT 
 DAC_CODIGO
 ,ACC_CODIGO
 ,AGA_CODIGO
 ,ATR_NOMBRE
 ,ATR_NOMBRE_DETALLE
 ,DAC_DETALLE_DESCRIPCION
 ,DAC_VALOR_ATR_DETALLE
 ,DAC_ORDEN
 ,DAC_OBSERVACION
 ,AUD_USUARIO
 ,AUD_FECHA
 FROM acuerdo_cliente_detalle
 WHERE (ACC_CODIGO=NMACC_CODIGO)  
 order by (DAC_ORDEN);

 VCESTADOPROCESO :='S';
 VCMENSAJEPROCESO:='Proceso terminado con exito.';

 EXCEPTION
 WHEN EXSALIR THEN
 VCESTADOPROCESO :='N';
 WHEN OTHERS THEN
 VCESTADOPROCESO := 'N';
VCMENSAJEPROCESO :='ERROR AL CONSULTAR LOS VALORES DE ATRIBUTO PARA LA  AGRUPACION  EN PL_CONSULTAR_VALOR_ATRIBUTO CAUSADO POR'||sqlerrm;

END PL_CONSULTAR_VALOR_ATRIBUTO;
 --{PL_CONSULTAR_VALOR_ATRIBUTO

 --{PL_CONSULTAR_OBSERVACIONES_GEN
PROCEDURE PL_CONSULTAR_OBSERVACIONES_GEN(
    NMEMP_ACU_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
) IS
EXSALIR EXCEPTION;

BEGIN

IF NMEMP_ACU_CODIGO IS NULL THEN
RAISE EXSALIR;
VCMENSAJEPROCESO := 'ERROR TRAYENDO EL ' || NMEMP_ACU_CODIGO ||  'PARA ESTA EMPRESA EN PL_CONSULTAR_OBSERVACIONES_GEN';
END IF;

OPEN CSCONSULTA FOR 
SELECT 
 OBS_CODIGO
,EMP_ACU_CODIGO
,TAO_CODIGO
,OBG_DETALLE
,AUD_USUARIO
,AUD_FECHA 
FROM observacion_general
WHERE OBSERVACION_GENERAL.TAO_CODIGO = 3 -- ID DE RRHH 
and (EMP_ACU_CODIGO=NMEMP_ACU_CODIGO);
 VCESTADOPROCESO :='S';
 VCMENSAJEPROCESO:='Proceso terminado con exito.';

EXCEPTION
WHEN EXSALIR THEN 
 VCESTADOPROCESO :='N';
 WHEN OTHERS THEN
 VCESTADOPROCESO := 'N';
VCMENSAJEPROCESO :='ERROR AL CONSULTAR LAS OBSERVACIONES PARA LA  AGRUPACION  EN PL_CONSULTAR_OBSERVACIONES_GEN CAUSADO POR'||sqlerrm;

END PL_CONSULTAR_OBSERVACIONES_GEN;
 --{PL_CONSULTAR_OBSERVACIONES_GEN


 --{PL_CONSULTAR_NOTA_AGRUPACION
PROCEDURE PL_CONSULTAR_NOTA_AGRUPACION(
    NMAGA_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
) IS
EXSALIR EXCEPTION;
BEGIN
IF NMAGA_CODIGO IS NULL THEN
RAISE EXSALIR;
VCMENSAJEPROCESO := 'ERROR TRAYENDO EL ' || NMAGA_CODIGO ||  'PARA ESTA EMPRESA EN PL_CONSULTAR_NOTA_AGRUPACION';
END IF;

OPEN CSCONSULTA FOR
SELECT NOA_DESCRIPCION
,AUD_USUARIO
,AUD_FECHA
,AGA_CODIGO
,NOA_CODIGO
,NOA_ORDEN 
FROM ACR.NOTA_AGRUPACION 
WHERE AGA_CODIGO = NMAGA_CODIGO;

 VCESTADOPROCESO :='S';
 VCMENSAJEPROCESO:='Proceso terminado con exito.';

EXCEPTION
WHEN EXSALIR THEN 
 VCESTADOPROCESO :='N';
 WHEN OTHERS THEN
 VCESTADOPROCESO := 'N';
VCMENSAJEPROCESO :='ERROR AL CONSULTAR LA NOTA PARA LA AGRUPACION  EN PL_CONSULTAR_NOTA_AGRUPACION CAUSADO POR'||sqlerrm;

END PL_CONSULTAR_NOTA_AGRUPACION;
 --{PL_CONSULTAR_NOTA_AGRUPACION

  --{PL_CONSULTAR_RESPONSABLES_EMP
 PROCEDURE PL_CONSULTAR_RESPONSABLES_EMP(
    NMEMP_ACU_CODIGO IN NUMBER ,
    NMAOR_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
)IS
EXSALIR EXCEPTION;
BEGIN
IF NMEMP_ACU_CODIGO IS NULL THEN
RAISE EXSALIR;
VCMENSAJEPROCESO := 'ERROR TRAYENDO EL ' || NMEMP_ACU_CODIGO ||  'PARA ESTA EMPRESA EN PL_CONSULTAR_RESPONSABLES_EMP';
END IF;

IF NMAOR_CODIGO IS NULL THEN
RAISE EXSALIR;
VCMENSAJEPROCESO := 'ERROR TRAYENDO EL ' || NMAOR_CODIGO ||  'PARA ESTA EMPRESA EN PL_CONSULTAR_RESPONSABLES_EMP';
END IF;

OPEN CSCONSULTA FOR
SELECT   trs.trs_codigo 
        , trs.trs_tipo 
        , res.rcl_codigo 
        , res.rcl_nombre 
        , res.rcl_cargo
        , res.rcl_observacion
FROM RESPONSABLE_CLIENTE res , ACR.tipo_responsable trs
WHERE res.trs_codigo = trs.trs_codigo
and res.AOR_CODIGO = NMAOR_CODIGO
and (res.EMP_ACU_CODIGO=NMEMP_ACU_CODIGO)
ORDER by res.rcl_orden;

 VCESTADOPROCESO :='S';
 VCMENSAJEPROCESO:='Proceso terminado con exito.';

 EXCEPTION
 WHEN EXSALIR THEN 
  VCESTADOPROCESO :='N';
  WHEN OTHERS THEN 
 VCESTADOPROCESO := 'N';
VCMENSAJEPROCESO :='ERROR AL CONSULTAR LOS RESPONSABLES  EN PL_CONSULTAR_RESPONSABLES_EMP CAUSADO POR'||sqlerrm;

END PL_CONSULTAR_RESPONSABLES_EMP;
--{PL_CONSULTAR_RESPONSABLES_EMP

--{PL_CONSULTAR_CONTACTO_RESPONSABLES
PROCEDURE PL_CONSULTAR_CONTACTO_RESPONSA(
    NMRCL_CODIGO IN NUMBER ,
    CSCONSULTA OUT CSCURSOR,
    VCESTADOPROCESO OUT VARCHAR2,
    VCMENSAJEPROCESO OUT VARCHAR2
) IS
EXSALIR EXCEPTION;
BEGIN
IF NMRCL_CODIGO IS NULL THEN
RAISE EXSALIR;
VCMENSAJEPROCESO := 'ERROR TRAYENDO EL ' || NMRCL_CODIGO ||  'PARA ESTA EMPRESA EN PL_CONSULTAR_CONTACTO_RESPONSABLES';
END IF;

OPEN CSCONSULTA FOR
SELECT 
cont.cor_codigo,
tic.tic_descripcion,
cont.cor_descripcion
FROM contacto_responsable cont , tipo_contacto tic
where cont.tic_codigo = tic.tic_codigo
AND ( cont.RCL_CODIGO= NMRCL_CODIGO) ;

 VCESTADOPROCESO :='S';
 VCMENSAJEPROCESO:='Proceso terminado con exito.';

EXCEPTION
WHEN EXSALIR THEN

VCESTADOPROCESO :='N';

WHEN OTHERS THEN
VCESTADOPROCESO := 'N';
VCMENSAJEPROCESO :='ERROR AL CONSULTAR LOS RESPONSABLES  EN PL_CONSULTAR_CONTACTO_RESPONSA CAUSADO POR'||sqlerrm;

END PL_CONSULTAR_CONTACTO_RESPONSA;



--{PL_CONSULTAR_CONTACTO_RESPONSA   
END QB_ACUERDO_EMPRESA;