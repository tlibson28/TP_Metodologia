# =============================================================================
# CONSTRUIR LA BASE DE DATOS 
#   Demanda eléctrica GRAN BS.AS. - base para el modelo Random Forest
# =============================================================================


# RESULTADO
#   Base_de_Datos.xlsx, dentro de la carpeta 2_Base_de_datos,
#   con dos hojas visibles:
#     "Base madre"  -> la tabla de 2.007 filas x 11 columnas.
#     "Diccionario" -> qué significa cada columna, unidad y cómo se calcula.

# =============================================================================


# -----------------------------------------------------------------------------
# PASO 0. Elegir la carpeta del trabajo
# -----------------------------------------------------------------------------
# Se abre una ventana para elegir a mano la carpeta "TP Metodologia" (la que
# contiene 1_Fuentes y 2_Base_de_datos), igual que el modelo con file.choose().
elegir_carpeta <- function() {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        raiz <- rstudioapi::selectDirectory(caption = "Elegí la carpeta TP Metodologia")
    } else if (.Platform$OS.type == "windows") {
        raiz <- utils::choose.dir(caption = "Elegí la carpeta TP Metodologia")
    } else {
        stop("Corré este script desde RStudio para poder elegir la carpeta.")
    }
    if (is.null(raiz) || is.na(raiz))
        stop("No se eligió ninguna carpeta.")
    if (!dir.exists(file.path(raiz, "1_Fuentes")) || !dir.exists(file.path(raiz, "2_Base_de_datos")))
        stop("La carpeta elegida no tiene 1_Fuentes y 2_Base_de_datos. Elegí la carpeta TP Metodologia.")
    normalizePath(raiz, winslash = "/")
}


# -----------------------------------------------------------------------------
# DATOS FIJOS DEL ESTUDIO
# -----------------------------------------------------------------------------
fecha_inicio <- as.Date("2021-01-01")
fecha_fin    <- as.Date("2026-06-30")

# Las cuatro estaciones del SMN (número OMM) y el nombre corto de sus columnas.
estaciones <- data.frame(
    numero_omm = c("87553",        "87576",  "87585",           "87593"),
    nombre     = c("SAN FERNANDO AERO", "EZEIZA AERO", "BUENOS AIRES OBSERVATORIO", "LA PLATA AERO"),
    sufijo     = c("san_fernando", "ezeiza", "buenos_aires_obs", "la_plata"),
    stringsAsFactors = FALSE)

# Archivos originales y su huella MD5. Si un archivo fue modificado, la huella
# cambia y el script se detiene: así nunca se construye la base con datos alterados.
archivo_demanda    <- "1_Fuentes/CAMMESA/Base Demanda Diaria 2017 2026.xlsx"
archivo_clima      <- "1_Fuentes/SMN/smn_datos_meteorologicos_desde_2021.lst"
archivo_estaciones <- "1_Fuentes/SMN/smn_estaciones_meteorologicas21-26.xlsx"
archivo_leame      <- "1_Fuentes/SMN/LEAME_SMN21-26.xlsx"
huellas_md5 <- c(
    "1_Fuentes/CAMMESA/Base Demanda Diaria 2017 2026.xlsx"     = "24c0f083259ece67c611929a914f6dc3",
    "1_Fuentes/SMN/smn_datos_meteorologicos_desde_2021.lst"    = "cf8b6ea0018baaabe728f1dc016ccda6",
    "1_Fuentes/SMN/smn_estaciones_meteorologicas21-26.xlsx"    = "88674191216f2c5efc4a384d83534acd",
    "1_Fuentes/SMN/LEAME_SMN21-26.xlsx"                        = "e4c50a6f75f3e824aa7a8f095609dfbb")

# -----------------------------------------------------------------------------
# FUNCIÓN AUXILIAR: convertir texto a número sin inventar valores
# -----------------------------------------------------------------------------
# "S/D" (sin dato) o una celda vacía pasan a NA. Cualquier otro texto que no
# sea un número detiene el script, para no perder un código desconocido.
texto_a_numero <- function(x, variable) {
    z <- trimws(as.character(x))
    sin_dato <- is.na(z) | z == "" | toupper(z) == "S/D"
    invalido <- !sin_dato & !grepl("^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$", z)
    if (any(invalido))
        stop("Valor no numérico en ", variable, ": ",
             paste(head(unique(z[invalido]), 8), collapse = ", "))
    numero <- suppressWarnings(as.numeric(z))
    numero[sin_dato] <- NA_real_
    numero
}


# -----------------------------------------------------------------------------
# PASO 1. Verificar que los archivos originales estén completos e intactos
# -----------------------------------------------------------------------------
verificar_originales <- function() {
    for (archivo in names(huellas_md5)) {
        if (!file.exists(archivo))
            stop("Falta el archivo original ", archivo, ". Descomprimí todo el ZIP de la entrega.")
        if (unname(tools::md5sum(archivo)) != huellas_md5[[archivo]])
            stop("El archivo ", archivo, " no coincide con el original registrado. ",
                 "No se continúa con datos alterados.")
    }
    cat("Paso 1: los 4 archivos originales están completos e intactos.\n")
}


# -----------------------------------------------------------------------------
# PASO 2. Leer el clima del SMN: temperatura media y humedad de 4 estaciones
# -----------------------------------------------------------------------------
# El archivo .lst es un texto separado por tabulaciones con una fila por
# estación y día. Se lee todo como texto para no perder ningún código.
leer_clima <- function() {
    smn <- read.delim(archivo_clima, skip = 1, check.names = FALSE,
                      colClasses = "character", na.strings = character(),
                      quote = "", comment.char = "", fileEncoding = "UTF-8")
    names(smn) <- trimws(names(smn))
    smn[] <- lapply(smn, trimws)

    # El listado repite su encabezado en algunas filas: se quitan sólo esas.
    encabezado_repetido <- smn$NRO_OMM == "NRO_OMM" & smn$FECHA == "FECHA"
    smn <- smn[!encabezado_repetido, ]

    # Fechas DD/MM/AAAA -> Date (se verifica la conversión ida y vuelta).
    smn$fecha <- as.Date(smn$FECHA, format = "%d/%m/%Y")
    stopifnot(!anyNA(smn$fecha), all(format(smn$fecha, "%d/%m/%Y") == smn$FECHA))

    # Quedarse con las 4 estaciones y el período del estudio.
    smn <- smn[smn$NRO_OMM %in% estaciones$numero_omm &
               smn$fecha >= fecha_inicio & smn$fecha <= fecha_fin, ]
    smn <- smn[order(smn$fecha, smn$NRO_OMM), ]
    if (anyDuplicated(smn[c("fecha", "NRO_OMM")]))
        stop("Hay una estación con dos filas para la misma fecha.")

    # Comprobar que los números OMM correspondan a las estaciones esperadas.
    catalogo <- as.data.frame(readxl::read_excel(archivo_estaciones))
    catalogo$NRO_OMM <- trimws(as.character(catalogo$NRO_OMM))
    nombres_catalogo <- trimws(catalogo$NOMBRE)[match(estaciones$numero_omm, catalogo$NRO_OMM)]
    stopifnot(identical(nombres_catalogo, estaciones$nombre),
              all(smn$NOMBRE == estaciones$nombre[match(smn$NRO_OMM, estaciones$numero_omm)]))

    # TMEDIA = temperatura media diaria (°C); HUM_REL = humedad relativa media (%).
    smn$temperatura <- texto_a_numero(smn$TMEDIA, "TMEDIA")
    smn$humedad     <- texto_a_numero(smn$HUM_REL, "HUM_REL")

    # Controles informativos: se reportan, pero NO se borra ni corrige ningún valor.
    fuera_de_rango <- sum(smn$temperatura < -50 | smn$temperatura > 60, na.rm = TRUE) +
                      sum(smn$humedad < 0 | smn$humedad > 100, na.rm = TRUE)
    cat("Paso 2: clima leído -", nrow(smn), "filas (estación x día);",
        sum(is.na(smn$temperatura)) + sum(is.na(smn$humedad)), "valores sin dato;",
        fuera_de_rango, "valores fuera de rango físico.\n")
    smn[c("fecha", "NRO_OMM", "temperatura", "humedad")]
}


# -----------------------------------------------------------------------------
# PASO 3. Leer la demanda de CAMMESA de la región GRAN BS.AS.
# -----------------------------------------------------------------------------
# Hoja "Datos Región": una fila por día y una columna por región eléctrica.
# La demanda es POTENCIA MEDIA del día, en MW (no energía en MWh).
leer_demanda <- function() {
    hoja <- as.data.frame(readxl::read_excel(archivo_demanda, sheet = "Datos Región",
        col_names = FALSE, col_types = "text", .name_repair = "minimal"))
    # La fila de títulos es la que contiene "GRAN BS.AS.".
    fila_titulos <- which(apply(hoja, 1, function(fila) any(trimws(fila) == "GRAN BS.AS.", na.rm = TRUE)))
    stopifnot(length(fila_titulos) == 1)
    names(hoja) <- trimws(as.character(hoja[fila_titulos, ]))
    datos <- hoja[seq.int(fila_titulos + 1, nrow(hoja)), ]

    # Excel guarda las fechas como número de días desde el 30/12/1899.
    serial <- texto_a_numero(datos$Fecha, "Fecha CAMMESA")
    fecha  <- as.Date(serial, origin = "1899-12-30")
    stopifnot(!anyNA(fecha), !anyDuplicated(fecha),
              # Control cruzado: la fecha coincide con las columnas AÑO y MES.
              all(texto_a_numero(datos[["AÑO"]], "AÑO") == as.numeric(format(fecha, "%Y"))),
              all(format(as.Date(texto_a_numero(datos$MES, "MES"), origin = "1899-12-30"), "%Y-%m") ==
                  format(fecha, "%Y-%m")))

    demanda <- data.frame(fecha = fecha,
                          demanda = texto_a_numero(datos[["GRAN BS.AS."]], "GRAN BS.AS."))
    demanda <- demanda[demanda$fecha >= fecha_inicio & demanda$fecha <= fecha_fin, ]
    demanda <- demanda[order(demanda$fecha), ]
    rownames(demanda) <- NULL
    cat("Paso 3: demanda leída -", nrow(demanda), "días;",
        sum(is.na(demanda$demanda)), "sin dato.\n")
    demanda
}


# -----------------------------------------------------------------------------
# PASO 4. Unir demanda y clima por fecha
# -----------------------------------------------------------------------------
# Se parte de las fechas de la demanda y, para cada estación, se busca su fila
# de la misma fecha (match). La unión es uno a uno: no se agregan ni se
# duplican filas. Si una estación no tuviera dato ese día, quedaría NA.
unir_por_fecha <- function(demanda, clima) {
    base <- data.frame(dia_a_evaluar = demanda$fecha,
                       demanda_electrica_neta_media_MW = demanda$demanda)
    for (i in seq_len(nrow(estaciones))) {
        estacion <- clima[clima$NRO_OMM == estaciones$numero_omm[i], ]
        fila <- match(base$dia_a_evaluar, estacion$fecha)
        base[[paste0("temperatura_media_C_", estaciones$sufijo[i])]]       <- estacion$temperatura[fila]
        base[[paste0("humedad_relativa_media_pct_", estaciones$sufijo[i])]] <- estacion$humedad[fila]
    }
    # Control: están todos los días del período, en orden y sin repetir.
    calendario <- seq(fecha_inicio, fecha_fin, by = "day")
    stopifnot(nrow(base) == length(calendario), all(base$dia_a_evaluar == calendario))
    cat("Paso 4: demanda y clima unidos -", nrow(base), "días, uno por fila.\n")
    base
}


# -----------------------------------------------------------------------------
# PASO 5. Agregar el mes del año
# -----------------------------------------------------------------------------
# En esta versión no se agregan el día de la semana ni la demanda de días
# anteriores: el modelo nuevo no los usa.
agregar_calendario <- function(base) {
    base$mes_del_anio <- as.integer(format(base$dia_a_evaluar, "%m"))   # 1 = enero ... 12 = diciembre
    cat("Paso 5: mes del año agregado.\n")
    base
}


# -----------------------------------------------------------------------------
# PASO 6. Diccionario de las 11 columnas
# -----------------------------------------------------------------------------
crear_diccionario <- function(base) {
    estacion_txt <- c(san_fernando = "San Fernando Aero (OMM 87553)",
                      ezeiza = "Ezeiza Aero (OMM 87576)",
                      buenos_aires_obs = "Buenos Aires Observatorio (OMM 87585)",
                      la_plata = "La Plata Aero (OMM 87593)")
    filas <- list(
        c("dia_a_evaluar", "Fecha del día (clave: hay una sola fila por fecha).", "fecha",
          "CAMMESA, columna Fecha", "Número de serie de Excel convertido a fecha y controlado contra las columnas AÑO y MES.",
          "Identifica el día. No es una variable del modelo."),
        c("demanda_electrica_neta_media_MW", "Demanda eléctrica neta media del día en la región GRAN BS.AS. Es potencia promedio, no el pico del día ni energía.", "MW",
          "CAMMESA, hoja Datos Región, columna GRAN BS.AS.", "Valor original, sin cambios de unidad.",
          "Es lo que el modelo quiere estimar (la respuesta)."))
    for (s in estaciones$sufijo) {
        filas[[length(filas) + 1]] <- c(paste0("temperatura_media_C_", s),
            paste0("Temperatura media del día en ", estacion_txt[[s]], "."), "°C",
            "SMN, columna TMEDIA", "Texto a número; \"S/D\" o vacío quedan sin dato. Sin rellenar ni promediar estaciones.",
            "Entrada del modelo.")
        filas[[length(filas) + 1]] <- c(paste0("humedad_relativa_media_pct_", s),
            paste0("Humedad relativa media del día en ", estacion_txt[[s]], "."), "%",
            "SMN, columna HUM_REL", "Texto a número; \"S/D\" o vacío quedan sin dato. Sin rellenar ni promediar estaciones.",
            "Entrada del modelo.")
    }
    filas <- c(filas, list(
        c("mes_del_anio", "Mes del año: 1 = enero ... 12 = diciembre.", "1 a 12",
          "Calculado desde la fecha", "format(fecha, \"%m\").", "Entrada del modelo.")))
    diccionario <- as.data.frame(do.call(rbind, filas), stringsAsFactors = FALSE)
    names(diccionario) <- c("Columna", "Qué significa", "Unidad", "De dónde sale",
                            "Cómo se obtiene", "Para qué se usa")
    stopifnot(identical(diccionario$Columna, names(base)))
    diccionario$`Días sin dato` <- vapply(base, function(x) sum(is.na(x)), integer(1), USE.NAMES = FALSE)
    diccionario
}


# -----------------------------------------------------------------------------
# PASO 7. Guardar el Excel final (y comprobarlo antes de reemplazar el anterior)
# -----------------------------------------------------------------------------
guardar_base <- function(base, diccionario, raiz) {
    salida <- file.path(raiz, "2_Base_de_datos", "Base_de_Datos.xlsx")
    temporal <- tempfile("base_nueva_", tmpdir = dirname(salida), fileext = ".xlsx")
    on.exit(unlink(temporal), add = TRUE)
    # Se escribe con writexl en un archivo temporal y se relee para comprobar
    # que la tabla guardada tenga las mismas filas, columnas y valores.
    writexl::write_xlsx(list(`Base madre` = base, Diccionario = diccionario), temporal)
    releida <- as.data.frame(readxl::read_excel(temporal, sheet = "Base madre"))
    releida$dia_a_evaluar <- as.Date(releida$dia_a_evaluar)
    if (!isTRUE(all.equal(releida, base, check.attributes = FALSE)))
        stop("El Excel guardado no coincide con la base armada.")
    if (!file.copy(temporal, salida, overwrite = TRUE))
        stop("No se pudo guardar ", salida, ". Cerrá el archivo en Excel y comprobá los permisos.")
    salida
}


# -----------------------------------------------------------------------------
# PROCESO COMPLETO
# -----------------------------------------------------------------------------
construir_base <- function(raiz) {
    raiz <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
    anterior <- getwd(); on.exit(setwd(anterior), add = TRUE); setwd(raiz)
    cat("Construyendo la base de datos desde los originales de CAMMESA y SMN...\n")
    verificar_originales()
    clima   <- leer_clima()
    demanda <- leer_demanda()
    base    <- unir_por_fecha(demanda, clima)
    base    <- agregar_calendario(base)
    stopifnot(identical(dim(base), c(2007L, 11L)))
    diccionario <- crear_diccionario(base)
    salida <- guardar_base(base, diccionario, raiz)
    cat("LISTO: base de", nrow(base), "filas y", ncol(base), "columnas.\nGuardada en:", salida, "\n")
    invisible(list(base = base, diccionario = diccionario))
}

if (!isTRUE(getOption("demanda.solo_funciones", FALSE))) {
    faltan <- c("readxl", "writexl")[!vapply(c("readxl", "writexl"), requireNamespace, logical(1), quietly = TRUE)]
    if (length(faltan))
        stop("Faltan paquetes. Instalalos con: install.packages(c(",
             paste0('"', faltan, '"', collapse = ", "), "))", call. = FALSE)
    raiz <- elegir_carpeta()
    tryCatch(construir_base(raiz), error = function(e)
        stop("No se pudo construir la base: ", conditionMessage(e), call. = FALSE))
}
