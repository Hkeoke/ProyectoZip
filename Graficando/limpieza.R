#!/usr/bin/env Rscript

# Leer el archivo con la codificación correcta
datos <- read.csv("BD.csv", sep = ";", fileEncoding = "latin1", check.names = FALSE)

# Eliminar columnas vacías
datos <- datos[, colSums(is.na(datos) | datos == "") != nrow(datos)]

# Asegurarse de que todos los nombres de columnas sean válidos
nombres <- names(datos)
nombres <- make.names(nombres, unique = TRUE)
names(datos) <- nombres

# Guardar el archivo limpio
write.csv(datos, "BD_limpio.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "") 