# Load packages
library(raster)
library(terra)
library(ggplot2)
################################################################################
# List the ".tif" files in your work folder 
## Optimistic
arquivos_2100_ot <- list.files(path = "./mapas/2100_ot/", pattern = "\\.tif$", full.names = TRUE)
arquivos_2080_ot <- list.files(path = "./mapas/2080_ot/", pattern = "\\.tif$", full.names = TRUE)
arquivos_2060_ot <- list.files(path = "./mapas/2060_ot/", pattern = "\\.tif$", full.names = TRUE)

## Pessismistic
arquivos_2100_pe <- list.files(path = "./mapas/2100_pe/", pattern = "\\.tif$", full.names = TRUE)
arquivos_2080_pe <- list.files(path = "./mapas/2080_pe/", pattern = "\\.tif$", full.names = TRUE)
arquivos_2060_pe <- list.files(path = "./mapas/2060_pe/", pattern = "\\.tif$", full.names = TRUE)

# Load the rasters into a SpatRaster
## Optimistic
raster_2100_ot <- rast(arquivos_2100_ot)
raster_2080_ot <- rast(arquivos_2080_ot)
raster_2060_ot <- rast(arquivos_2060_ot)

## Pessimistic
raster_2100_pe <- rast(arquivos_2100_pe)
raster_2080_pe <- rast(arquivos_2080_pe)
raster_2060_pe <- rast(arquivos_2060_pe)

# Calculates the average pixel by pixel
## Optimistic
media_raster_2100_ot <- mean(raster_2100_ot, na.rm = TRUE)
media_raster_2080_ot <- mean(raster_2080_ot, na.rm = TRUE)
media_raster_2060_ot <- mean(raster_2060_ot, na.rm = TRUE)

## Pessimistic
media_raster_2100_pe <- mean(raster_2100_pe, na.rm = TRUE)
media_raster_2080_pe <- mean(raster_2080_pe, na.rm = TRUE)
media_raster_2060_pe <- mean(raster_2060_pe, na.rm = TRUE)


# Salve the results
## Optimistic
writeRaster(media_raster_2100_ot, "media_raster_ot_2100.tif", overwrite = TRUE)
writeRaster(media_raster_2080_ot, "media_raster_ot_2080.tif", overwrite = TRUE)
writeRaster(media_raster_2060_ot, "media_raster_ot_2060.tif", overwrite = TRUE)

## Pessimistic
writeRaster(media_raster_2100_pe, "media_raster_pe_2100.tif", overwrite = TRUE)
writeRaster(media_raster_2080_pe, "media_raster_pe_2080.tif", overwrite = TRUE)
writeRaster(media_raster_2060_pe, "media_raster_pe_2060.tif", overwrite = TRUE)

# Calculates the difference between scenarios 
diff_media_2060 <- abs(media_raster_2060_ot - media_raster_2060_pe)
diff_media_2080 <- abs(media_raster_2080_ot - media_raster_2080_pe)
diff_media_2100 <- abs(media_raster_2100_ot - media_raster_2100_pe)

################################################################################
# Threshold for binary maps
limiar_adequabildiade <- 0.7

# Present (BASELINE)
present_adequado <- present_mean >= limiar_adequabilidade

# Binarizing Future Scenarios
## Optimistic
oti_adequado_2100 <- media_raster_2100_ot >= limiar_adequabilidade
oti_adequado_2080 <- media_raster_2080_ot >= limiar_adequabilidade
oti_adequado_2060 <- media_raster_2060_ot >= limiar_adequabilidade

## Pessimistic
pes_adequado_2100 <- media_raster_2100_pe >= limiar_adequabilidade
pes_adequado_2080 <- media_raster_2080_pe >= limiar_adequabilidade
pes_adequado_2060 <- media_raster_2060_pe >= limiar_adequabilidade

# --- Maps (Present vs. Future) --
# Definition of Categories:
# 1 = Unsuitable (Unsuit. Present and Uns. Future)
# 2 = Loss/Contraction (Suit. Present and Unsuit. Future)
# 3 = Gain/Expansion (Unsuit. Present and Suit. Future)
# 4 = Stable/Refugia (Suit. Present and Suit. Future)


## Comparisons with the current scenario
mapa_cat_Pres_vs_OT_2100 <- 1 * (!present_adequado & !oti_adequado_2100) +
  2 * (present_adequado  & !oti_adequado_2100) +
  3 * (!present_adequado & oti_adequado_2100)  +
  4 * (present_adequado  & oti_adequado_2100)

mapa_cat_Pres_vs_OT_2080 <- 1 * (!present_adequado & !oti_adequado_2080) +
  2 * (present_adequado  & !oti_adequado_2080) +
  3 * (!present_adequado & oti_adequado_2080)  +
  4 * (present_adequado  & oti_adequado_2080)

mapa_cat_Pres_vs_OT_2060 <- 1 * (!present_adequado & !oti_adequado_2060) +
  2 * (present_adequado  & !oti_adequado_2060) +
  3 * (!present_adequado & oti_adequado_2060)  +
  4 * (present_adequado  & oti_adequado_2060)

## Comparisons with the pessimistic scenario
mapa_cat_Pres_vs_PE_2100 <- 1 * (!present_adequado & !pes_adequado_2100) +
  2 * (present_adequado  & !pes_adequado_2100) +
  3 * (!present_adequado & pes_adequado_2100)  +
  4 * (present_adequado  & pes_adequado_2100)

mapa_cat_Pres_vs_PE_2080 <- 1 * (!present_adequado & !pes_adequado_2080) +
  2 * (present_adequado  & !pes_adequado_2080) +
  3 * (!present_adequado & pes_adequado_2080)  +
  4 * (present_adequado  & pes_adequado_2080)

mapa_cat_Pres_vs_PE_2060 <- 1 * (!present_adequado & !pes_adequado_2060) +
  2 * (present_adequado  & !pes_adequado_2060) +
  3 * (!present_adequado & pes_adequado_2060)  +
  4 * (present_adequado  & pes_adequado_2060)

# --- Viewing and Saving  ---
# Now we have 6 maps to plot (2 scenario x 3 times) 
# Define colors and labels for the map (Choose user)
cores <- c("gray80", "red", "yellow", "darkblue")

# Correct legends for our logic:
legendas <- c("Unsuitable (Present and Future)", "Loss (Contraction)", "Gain (Expansion)", "Stable (Refugia)")

# Save in SVG
svg("mapas_estabilidade_presente_vs_futuro.svg", width = 12, height = 8)
par(mfrow = c(2, 3), mar = c(3, 3, 4, 1), oma = c(0, 0, 2, 0))

# Optimistic scenario (vs. Present)
plot(mapa_cat_Pres_vs_OT_2060, type = "classes", main = "Presente vs. Otimista 2060", col = cores, legend = FALSE)
plot(mapa_cat_Pres_vs_OT_2080, type = "classes", main = "Presente vs. Otimista 2080", col = cores, legend = FALSE)
plot(mapa_cat_Pres_vs_OT_2100, type = "classes", main = "Presente vs. Otimista 2100", col = cores, legend = FALSE)

# Pessimistic scenario (vs. Present)
plot(mapa_cat_Pres_vs_PE_2060, type = "classes", main = "Presente vs. Pessimista 2060", col = cores, legend = FALSE)
plot(mapa_cat_Pres_vs_PE_2080, type = "classes", main = "Presente vs. Pessimista 2080", col = cores, legend = FALSE)
plot(mapa_cat_Pres_vs_PE_2100, type = "classes", main = "Presente vs. Pessimista 2100", col = cores, 
     legend = "right", # Adiciona a legenda apenas no último plot
     plg = list(legend = legendas, col = cores, cex = 1.0))

dev.off()
################################################################################