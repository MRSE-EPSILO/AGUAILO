# Estación de tipo pesaje -------------------------------------------------

pesaje <- function(carpeta){
  df_nuevo <- list.files(path = carpeta, pattern = "*.xlsx", full.names = TRUE)[1]

  a <- read.csv("datasets/acumulado/estacion3_acumulado.csv") %>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M", tz = "UTC"))

  b <- read_excel(df_nuevo) %>%
  mutate(`Date/Time` = as.POSIXct(`Date/Time`, tz = "UTC")) %>%
  filter(`Date/Time` >= as.POSIXct(
    "2025-01-15 12:15", tz = "UTC")) %>%
  rename(
    date = `Date/Time`,
    precip_inst = `Precip Inst (mm)`,
    precip_tot = `Precip Tot (mm)`,
    peso_cubo = `Peso Cubo (g)`,
    temp = `Temperatura (°C)`
  ) %>% arrange(date) %>%
  mutate(reinicio = precip_tot < lag(
    precip_tot, default = first(precip_tot))) %>%
  mutate(
    precip_continua = precip_tot +
      cumsum(if_else(precip_tot < lag(
        precip_tot, default = first(precip_tot)),
        lag(precip_tot, default = first(precip_tot)), 0)),
        pp = precip_continua - lag(
          precip_continua, default = first(precip_continua))
  ) %>% select(date, temp, pp)
  
  a <- rbind(a,b) %>% distinct()
  write.table(a %>%
    mutate(date = format(date, format = "%Y-%m-%d %H:%M")),
   "datasets/acumulado/estacion3_acumulado.csv",
    row.names = FALSE, sep = ",")
  
  a <- a %>%
    mutate(date = as.Date(date)) %>%
    summarise(temp = mean(temp, na.rm = TRUE),
              pp = sum(pp, na.rm = TRUE),.by = "date") %>% 
    mutate(date = as.POSIXct(paste(date, "01:00:00"),
                             format = "%Y-%m-%d %H:%M:%S"),
           date = as.numeric(date)*1000)
  
  highchart() %>%
    hc_add_series(
      data = list_parse2(a %>% select(date, pp)), 
      type = "column", 
      name = "Precipitación",
      color = "#154c79",
      tooltip = list(valueDecimals = 1)) %>% 
    hc_add_series(
      data = list_parse2(a %>% select(date, temp)), 
      type = "line", 
      name = "Temperatura",
      color = "hotpink",
      tooltip = list(valueDecimals = 1)) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_title(
      text = "<b>Estación 03</b> - Precipitación diaria (mm)",
      margin = 20,
      align = "left",
      style = list(color = "#154c79", useHTML = TRUE)) %>% 
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = c("downloadPNG",
                        "downloadCSV",
                        "downloadXLS",
                        "downloadPDF"))))
}
