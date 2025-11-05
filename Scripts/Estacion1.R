# Estación Victoriano -----------------------------------------------------
estacion1 <- function(carpeta){
  df_nuevo <- list.files(path = carpeta, pattern = "*.csv", full.names = TRUE)[1]
  a <- read.csv("datasets/acumulado/estacion1_acumulado.csv") %>% 
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M", tz = "UTC"))
  
  b <- read.csv(df_nuevo, skip = 2,header = F)[-1]
  names(b) <- c("date", "pp")
  b$date <- as.POSIXct(b$date, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
  
  # Unión de datasets
  df1 <- rbind(a, b) %>% distinct()
  
  write.table(df1 %>%
    mutate(date = format(date, format = "%Y-%m-%d %H:%M")),
   "datasets/acumulado/estacion1_acumulado.csv",
    row.names = FALSE, sep = ",")

  df2 <- df1 %>%
    mutate(pp = if_else(pp >=5, 0, pp),
           dia = as.Date(date, format = "%Y-%m-%d")) %>% 
    summarise(pp = sum(pp, na.rm = T), .by = dia) %>% 
    hchart(., type = "column", 
           hcaes(x = dia, 
                 y = pp), color = "#154c79", name = "Precipitación") %>% 
    hc_title(
      text = "<b>Estación 01</b> - Precipitación diaria (mm)",
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
  
  df2
}
