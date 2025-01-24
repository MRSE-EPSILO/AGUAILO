# Estación Victoriano -----------------------------------------------------

estacion1 <- function(df_nuevo){
  a <- read.csv("data_marzo.csv") %>% 
    mutate(date = as.POSIXct(paste(date, hora, sep = " "),format = "%Y-%m-%d %H:%M")) %>% 
    select(-hora)
  
  b <- read.csv(df_nuevo, skip = 2,header = F)
  names(b) <- c("date", "pp")
  b$date <- as.POSIXct(b$date, format = "%Y/%m/%d %H:%M")
  
  # Unión de datasets
  df2 <- rbind(a, b) %>% 
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
