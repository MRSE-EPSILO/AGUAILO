# Estación de tipo pesaje -------------------------------------------------

pesaje <- function(df_nueva){
  a <- read.xlsx(df_nueva, startRow = 1440) %>% 
    select(c(1,4,6)) %>% 
    rename_at(vars(1,2,3), ~c("date", "pp", "temp")) %>% 
    mutate(date = as.POSIXct(date * 86400, origin = "1899-12-30", tz = "UTC"),
           prec_5min = c(0, diff(pp))) %>% 
    mutate(date = as.Date(date)) %>% 
    summarise(temp = mean(temp, na.rm = TRUE),
              pp = sum(prec_5min, na.rm = TRUE),.by = "date") %>% 
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
