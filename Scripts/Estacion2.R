# Estación de la qocha ----------------------------------------------------

qocha <- function(carpeta){
  df_nuevo <- list.files(path = carpeta, pattern = "*.csv", full.names = TRUE)[1]
  a <- read.csv(df_nuevo, skip = 2) %>% 
    select(c(2,3,5)) %>% 
    rename_at(vars(1,2,3), ~c("date", "temp", "pp")) %>% 
    mutate(date = as.POSIXct(date, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC"))
  
  a1 <- a %>%
  mutate(date = as.Date(date)) %>%
  summarise(
    temp = mean(temp, na.rm = TRUE),
    pp   = sum(pp, na.rm = TRUE),
    .by  = date
  )
  
  a1 <- a1 %>% 
    mutate(date = as.POSIXct(paste(date, "01:00:00"),
                             format = "%Y-%m-%d %H:%M:%S" , tz = "UTC"),
           date = as.numeric(date)*1000)
  
  highchart() %>%
    hc_add_series(
      data = list_parse2(a1 %>% select(date, pp)), 
      type = "column", 
      name = "Precipitación",
      color = "#154c79",
      tooltip = list(valueDecimals = 1)) %>% 
    hc_add_series(
      data = list_parse2(a1 %>% select(date, temp)), 
      type = "line", 
      name = "Temperatura",
      color = "hotpink",
      tooltip = list(valueDecimals = 1)) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_title(
      text = "<b>Estación 02</b> - Precipitación diaria (mm)",
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
