# Estación de nivel -------------------------------------------------

bofedal <- function(carpeta){
  df_nuevo <- list.files(path = carpeta, pattern = "*.csv", full.names = TRUE)[1]

  a <- read.csv("datasets/acumulado/estacion4_acumulado.csv") %>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M", tz = "UTC"))

  b <- read.csv(df_nuevo, skip = 2, header = F) %>%
  select(2,6) %>% mutate(V2 = as.POSIXct(V2, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC"))
  names(b) <- c("date", "nivel")

  a <- rbind(a,b) %>% distinct()
  write.table(a %>%
    mutate(date = format(date, format = "%Y-%m-%d %H:%M")),
   "datasets/acumulado/estacion4_acumulado.csv",
    row.names = FALSE, sep = ",")
  a <- a %>% mutate(Día = as.Date(date, format = "%Y-%m-%d")) %>%
    summarise(Nivel = mean(nivel, na.rm = T), .by = Día)

  df2 <- a %>%
    hchart(., type = "column", 
           hcaes(x = Día, 
                 y = Nivel), color = "#154c79", name = "Altura") %>% 
    hc_title(
      text = "<b>Estación 04</b> - Nivel (pies)",
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