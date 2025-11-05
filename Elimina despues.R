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
    mutate(pp = if_else(pp >=5, 0, pp)) %>%
    distinct()
  df2
}
df1 <- estacion1(df_nuevo = "datasets/acumulado/Victoriano_estación.csv")

dff <- df1 %>% mutate(date = format(date, format = "%Y-%m-%d %H:%M"))

write.table(dff, "datasets/acumulado/estacion1_acumulado.csv", row.names = FALSE, sep = ",")

xd <- read.csv("datasets/acumulado/estacion1_acumulado.csv")
xd2 <- read.csv("datasets/acumulado/Victoriano_estación.csv")
source("Scripts/Estacion1.R")
estacion1(carpeta = "datasets/ASA_01_PO_01")
# Estación 2 ----

est2 <- read.csv("datasets/ASA_01_PO_02/P02_0.csv", skip = 2) %>%
  select(c(2,3,5)) %>% 
    rename_at(vars(1,2,3), ~c("date", "temp", "pp")) %>% 
    mutate(date = as.POSIXct(date, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")) %>%
  mutate(date = format(date, format = "%Y-%m-%d %H:%M"))

write.table(est2, "datasets/acumulado/estacion2_acumulado.csv", row.names = FALSE, sep = ",")

est22 <- read.csv("datasets/acumulado/estacion2_acumulado.csv")

source("Scripts/Estacion2.R")
qocha(carpeta = "datasets/ASA_01_PO_02")

# Estación 3 ---


df <- read_excel("datasets/ASA_01_PO_03/AG3-3477.xlsx",
 sheet = "AG3-3477") %>%
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
  ) %>% select(date, temp, pp) %>%
  mutate(date = format(date, format = "%Y-%m-%d %H:%M"))


write.table(df, "datasets/acumulado/estacion3_acumulado.csv", row.names = FALSE, sep = ",")

df2 <- read.csv("datasets/acumulado/estacion3_acumulado.csv")

source("Scripts/Estacion3.R")
pesaje(carpeta = "datasets/ASA_01_PO_03")

# Estación de nivel:

df <- read.csv("datasets/ASA_02_SO/PROCESADO_ASA_01_SO_14_10_25.csv", skip = 2, header = F) %>%
  select(2,6) %>% mutate(V2 = as.POSIXct(V2, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC"))
names(df) <- c("date", "nivel")

df <- df %>% mutate(date = format(date, format = "%Y-%m-%d %H:%M"))
write.table(df, "datasets/acumulado/estacion4_acumulado.csv", row.names = FALSE, sep = ",")
df <- read.csv("datasets/acumulado/estacion4_acumulado.csv")

df2 <- df %>%
    mutate(dia = as.Date(date, format = "%Y-%m-%d")) %>%
    summarise(hidrometria_punto1 = mean(hidrometria_punto1, na.rm = T), .by = dia)

df2 <- df2 %>%
    hchart(., type = "column", 
           hcaes(x = dia, 
                 y = hidrometria_punto1), color = "#154c79", name = "Altura") %>% 
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

source("Scripts/Estacion4.R")
bofedal("datasets/ASA_02_SO")
