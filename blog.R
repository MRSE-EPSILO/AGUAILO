library(tidyverse)
a <- read.csv("datasets/Victoriano_estación.csv") %>% 
  rename_at(vars(1, 2), ~c("date", "pp")) %>% 
  mutate(date = as.POSIXct(date, format = "%Y/%m/%d %H:%M"))



b <- read.csv("datasets/Qocha_estación.csv") %>% 
  select(c(1,2,4)) %>% 
  rename_at(vars(1,2,3), ~c("date", "temp", "pp")) %>% 
  mutate(date = as.POSIXct(date, format = "%Y/%m/%d %H:%M"))


b1 <- b %>% 
  mutate(date = as.Date(date)) %>% 
  summarise(temp = mean(temp, na.rm = TRUE), .by = "date")


b1$pp <- b %>% filter(!is.na(pp)) %>% select(pp) %>% pull()


c <- read.csv("datasets/Preciball.csv", skip = 1, header = F) %>% 
  select(c(1,3,7)) %>% 
  rename_at(vars(1,2,3), ~c("date", "pp", "temp")) %>% 
  mutate(date = as.POSIXct(date, format = "%d/%m/%Y %H:%M"))

c$date <- seq(from = as.POSIXct("2025-01-10 12:20:00"),
              to = as.POSIXct("2025-01-21 09:40:00"), by = "5 mins")


c <- c %>% mutate(date = as.Date(date)) %>% 
  summarise(temp = mean(temp, na.rm = TRUE),
            pp = sum(pp, na.rm = TRUE),.by = "date")



