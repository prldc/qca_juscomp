library(QCA)
library(tidyverse)
library(readr)

# FILTRANDO E UNINDO OS DADOS
gc <- read_csv("gc.csv")
gc <- gc %>% select(-"2017")
gc <- gc %>% rename(COUNTRY = "Country Name", id = "Indicator Id", indicator = Indicator, "2017" = "2017-2018")
gc <- gc %>% filter(COUNTRY %in% c("Japan", "Canada", "United States", "Germany"), 
                    id %in% c(665, 669)) 
gc <- gc %>% select(COUNTRY, indicator, "2017")
gc <- gc %>% pivot_wider(names_from = "indicator", values_from = "2017")
gc <- gc %>% rename(CONF = "Public trust in politicians", RESULT = "Judicial independence (WEF)")

elec <- read_csv("elec_dock.csv")
dados <- gc %>% inner_join(elec, by = "COUNTRY")
write.csv(dados, "dados.csv", row.names = F)
dados <- read_csv("dados.csv")
# QCA
dados_cal <- dados %>% 
  mutate(CONF = 
  case_when(dados$CONF < 3 ~ 0,
            (dados$CONF > 3)&(dados$CONF < 5) ~ 1,
            T ~ 2)) %>%
  mutate(RESULT = 
  case_when(dados$RESULT < 3 ~ 0,
            (dados$RESULT > 3)&(dados$RESULT < 6) ~ 1,
                     T ~ 2)) %>%
  mutate(ELEC = 
           case_when(dados$ELEC > 0.5 ~ 0,
                     (dados$ELEC > 0.2)&(dados$ELEC < 0.5) ~ 1,
                     T ~ 2))

write.csv(dados_cal, "dados_cal.csv", row.names = F)
runGUI()
truthTable(dados_cal, outcome = "RESULT", conditions = "CONF, ELEC, DOCK",
           complete = TRUE, show.cases = TRUE)

