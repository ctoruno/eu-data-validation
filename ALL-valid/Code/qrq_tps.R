qrq<-read_xlsx("./Inputs/QRQ Matches.xlsx")
tps<- read_csv("../TPS/TPS_data.csv")
tpsnames<-qrq%>%
  select(PREFIX, SUFFIX)%>%
  drop_na()%>%
  mutate(varname = paste0(PREFIX, SUFFIX))

vars<- sort(unique(tpsnames$varname))

qrqtps<-tps%>%
  select(country_name_ltn, country_code_nuts, all_of(vars))
write_xlsx(qrqtps, "QRQ_TPS.xlsx")
