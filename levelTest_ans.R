y<- c(1,0,0,1,1,1,0,1,1)
which(head(y,-1) == tail(y,-1) & head(y,-1)==1)
# time: 2min

raw <- read.delim("data/weather.txt",check.names = F, na.strings = ".")
library(dplyr)
raw_n <- melt(raw, id.vars = names(raw)[1:3],variable.name = "day") %>% 
  dcast(., year + month + day ~ element) %>%
  mutate(., diff = .[,4]- .[,5]) #%>%
  #na.omit(.)

# time: 4min

library(hflights)
str(hflights)
group_by(hflights, UniqueCarrier, Month) %>% 
  summarise(., quan = quantile(ArrDelay,0.1, na.rm=T)) %>%
  group_by(., UniqueCarrier) %>%
  summarise(., mean(quan))

# time: 4min
