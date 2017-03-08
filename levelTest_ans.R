# test1:

y<- c(1,0,0,1,1,1,0,1,1)
#which(head(y,-1) == tail(y,-1) & head(y,-1)==1)
findNum <- function(y, x, l){
  start <- which(y == x)
  end <- start + l - 1
  if (sum(end > length(y))>0){
    start <- start[-which(end > length(y))]
    end <- end[-which(end > length(y))]
    }
  win <- rbind(start,end)
  unlist(apply(win, 2, FUN = function(x){
    if (length(unique(y[x[1]:x[2]])) == 1 ){
      return(x[1])
    }
    }
    )
    )
}

findNum(y, 1, 2)

# time: 7 min


# test2:
raw <- read.delim("data/weather.txt",check.names = F, na.strings = ".")
library(dplyr)
raw_n <- melt(raw, id.vars = names(raw)[1:3],variable.name = "day") %>% 
  dcast(., year + month + day ~ element) %>%
  mutate(., diff = .[,4]- .[,5]) #%>%
  #na.omit(.)

# time: 4 min

# test3：
library(hflights)
str(hflights)
group_by(hflights, UniqueCarrier, Month) %>% 
  summarise(., quan = quantile(ArrDelay,0.1, na.rm=T)) %>%
  group_by(., UniqueCarrier) %>%
  summarise(., mean(quan))

# time: 4 min