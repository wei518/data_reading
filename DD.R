library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(knitr)
library(readr)

DD=read_csv('D:/Traffic/bus/group_R.csv')
DD1=data.frame(Time=as.Date(DD$time, format="%Y/%m/%d"),
               treatment=DD$R52+DD$`818`+DD$`983`,
               control=DD$'870'+DD$'R51'+DD$R38+DD$`894`+DD$R23+DD$`881`)


DD1 %>% 
  gather(STATE,volumn,treatment,control,-Time) -> test #time放前面

t1=test %>% 
  filter(Time <= "2018-12-21")
t2=test %>% 
  filter(Time >= "2019-04-30")

test %>% 
  ggplot() +
  geom_point(data=t1,aes(x=Time,y=volumn,group=STATE,color=STATE))+
  geom_point(data=t2,aes(x=Time,y=volumn,group=STATE,color=STATE))+
  geom_line(data=t1,aes(x=Time,y=volumn,group=STATE,color=STATE))+
  geom_line(data=t2,aes(x=Time,y=volumn,group=STATE,color=STATE))+
  scale_x_date(date_labels = '%y/%m',date_breaks = '1 months')


test %>%
  mutate(
    STATE1=(STATE=="treatment"),
    AFTER=(Time>="2018-12-23"),
    PolicyImpact=STATE1*AFTER
  ) -> test2

lm(volumn~STATE1+AFTER+PolicyImpact,data=test2)->DD_result
summary(DD_result)

