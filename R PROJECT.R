library(tidyverse)
view(nyp_data)
nyp_data
tidydata <- nyp_data

ggplot(data=nyp_data)+
geom_point(mapping = aes(x = APR_DRG_Code, y= Mean_Charge, color = Facility_Name))

ggplot(data=nyp_data)+
  geom_point(mapping= aes(x=Mean_Cost, y = Mean_Charge, alpha = Discharges))
             
