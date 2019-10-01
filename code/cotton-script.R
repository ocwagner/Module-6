########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
read_csv("data/cotton-usda-nass.csv")->cotton_data
head(cotton_data)
str(cotton_data)
tail(cotton_data)
dim(cotton_data)
summary(cotton_data)

# 3.1. Create a NC data subset ----
cotton_data[c("year", "state", "ag_district", "county", "data_item", "value")]->clean_cotton_data
head(clean_cotton_data)
clean_cotton_data%>%
  filter(state=="NORTH CAROLINA") ->NC_cotton
NC_cotton

# 3.2. Divide the data_item column ----
NC_cotton%>%
  separate(data_item,into=c("cotton_type", "measurement"), sep = " - ")->NC_divided_cotton
  

# 3.3. Convert the value column to numeric type ----
NC_divided_cotton%>%
  subset(value!="(D)")->NC_disclosed

NC_disclosed$value <- as.numeric(NC_disclosed$value)
str(NC_disclosed)
head(NC_disclosed)

# 4. Visualizing trends ----
NC_disclosed%>%
  ggplot(mapping=aes(x=year, y=value))+
  geom_point()+
  theme_minimal()+
  facet_grid(measurement ~ ag_district, scales = "free_y")+
  theme(axis.text.x=element_text(angle=90))

# 5. Summarize data from 2018 ----
head(NC_disclosed)

###Need to create an identified row becuase each row must be identified by a unique combination of keys
NC_disclosed%>%
   group_by_at(vars(-value)) %>% 
   mutate(row_id=1:n()) %>% ungroup() %>% 
   spread(key=measurement, value=value) %>%
   select(-row_id)->NC_spread
head(NC_spread)  
NC_spread%>%
  mutate("PRODUCTION_LBS"=`ACRES HARVESTED`* `YIELD, MEASURED IN LB / ACRE`)->NC_production
head(NC_production)

### looking just at 2018 data
subset(NC_production,year==2018)->NC_2018


### top 3 in terms of production
top_n(NC_2018,3,PRODUCTION_LBS)

         