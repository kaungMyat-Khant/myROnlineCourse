sr <- sf::st_read(dsn = "C:/docs/Power BI Training Materials/04-SpatialData_to_Use/state_region.json")  
nrow(sr)
set.seed(123)
sr$temp <- as.double(sample(20:45, nrow(sr)))
sr$mmr <- as.integer(sample(100:800, nrow(sr)))
library(ggplot2)

ggplot(sr) +
  geom_sf(aes(fill = temp))+
  scale_fill_gradient(low = "yellow", high = "red")+theme_void()


