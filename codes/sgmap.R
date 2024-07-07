sr <- sf::st_read(dsn = "C:/docs/Power BI Training Materials/04-SpatialData_to_Use/state_region.json")  
nrow(sr)
library(ggplot2)
sr$sagaing <- ifelse(sr$ST=="Sagaing","Sagaing","")
ggplot(sr) +
  geom_sf(aes(fill = sagaing))+
  scale_fill_manual(values = c("white","#F0DDCC"))+
  theme_void()+
  theme(
    legend.position = "")

# + 
#   geom_sf_text(aes(label = sagaing), size = 2.5, hjust =1, vjust = -1 )+
#   labs(title = "Study area is Sagaing Region in Myanmar")+
#   theme(plot.title = element_text(size = 8))
  
