data<-
  df3 %>% 
  select(ANIO,ssirii) %>% 
  unnest(cols = c(ssirii)) %>%
  #filter(vacunatipo%in%c("INFLU")) %>% 
  
  left_join(region_shape, by = "DEPARTAMEN") %>% 
  
  group_by(vacunatipo) %>% 
  
  st_as_sf()

names(data)
nb <- poly2nb(data, queen=TRUE)
