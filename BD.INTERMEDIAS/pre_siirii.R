install.packages("sp")
library(sp)

plo<-coordinates(coords = region_shape$geometry)

t<-region_shape %>% 
  sf::st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  bind_cols(region_shape) %>% 
  filter(CAPITAL != "CALLAO") %>% 
  mutate(
    CAP = substr(DEPARTAMEN, start = 1, stop = 2),
    CAP = ifelse(DEPARTAMEN == "HUANCAVELICA","HV",
                 ifelse(DEPARTAMEN == "LA LIBERTAD","LL",CAP))
  )
t

