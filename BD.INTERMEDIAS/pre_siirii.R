library(spdep)

 data<-
  df3 %>% 
  
  select(ANIO,ssirii) %>% 
  unnest(cols = c(ssirii)) %>%
  #filter(ANIO == 2010 & vacunatipo == "PTV") %>%  
  
  left_join(region_shape, by = "DEPARTAMEN") %>% 
  
  group_by(ANIO,vacunatipo) %>%
  
  st_as_sf() %>% 
  
  nest() %>% 
    
    mutate(
      nb= map(.x = data,
               .f = ~poly2nb(.x, queen = T)),
      
      lw = map(.x = nb,
               .f = ~nb2listw(.x, style = "W", zero.policy = T)),
      
      
      moran = map2(.x = data, .y = lw,
                     .f = ~moran.test(.x$sii,.y,alternative = "greater")),
      
      moran.mc = map2(.x = data, .y = lw,
                      .f = ~moran.mc(.x$sii,.y, nsim = 9999, alternative = "greater")),
      
      
      moran.t = map_dbl(.x = moran,
                        .f = ~.x$estimate[[1]]),
      
      moran.p = map_dbl(.x = moran,
                        .f = ~.x$p.value)
    )
 
data %>% 
  select(ANIO,vacunatipo,moran.t,moran.p) %>% 
  mutate(
    alpha = ifelse(moran.p>0.05,0.2,1)
  ) %>% 

  ggplot(aes(x = as.factor(ANIO), y = vacunatipo, fill = moran.t, alpha = alpha))+
  geom_tile()+
  scale_x_discrete(position = "top") +
  scale_fill_gradient(high = "#88A5B4")+
  geom_text(aes(label = formatC(round(moran.t, 2), format = "f", digits = 2)))+
  labs(x = "Years", y = NULL) +
  guides(alpha = "none")+
  theme_minimal()+
  theme(
    panel.grid = element_blank()
  )
  

   
  
 


