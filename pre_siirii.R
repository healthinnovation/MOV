df3<-
  df2 %>% 
  mutate(
    
    ptv = map(.x =df,
              .f = ~svyby(~MOV_PTV_e1, by=~EDU_MADRE, design =.x, FUN=svytotal, na.rm = T) %>% 
                
                mutate(vacuna = MOV_PTV_e1,vacunatipo = "PTV") %>% 
                
                select(-MOV_PTV_e1) %>% 
                
                left_join(
                  
                  svytotal(~EDU_MADRE, design = .x) %>%
                    
                    data.frame() %>%
                    
                    rownames_to_column("EDU_MADRE") %>%
                    
                    mutate(
                      EDU_MADRE = substring(EDU_MADRE,10))) %>%
                
                select(EDU_MADRE,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    
    neumo = map(.x =df,
                .f = ~svyby(~MOV_NEUMO_e1, by=~EDU_MADRE, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_NEUMO_e1,vacunatipo = "NEUMO") %>% 
                  
                  select(-MOV_NEUMO_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~EDU_MADRE, design = .x) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("EDU_MADRE") %>%
                      
                      mutate(
                        EDU_MADRE = substring(EDU_MADRE,10))) %>%
                  
                  select(EDU_MADRE,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    influ = map(.x =df,
                .f = ~svyby(~MOV_INFLU_e1, by=~EDU_MADRE, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_INFLU_e1,vacunatipo = "INFLU") %>% 
                  
                  select(-MOV_INFLU_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~EDU_MADRE, design = .x) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("EDU_MADRE") %>%
                      
                      mutate(
                        EDU_MADRE = substring(EDU_MADRE,10))) %>%
                  
                  select(EDU_MADRE,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    rota  = map(.x =df,
                .f = ~svyby(~MOV_ROTA_e1, by=~EDU_MADRE, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_ROTA_e1,vacunatipo = "ROTA") %>% 
                  
                  select(-MOV_ROTA_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~EDU_MADRE, design = .x) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("EDU_MADRE") %>%
                      
                      mutate(
                        CREDLUGAR = substring(EDU_MADRE,10))) %>%
                  
                  select(EDU_MADRE,total,vacuna,vacunatipo,se,-`SE`))) %>% 
  
  
  mutate(
    
    pretotal1  = map2(.x = ptv, .y = neumo, .f = ~bind_rows(.x,.y)),
    pretotal2  = map2(.x = influ, .y = rota, .f = ~bind_rows(.x,.y)),
    
    total  = map2(.x = pretotal1, .y = pretotal2, .f = ~bind_rows(.x,.y)),
    
    hr = map(.x = total,
             .f = ~h_prop(.x)),
    
    ssirii = map(.x = total,
                 .f = ~sii_rii(.x)),
    
    
    graf = map(.x = hr,
               .f = ~gg(.x))
    
  ) 

# SII y RII por año (2010 - 2020)
sii_rii.t <-
  df3 %>% select(ANIO,ssirii) %>% 
  unnest(cols = c(ssirii)) %>% 
  as.data.frame() %>% 
  ungroup()

# Grafico: tasa vs HR
ggsave("rr.png",cowplot::plot_grid(plotlist = df3$graf), width = 18, height = 15)


# sii/rii por año: grafico

ggplot(sii_rii.t) +
  
  geom_line(aes(x = as.factor(ANIO), y = ssi, col = vacunatipo, group = vacunatipo)) 


df3$hr[[1]]
