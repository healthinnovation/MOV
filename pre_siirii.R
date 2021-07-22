df3<-
  df2 %>% 
  mutate(
    
    ptv = map(.x =df,
              .f = ~svyby(~MOV_PTV_e1, by=~DEPARTAMEN, design =.x, FUN=svytotal, na.rm = T) %>% 
                
                mutate(vacuna = MOV_PTV_e1,vacunatipo = "PTV") %>% 
                
                select(-MOV_PTV_e1) %>% 
                
                left_join(
                  
                  svytotal(~DEPARTAMEN, design = .x) %>%
                    
                    data.frame() %>%
                    
                    rownames_to_column("DEPARTAMEN") %>%
                    
                    mutate(
                      DEPARTAMEN = substring(DEPARTAMEN,11))) %>%
                
                select(DEPARTAMEN,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    
    neumo = map(.x =df,
                .f = ~svyby(~MOV_NEUMO_e1, by=~DEPARTAMEN, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_NEUMO_e1,vacunatipo = "NEUMO") %>% 
                  
                  select(-MOV_NEUMO_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~DEPARTAMEN, design = .x) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("DEPARTAMEN") %>%
                      
                      mutate(
                        DEPARTAMEN = substring(DEPARTAMEN,11))) %>%
                  
                  select(DEPARTAMEN,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    influ = map(.x =df,
                .f = ~svyby(~MOV_INFLU_e1, by=~DEPARTAMEN, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_INFLU_e1,vacunatipo = "INFLU") %>% 
                  
                  select(-MOV_INFLU_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~DEPARTAMEN, design = .x) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("DEPARTAMEN") %>%
                      
                      mutate(
                        DEPARTAMEN = substring(DEPARTAMEN,11))) %>%
                  
                  select(DEPARTAMEN,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    rota  = map(.x =df,
                .f = ~svyby(~MOV_ROTA_e1, by=~DEPARTAMEN, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_ROTA_e1,vacunatipo = "ROTA") %>% 
                  
                  select(-MOV_ROTA_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~DEPARTAMEN, design = .x) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("DEPARTAMEN") %>%
                      
                      mutate(
                        DEPARTAMEN = substring(DEPARTAMEN,11))) %>%
                  
                  select(DEPARTAMEN,total,vacuna,vacunatipo,se,-`SE`))) %>% 
  
  
  mutate(
    
    pretotal1  = map2(.x = ptv, .y = neumo, .f = ~bind_rows(.x,.y)),
    pretotal2  = map2(.x = influ, .y = rota, .f = ~bind_rows(.x,.y)),
    
    total  = map2(.x = pretotal1, .y = pretotal2, .f = ~bind_rows(.x,.y)),
    
    hr = map(.x = total,
             .f = ~h_prop(.x)),
    
    ssirii = map(.x = total,
                 .f = ~sii_rii(.x))
    
  ) %>% 
  
  select(ANIO,total,hr,ssirii)

              
df3

