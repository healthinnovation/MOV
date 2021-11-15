MOV_GENERAL <- read.csv("./BD.INTERMEDIAS/mov_general.csv")

df2<-MOV_GENERAL %>% 
  
  filter((CRED == "Si" | SEGURO == "Si")) %>% 
  filter(ANIO !=2013 & !is.na(JUNTOS)) %>% 
  
  group_by(ANIO,JUNTOS) %>% 
  nest() %>%
  mutate(
    df= map(.x = data, 
            .f = ~svydesign(id =~ V001, strata =~ V022, weights=~V005, data=.x))) 

options(survey.lonely.psu="remove")


t<- df2 %>% 
  mutate(
    
    ptv = map(.x =df,
              .f = ~svyby(~MOV_PTV_e1, by=~INDICERIQUEZA+DEPARTAMEN, design =.x, FUN=svytotal, na.rm = T) %>% 
                
                mutate(vacuna = MOV_PTV_e1,vacunatipo = "PTV",
                       orden =as.numeric(as.factor(INDICERIQUEZA))) %>% 
                
                select(-MOV_PTV_e1) %>% 
                
                left_join(
                  
                  svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                    
                    data.frame() %>%
                    
                    rownames_to_column("INDICERIQUEZA") %>%
                    
                    mutate(
                      INDICERIQUEZA = substring(INDICERIQUEZA,14))) %>%
                
                select(DEPARTAMEN,INDICERIQUEZA,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    neumo = map(.x =df,
                .f = ~svyby(~MOV_NEUMO_e1, by=~INDICERIQUEZA+DEPARTAMEN, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_NEUMO_e1,vacunatipo = "NEUMO",
                         orden =as.numeric(as.factor(INDICERIQUEZA))) %>% 
                  
                  select(-MOV_NEUMO_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("INDICERIQUEZA") %>%
                      
                      mutate(
                        INDICERIQUEZA = substring(INDICERIQUEZA,14))) %>%
                  
                  select(DEPARTAMEN,INDICERIQUEZA,orden,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    
    rota = map(.x =df,
               .f = ~svyby(~MOV_ROTA_e1, by=~INDICERIQUEZA+DEPARTAMEN, design =.x, FUN=svytotal, na.rm = T) %>% 
                 
                 mutate(vacuna = MOV_ROTA_e1,vacunatipo = "ROTA",
                        orden =as.numeric(as.factor(INDICERIQUEZA))) %>% 
                 
                 select(-MOV_ROTA_e1) %>% 
                 
                 left_join(
                   
                   svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                     
                     data.frame() %>%
                     
                     rownames_to_column("INDICERIQUEZA") %>%
                     
                     mutate(
                       INDICERIQUEZA = substring(INDICERIQUEZA,14))) %>%
                 
                 select(DEPARTAMEN,INDICERIQUEZA,orden,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    influ = map(.x =df,
                .f = ~svyby(~MOV_INFLU_e1, by=~INDICERIQUEZA+DEPARTAMEN, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_INFLU_e1,vacunatipo = "INFLU",
                         orden =as.numeric(as.factor(INDICERIQUEZA))) %>% 
                  
                  select(-MOV_INFLU_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("INDICERIQUEZA") %>%
                      
                      mutate(
                        INDICERIQUEZA = substring(INDICERIQUEZA,14))) %>%
                  
                  select(DEPARTAMEN,INDICERIQUEZA,total,orden,vacuna,vacunatipo,se,-`SE`))
    
  ) %>% 
  mutate(
    pretotal1  = map2(.x = ptv, .y = neumo, .f = ~bind_rows(.x,.y)),
    pretotal2  = map2(.x = influ, .y = rota, .f = ~bind_rows(.x,.y)),
    
    total  = map2(.x = pretotal1, .y = pretotal2, .f = ~bind_rows(.x,.y)))



#####################################################3

g<-
  t %>% 

  mutate(
    hr = map(.x = total,
             .f = ~h_prop.map(.x)),
    
   
    sii = map(.x = total,
              .f = ~sii_rii_map(.x))
  ) 
