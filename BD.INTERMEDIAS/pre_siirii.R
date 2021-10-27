t<-data %>% 
  filter(ANIO!=2013) %>% 
  mutate(
    
    ptv = map(.x =df,
              .f = ~svyby(~MOV_PTV_e1, by=~INDICERIQUEZA + JUNTOS, design =.x, FUN=svytotal, na.rm = T) %>% 
                
                mutate(vacuna = MOV_PTV_e1,vacunatipo = "PTV",
                       orden =as.numeric(as.factor(INDICERIQUEZA))) %>% 
                
                select(-MOV_PTV_e1) %>% 
                
                left_join(
                  
                  svyby(~INDICERIQUEZA, by=~JUNTOS, design = .x, FUN = svytotal, na.rm.all = T) %>%
                    
                    pivot_longer(cols = c(INDICERIQUEZA1ro:INDICERIQUEZA5to), names_to = "INDICERIQUEZA") %>% 
                    group_by(JUNTOS,INDICERIQUEZA) %>% 
                    slice(1) %>% 
                    select(JUNTOS,INDICERIQUEZA,value)) %>%
                
                select(INDICERIQUEZA,JUNTOS,orden,vacuna,vacunatipo,se)))

t$ptv[[1]]

data<-MOV_GENERAL %>% 
  filter(JUNTOS == 1) %>% 
  group_by(ANIO) %>% 
  nest() %>%
  mutate(
    df= map(.x = data, 
            .f = ~svydesign(id =~ V001, strata =~ V022, weights=~V005, data=.x))) 




t<-
  data %>% 
  mutate(
    pvt = map(.x = df,
              .f = ~svyby(~INDICERIQUEZA, by=~JUNTOS, design = .x, FUN = svytotal, na.rm.all = T))
  )

