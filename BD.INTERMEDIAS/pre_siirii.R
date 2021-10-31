
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



data<-MOV_GENERAL %>% 
  group_by(ANIO,TIPORESIDENCIA) %>% 
  nest() %>%
  mutate(
    df= map(.x = data, 
            .f = ~svydesign(id =~ V001, strata =~ V022, weights=~V005, data=.x))) 


t<-df3 %>% 
  mutate(
    hist = map(.x = hr,
               .f = ~ggplot(data = .x, aes(tasa))+
                 geom_histogram()+
                 facet_wrap(~vacunatipo)))


plot_grid(plotlist = t$hist)
ggsave("imagen.png",width = 17, height = 10)

df2
plot_grid(g1,g2,g3,g4,legend, ncol = 2, labels = c("A","B","C","D"), rel_heights = c(.8,.8,.15))
ggsave("SFig3.png",width = 13,height = 6)
plot_grid(g1,g2,g3,g4,legend, ncol = 2, labels = c("A","B","C","D"), rel_heights = c(.8,.8,.15))
ggsave("SFig4.png",width = 13,height = 6)
