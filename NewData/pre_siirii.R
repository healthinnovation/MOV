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



#####################################################

sii_rii_map<-function(data) {
  
  
  bd <- data.frame(hr2 = c(0,100))
  
  
  data<-
    g$data[[2]] %>% 
    
    group_by(DEPARTAMEN,vacunatipo) %>% 
    
    mutate(
      tasa = (vacuna/total)*10000
    ) %>% 
    
    # ordenar de - a +
    arrange(orden,.by_group = T) %>% 
    
    # Calculo de hr
    mutate(pop= vacuna/sum(vacuna),
           
           popr = cumsum(pop)/sum(pop),
           pop.popr = popr - pop,
           hr2 = round((pop.popr + popr)/2,3)
    ) %>% 
    
    select(-pop,-popr,-pop.popr) %>% 
    
    
    #group_by(DEPARTAMEN,vacunatipo) %>% 
    
    nest() %>% 
    
    mutate(
      
      # lm para ssi 
      modelo = map(.x = data, 
                   .f = ~glm(formula = tasa~hr2, data = .x, family = poisson(), weights = total)),
      
      # extraer beta
      modelo2 = map(.x = modelo, 
                    .f = ~tidy(lmtest::coeftest(.x, vcov = vcovHC, 
                                                type = "HC3"), conf.int = T))
      
    ) %>% 
    
    #ungroup() %>% 
    
    unnest(cols = c(modelo2)) %>% 
    
    slice(2) %>%
    
    # rri f(1)/f(0)
    mutate(
      # rii = map_dbl(.x = modelo,
      #                    .f =~predict(.x,
      #                                 newdata = bd %>% filter(hr2 == 100))/predict(.x,newdata = bd %>% filter(hr2 == 0))),
           
           sii = estimate,
           sii_l = conf.low,
           sii_h = conf.high) %>% 
    
    # mutate(sii = round(sii,2),
    #        rii = round(rii,1)) 
  
  select(vacunatipo,term,sii,sii_l,sii_h)
  
  data
  
}

######################

g<-
  t %>% 
  unnest(cols = c("total")) %>% 
  group_by(ANIO,JUNTOS) %>% 
  nest() %>% 

  mutate(

    sii = map(.x = data,
              .f = ~h_prop.map(.x)),
    
    
    ssi2 = map(.x = data, 
               .f = ~sii_rii_map(.x))

    
  ) 


######################################


df4<-g %>% 
  select(ANIO,ssi2) %>% 
  unnest(cols = c(ssi2)) %>%
  filter(JUNTOS==0) %>% 
  
  
  left_join(region_shape, by = "DEPARTAMEN") %>% 
  #left_join(centroid) %>% 
  
  mutate(
    line = ifelse(p.value <0.05,"bold.italic","plain"),
    size = ifelse(p.value > 0.05,0.9,1)
  ) %>% 
  
  group_by(vacunatipo) %>% 
  
  st_as_sf()


df4<-
  df4 %>% 
  
  nest() %>% 
  
  mutate(
    
    grafico = map(.x = data,
                  .f = ~ggplot(data = .x %>%
                                 
                                 filter(ANIO%in%c(2010,2015,2019,2020)), aes( alpha =size)) +
                    
                    geom_sf(aes(fill=sii), size = 0.2) +
                    geom_text(aes(x = X, y = Y, label = CAP), size = 1.5)+
                    
                    
                    scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits = c(-6,6))+
                    
                    
                    guides(fill = guide_colourbar(barheight = 0.5, 
                                                  barwidth = 20,
                                                  title.position = "top",
                                                  direction = "horizontal"),
                           
                           alpha = "none")+
                    
                    facet_wrap(~ANIO, ncol = 4)+
                    
                    labs(fill = "Slope Index of Inequality (Income Quintile)") +
                    
                    theme(axis.title = element_blank(),
                          axis.text.x = element_text(angle = 30, size = 6),
                          strip.background = element_blank(),
                          panel.background = element_blank(),
                          strip.text = element_text(face = "bold"),
                          legend.position = "bottom",
                          legend.title = element_text(size = 9, face = "bold"),
                          legend.key.size = unit(0.5, 'cm'))
    ))
