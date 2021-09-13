df3<-
  df2 %>% 
  mutate(
    
    ptv = map(.x =df,
              .f = ~svyby(~MOV_PTV_e1, by=~INDICERIQUEZA + CRED, design =.x, FUN=svytotal, na.rm = T) %>% 
                
                mutate(vacuna = MOV_PTV_e1,vacunatipo = "PTV",
                       orden =as.numeric(as.factor(INDICERIQUEZA))) %>% 
                
                select(-MOV_PTV_e1) %>% 
                
                left_join(
                  
                  svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                    
                    data.frame() %>%
                    
                    rownames_to_column("INDICERIQUEZA") %>%
                    
                    mutate(
                      INDICERIQUEZA = substring(INDICERIQUEZA,14))) %>%
                
                select(INDICERIQUEZA,CRED,orden,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    
    neumo = map(.x =df,
                .f = ~svyby(~MOV_NEUMO_e1, by=~INDICERIQUEZA + CRED, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_NEUMO_e1,vacunatipo = "NEUMO",
                         orden =as.numeric(as.factor(INDICERIQUEZA))) %>% 
                  
                  select(-MOV_NEUMO_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("INDICERIQUEZA") %>%
                      
                      mutate(
                        INDICERIQUEZA = substring(INDICERIQUEZA,14))) %>%
                  
                  select(INDICERIQUEZA,CRED,orden,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    influ = map(.x =df,
                .f = ~svyby(~MOV_INFLU_e1, by=~INDICERIQUEZA + CRED, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_INFLU_e1,vacunatipo = "INFLU",
                         orden =as.numeric(as.factor(INDICERIQUEZA))) %>% 
                  
                  select(-MOV_INFLU_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("INDICERIQUEZA") %>%
                      
                      mutate(
                        INDICERIQUEZA = substring(INDICERIQUEZA,14))) %>%
                  
                  select(INDICERIQUEZA,CRED,orden,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    rota  = map(.x =df,
                .f = ~svyby(~MOV_ROTA_e1, by=~INDICERIQUEZA + CRED, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_ROTA_e1,vacunatipo = "ROTA",
                         orden =as.numeric(as.factor(INDICERIQUEZA))) %>% 
                  
                  select(-MOV_ROTA_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("INDICERIQUEZA") %>%
                      
                      mutate(
                        INDICERIQUEZA = substring(INDICERIQUEZA,14))) %>%
                  
                  select(INDICERIQUEZA,CRED,orden,total,vacuna,vacunatipo,se,-`SE`))) %>% 
  
  
  mutate(
    
    pretotal1  = map2(.x = ptv, .y = neumo, .f = ~bind_rows(.x,.y)),
    pretotal2  = map2(.x = influ, .y = rota, .f = ~bind_rows(.x,.y)),
    
    total  = map2(.x = pretotal1, .y = pretotal2, .f = ~bind_rows(.x,.y)),
    
    hr_si = map(.x = total,
             .f = ~h_prop(.x %>% dplyr::filter(CRED == "Si"))),
    
    hr_no = map(.x = total ,
                .f = ~h_prop(.x %>% dplyr::filter(CRED == "No"))),
    
    ssirii_si = map(.x = total ,
                 .f = ~sii_rii(.x %>% dplyr::filter(CRED == "Si"))),
    
    ssirii_no = map(.x = total ,
                 .f = ~sii_rii(.x %>% dplyr::filter(CRED == "No"))),
    
    
    graf_si = map(.x = hr_si,
               .f = ~gg(.x)),
    
    graf_no = map(.x = hr_no,
                  .f = ~gg(.x)))
  
  
  
df3 %>% select(ANIO,ssirii_no) %>% 
  unnest(cols = c(ssirii_no)) %>% 
  as.data.frame() %>% 
  ungroup() %>% 
  
  pivot_longer(cols = c(rii,ssi), names_to = "index") %>% 
  
  mutate(
    line = ifelse(index=="rii",1,0)
  ) %>% 
  
  ggplot() +
  geom_line(aes(x = as.factor(ANIO), y = value, col = vacunatipo, group = vacunatipo), size = 1.3) +
  #geom_point(aes(x = as.factor(ANIO), y = value, col = vacunatipo, group = vacunatipo)) +
  geom_hline(aes(yintercept = line), linetype = "dashed") + 
  
  scale_color_manual(values = c("#595260","#7ECA9C","#A799B7","#AD6C80")) +
  facet_wrap(~index, scales = "free", labeller = labeller(index = c("rii" = "RII",
                                                                    "ssi" = "SII"))) +
  xlab("Years")+
  ylab("Value")+
  #theme_minimal()+
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(face = "bold"),
    axis.text.x = element_text( size = 8),
    legend.position = "top",
    legend.title = element_blank(),
    axis.line = element_line(colour = "black", size = 1),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9)
  )


temp<-df3 %>% 
  select(ANIO,hr_si) %>% 
  unnest(cols = c(hr_si)) %>% 
  as.data.frame() %>% 
  group_by(as.factor(ANIO)) %>% 
  nest() %>% 
  mutate(grafico = map(.x = data,
                       .f = ~ggplot(data = .x, aes(x = INDICERIQUEZA, y = tasa, fill = as.factor(ANIO), group = as.factor(ANIO)))+
                         geom_col(fill = "#AA5674")+
                         geom_point()+
                         geom_smooth(method = "lm",se = F)+
                         xlab("Poverty quintile") +
                         ylab("Rate")+
                         scale_x_discrete(labels = c("1st","2nd","3rd","4th","5th"))+
                         theme(
                           axis.title = element_text(face ="bold", size = 8),
                           legend.text = element_text(size = 9, face = "bold"),
                           axis.text = element_text(face = "bold"),
                           axis.text.x = element_text( size = 8),
                           legend.position = "top",
                           legend.title = element_blank(),
                           axis.line = element_line(colour = "black", size = 1),
                           panel.background = element_blank(),
                           strip.background = element_blank(),
                           strip.text = element_text(face = "bold", size = 8)) +
                         facet_wrap(~vacunatipo, scales = "free")))


cowplot::plot_grid(plotlist = temp$grafico)
