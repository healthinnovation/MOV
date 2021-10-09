t<-
  df2 %>% 
  mutate(
    
    ptv = map(.x =df,
              .f = ~svyby(~MOV_PTV_e1, by=~INDICERIQUEZA, design =.x, FUN=svytotal, na.rm = T) %>% 
                
                mutate(vacuna = MOV_PTV_e1,vacunatipo = "PTV",
                       orden =as.numeric(as.factor(INDICERIQUEZA)),
                       
                       vacuna_ll = MOV_PTV_e1 - (1.94 * se),
                       vacuna_hl = MOV_PTV_e1 + (1.94 * se)) %>% 
                
                select(-MOV_PTV_e1) %>% 
                
                left_join(
                  
                  svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                    
                    data.frame() %>%
                    
                    rownames_to_column("INDICERIQUEZA") %>%
                    
                    mutate(
                      INDICERIQUEZA = substring(INDICERIQUEZA,14),
                      total_ll = total - (1.94 * SE),
                      total_hl = total + (1.94 * SE))) %>%
                
                select(INDICERIQUEZA,orden,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    
    neumo = map(.x =df,
                .f = ~svyby(~MOV_NEUMO_e1, by=~INDICERIQUEZA, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_NEUMO_e1,vacunatipo = "PTV",
                         orden =as.numeric(as.factor(INDICERIQUEZA)),
                         
                         vacuna_ll = MOV_NEUMO_e1 - (1.94 * se),
                         vacuna_hl = MOV_NEUMO_e1 + (1.94 * se)) %>% 
                  
                  select(-MOV_NEUMO_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("INDICERIQUEZA") %>%
                      
                      mutate(
                        INDICERIQUEZA = substring(INDICERIQUEZA,14),
                        total_ll = total - (1.94 * SE),
                        total_hl = total + (1.94 * SE))) %>%
                  
                  select(INDICERIQUEZA,orden,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    influ = map(.x =df,
                .f = ~svyby(~MOV_INFLU_e1, by=~INDICERIQUEZA, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_INFLU_e1,vacunatipo = "PTV",
                         orden =as.numeric(as.factor(INDICERIQUEZA)),
                         
                         vacuna_ll = MOV_INFLU_e1 - (1.94 * se),
                         vacuna_hl = MOV_INFLU_e1 + (1.94 * se)) %>% 
                  
                  select(-MOV_INFLU_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("INDICERIQUEZA") %>%
                      
                      mutate(
                        INDICERIQUEZA = substring(INDICERIQUEZA,14),
                        total_ll = total - (1.94 * SE),
                        total_hl = total + (1.94 * SE))) %>%
                  
                  select(INDICERIQUEZA,orden,total,vacuna,vacunatipo,se,-`SE`)),
    
    
    rota  = map(.x =df,
                .f = ~svyby(~MOV_ROTA_e1, by=~INDICERIQUEZA, design =.x, FUN=svytotal, na.rm = T) %>% 
                  
                  mutate(vacuna = MOV_ROTA_e1,vacunatipo = "PTV",
                         orden =as.numeric(as.factor(INDICERIQUEZA)),
                         
                         vacuna_ll = MOV_ROTA_e1 - (1.94 * se),
                         vacuna_hl = MOV_ROTA_e1 + (1.94 * se)) %>% 
                  
                  select(-MOV_ROTA_e1) %>% 
                  
                  left_join(
                    
                    svytotal(~INDICERIQUEZA, design = .x, na.rm = T) %>%
                      
                      data.frame() %>%
                      
                      rownames_to_column("INDICERIQUEZA") %>%
                      
                      mutate(
                        INDICERIQUEZA = substring(INDICERIQUEZA,14),
                        total_ll = total - (1.94 * SE),
                        total_hl = total + (1.94 * SE))) %>%
                  
                  select(INDICERIQUEZA,orden,total,vacuna,vacunatipo,se,-`SE`))) %>% 
  
  
  mutate(
    
    total = map(.x = ptv,
                .f = ~bind_rows(.x,neumo,influ,rota)),
    
    # hr_l = map(.x = total,
    #          .f = ~h_prop(.x, level = 1)),
    # 
    # hr_h = map(.x = total,
    #          .f = ~h_prop(.x, level = 2)),
    
    hr = map(.x = total,
             .f = ~h_prop(.x, 3)),
    
    
    ssirii = map(.x = total,
                 .f = ~sii_rii(.x, level = 3)),
    
    ssirii_l = map(.x = total,
                   .f = ~sii_rii(.x, level = 1)),
    
    ssirii_h = map(.x = total,
                   .f = ~sii_rii(.x, level = 2)),
    
    graf = map(.x = hr,
               .f = ~gg(.x))
    
  ) 



  

t %>% select(ANIO,ssirii,ssirii_l,ssirii_h) %>% 
  unnest() %>% 
  as.data.frame() %>% 
  ungroup() %>%
  
  ggplot(aes(x = as.factor(ANIO), y = ssi, group = vacunatipo)) +
  
  geom_line(aes(color = vacunatipo), size = 1.2) +
  geom_ribbon(aes(ymin = ssi_l, ymax = ssi_h, fill = vacunatipo), alpha = 0.1)+
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  
  #scale_color_manual(values = c("#595260","#7ECA9C","#A799B7","#AD6C80")) +
  scale_color_npg()+
  scale_fill_npg()+
  
  xlab("Years")+
  ylab("SII")+
  scale_x_discrete(expand = c(0, 0.15))+
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

  ggsave("ssi.png",width = 10, height = 6)
