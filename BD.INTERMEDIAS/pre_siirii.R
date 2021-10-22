ggplot(data = df3$ssirii[[1]] %>%
         left_join(region_shape) %>% 
         st_as_sf() %>% 
         #filter(ANIO%in%c(2010,2015,2019,2020)) %>% 
         mutate(
           siicat = ifelse(sii< -10,"< -10",
                           ifelse(sii>=-10&sii<=-5,"-10 to -5",
                                  ifelse(sii>=-5&sii<0,"-5 to 0",
                                         ifelse(sii>=0&sii<5,"0 to 5",
                                                ifelse(sii>=5&sii<10,"5 to 10",
                                                       ifelse(sii>10,"> 10",NA)))))),
           
           
           siicat2 = factor(siicat, levels = c("< -10","-10 to -5","-5 to 0",
                                               "0 to 5","5 to 10","> 10"))
           
         )) +
  geom_sf(alpha = 0.5, aes(fill=siicat2)) +

  scale_fill_manual(values = c("#531a23","#a63446","#e4c2c7","#b6cfde","#0c6291","#084465"))+
  
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(4, units = "mm"),
    keywidth = unit(20/length(labels), units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = 1,
    nrow = 1,
    byrow = T,
    label.position = "bottom"
  ))+
  
  labs(fill = "Slope Index of Inequality") +
  
  theme(axis.text.x = element_text(angle = 30, size = 6),
        strip.background = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 9, face = "bold"),
        legend.key.size = unit(0.5, 'cm'))
