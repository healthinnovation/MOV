df4<-df3 %>% 
  select(ANIO,ssirii) %>% 
  unnest(cols = c(ssirii)) %>%
  
  
  left_join(region_shape, by = "DEPARTAMEN") %>% 
  
  group_by(vacunatipo)


df4<- bi_class(.data = df4, x = sii, y = p.value, dim = 3)
  
  df4<-
    df4 %>% 
    st_as_sf() %>%
  
  nest() %>% 
  
  mutate(
    
    grafico = map(.x = data,
                  .f = ~ggplot(data = .x %>%
                                 
                                 filter(ANIO%in%c(2010,2015,2019,2020)) %>% 
                                 mutate(
                                   siicat = ifelse(sii< -10,"< -10",
                                                   ifelse(sii>=-10&sii<=-5,"-10 to -5",
                                                          ifelse(sii>=-5&sii<0,"-5 to 0",
                                                                 ifelse(sii>=0&sii<5,"0 to 5",
                                                                        ifelse(sii>=5&sii<10,"5 to 10",
                                                                               ifelse(sii>10,"> 10",NA)))))),
                                   
                                   
                                   siicat2 = factor(siicat, levels = c("< -10","-10 to -5","-5 to 0",
                                                                       "0 to 5","5 to 10","> 10")),
                                   
                                   
                                  
                                   
                                 )) +
                    geom_sf(aes(fill=bi_class), size = 0.2) +
                    geom_text(data=centroid, aes(x = X, y = Y, label = CAP), size = 1.5)+
                    
                    
                    #scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits = c(-6,6))+
                    
                    bi_scale_fill(pal = "DkViolet", dim = 3) +
                    #guides(fill = guide_colourbar(barheight = 0.5, barwidth = 20,title.position = "top", direction = "horizontal"),
                     #      alpha = "none")+
                    
                    facet_wrap(~ANIO, nrow = 1)+
                    
                    labs(fill = "Slope Index of Inequality") +
                    
                    theme(axis.title = element_blank(),
                          axis.text.x = element_text(angle = 30, size = 6),
                          strip.background = element_blank(),
                          panel.background = element_blank(),
                          strip.text = element_text(face = "bold"),
                          legend.position = "bottom",
                          legend.title = element_text(size = 9, face = "bold"),
                          legend.key.size = unit(0.5, 'cm'))
    ))



g1<-plot_grid(df4$grafico[[1]] + theme(legend.position = "none"))
g2<-plot_grid(df4$grafico[[2]] + theme(legend.position = "none"))
g3<-plot_grid(df4$grafico[[3]] + theme(legend.position = "none"))
g4<-plot_grid(df4$grafico[[4]] + theme(legend.position = "none"))

legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "SII ",
                    ylab = "p.value",
                    size = 8)

plot_grid(g1,g2,g3,g4,legend, ncol = 1, labels = c("A","B","C","D"), rel_heights = c(.8,.8,.8,.8,.2))


ggsave("Fig6.png",width = 10,height = 15)
