coverage2<-
  coverage1 %>% 
  ggplot(aes(x = as.factor(ANIO), y = vacunatipo, fill = vacuna))+
  geom_tile(alpha = 0.9)+
  scale_fill_gradient2(high  ="#0C6291",low = "#A63446") + 
  geom_text(aes(label = formatC(estimate, format = "f", digits = 2)), size = 3)+
  theme_minimal()+
  labs(fill = "", y = "Vaccine coverage")+
  theme(
    
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_blank(),
    legend.position = "none")

#ggsave("Fig1-Supplementaltable.png",width = 10, height = 4, dpi = 300)  

mov2<-
  mov %>% 
  ggplot(aes(x = as.factor(ANIO), y = vacunatipo, fill = vacuna))+
  geom_tile(alpha = 0.9)+
  scale_fill_gradient2(high  ="#0C6291",low = "#91a5b1") + 
  geom_text(aes(label = formatC(estimate, format = "f", digits = 2)), size = 3)+
  theme_minimal()+
  labs(fill = "", y = "Missed Oportunities for Vaccination")+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_blank(),
    legend.position = "none")


#ggsave("Fig3-Supplementaltable.png",width = 10, height = 4, dpi = 300) 

edu2<-
  edu1 %>%
  ggplot(aes(x = as.factor(ANIO), y = vacunatipo, fill = ssi))+
  geom_tile()+
  scale_fill_gradient2(high  ="#0C6291",low = "#A63446", midpoint = 0) + 
  geom_text(aes(label = formatC(estimate, format = "f", digits = 2)), size = 3)+
  labs(fill = "", y = "SII - Maternal education")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_blank(),
    legend.position = "none")

#ggsave("Fig-Supplementaltable.png",width = 14, height = 4, dpi = 300)

wi2<-
  wi %>%
  ggplot(aes(x = as.factor(ANIO), y = vacunatipo, fill = ssi))+
  geom_tile()+
  scale_fill_gradient2(high  ="#0C6291",low = "#A63446", midpoint = 0) + 
  geom_text(aes(label = formatC(estimate, format = "f", digits = 2)), size = 3)+
  labs(fill = "", y = "SII - Wealth Index")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text = element_text(face = "bold"),
    legend.position = "none")

#ggsave("Figb-Supplementaltable.png",width = 14, height = 4, dpi = 300)

###################3

library(patchwork)

a<-plot_grid(coverage2,mov2, ncol = 1)
b<-plot_grid(edu2,wi2, ncol = 1)

plot_grid(a,b, ncol = 1)
ggsave("Stable2.pdf",dpi = 300, width = 15, height = 11)
ggsave("Stable2.png",dpi = 300, width = 15, height = 11)
