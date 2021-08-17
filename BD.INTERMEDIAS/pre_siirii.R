t<-df2 %>%
  mutate(
    
    ptv1=map(.x = df,
             .f =~svyciprop(~S45PV1_1, design =.x, na.rm = T))) %>% 
  
  ungroup() 


t$ptv1[[1]] %>% data.frame() %>% rownames_to_column()
    
    ptv1ci = map(.x = ptv1,
                 .f = ~confint(.x) %>% 
                   as.data.frame() %>%
                   rownames_to_column("tar"))) %>% 
  unnest(cols = c(ptv1ci)) %>% 
  as.data.frame() %>% 
  mutate(ci_l = `2.5%`,
         ci_h = `97.5%`,
         vacuna = ptv1$S45PV1_1,
         vacunatipo = "ptv1") %>% 
  select(ANIO,vacuna,ci_l,ci_h,vacunatipo)
