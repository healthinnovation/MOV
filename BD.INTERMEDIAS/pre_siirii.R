prueba<-mov_general %>% filter(ANIO == 2020) %>% mutate(V005 = V005/1000000)

test<-svydesign(id =~ V001, strata =~ V022, weights=weights(V005), data=prueba)

svymean(~TIPORESIDENCIA, design = test, na.rm = T)

waldo::compare(mov_general$V005,mov_general$V005_A)

mov_general %>% 
  select(V005) %>% 
  mutate(
    t = V005/1000000
  )


