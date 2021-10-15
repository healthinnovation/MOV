HOGAR1<-
  HOGAR1 %>% 
  mutate(
    HV005_A = HV005/1000000000000
  )

test <- svydesign(id =~ HV001, strata =~ HV022, weights=~HV005_A, data= HOGAR1)

svymean(~HV025 %>% as.factor(), design = test, na.rm = T)

HOGAR1$HV025 %>% class()

table(HOGAR1$HV005_A)
