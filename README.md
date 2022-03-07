# MOV - Missed opportunities for vaccination in Peru 2010-2020: A study of socioeconomic inequalities

## Study description
This is the first study published in Peru that uses 11-year DHS to describe the frequency and evolution of MOVs for five vaccines against the leading causes of mortality in children under 5 years old. Moreover, we estimate inequalities in MOV based on socioeconomic variables (Maternal education and Wealth index) at the national and departmental levels. We found that by 2010, the national levels of MOVs reached levels ranging from 45% (pentavalent vaccine) to 78% (rotavirus vaccine). At the national level, we found a greater concentration of inequities in MOVs in the higher socioeconomic strata. However, we identified that the departments with higher poverty rates concentrated higher levels of inequality on MOVs in the lowest strata of the socioeconomic variables. In addition, we found that some departments with similar geographic and socioeconomic characteristics had spatially correlated levels of inequalities on MOVs. 

![](https://github.com/healthinnovation/MOV/blob/main/Figures/Fig3.png)

> Evolution of MOVs for each vaccine during the 11-years period. MOVs of pentavalent (PTV), Pneumococcal (NM), Influenza (INLFU) and Rotavirus vaccine (ROTA).

## Repository structure

1. [NewData](https://github.com/healthinnovation/MOV/tree/main/BD.INTERMEDIAS) -  databases created from the raw data of the 2010 - 2020 ENDES surveys.
2. [ENDES](https://github.com/healthinnovation/MOV/tree/main/ENDES) - Raw data from ENDES surveys for the period 2010 - 2020.
3. [Figures](https://github.com/healthinnovation/MOV/tree/main/Figures) - Figures in the main text
  - Fig1: Vaccination coverage 2010-2020. 
  - Fig2: Distribution of complete doses of vaccine.
  - Fig3: Evolution of MOVs for each vaccine during the 11-years period. 
  - Fig4: Slope Index of Inequality at national level for the 4 vaccines.
  - Fig5: Slope Index of Inequality of the MOVs for each department from 2010 – 2020 (Wealth index, WI). 
  - Fig6: Slope Index of Inequality of the MOVs for each department from 2010 – 2020 (Maternal Education, ME). 
  - Fig7: Stratification of the Slope Index of Inequality analysis. 
5. [SII_RII_functions](https://github.com/healthinnovation/MOV/tree/main/SII_RII_functions) - Functions created for each developed analysis, including SII and RII. 
6. .gitignore
7. [00_Data.rmd](https://github.com/healthinnovation/MOV/blob/main/00_Data.rmd) - Rmarkdown of data preparation.
8. [01_EDA.rmd](https://github.com/healthinnovation/MOV/blob/main/01_EDA.rmd) - Rmarkdown of exploratory data analysis.
9. [02_SII_RII.rmd](https://github.com/healthinnovation/MOV/blob/main/02_SII_RII.Rmd) - Rmarkdown of the SII calculation at national and departmental level.
10. [MOV.Rproj](https://github.com/healthinnovation/MOV/blob/main/MOV.Rproj) - Rproject file
11. README.md

##Environment and version

```
platform       x86_64-w64-mingw32          
arch           x86_64                      
os             mingw32                     
system         x86_64, mingw32             
status                                     
major          4                           
minor          0.3                         
year           2020                        
month          10                          
day            10                          
svn rev        79318                       
language       R                           
version.string R version 4.0.3 (2020-10-10)
nickname       Bunny-Wunnies Freak Out
```
