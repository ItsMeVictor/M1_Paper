# Econometrics/Quantitative Paper

Thank you for your interest in the code! I hope my paper was not too long. 

In this repository, you will find:
- The R code used to construct the dataset, divided into the different main parts. 
  - The code_merge.R file is the one that merges election, census, and control data. It will run only once all the other files have run. 
- The Stata code used to compute the diagnostic/specification tests, applied to the different datasets I constructed. 
- The datasets: 
  - final_merge.dta is the dataset before I add the 1997 and 2002 electoral results. 
  - extend_merge.dta is the same dataset but with these results added. 
  - instrument_merge.dta is the same dataset, but with the instruments too. 

I cannot promise that the code is the cleanest, but ultimately, all of it should run and all my results should be fully replicable. 