library(haven)
GMMA_data_STATA <- read_dta("C:/Users/Admin/Desktop/GMMA data STATA.dta")
View(GMMA_data_STATA)


#new variable


r

data(Finance)
f1 <- Finance[1:232, "rm"]
f2 <- Finance[1:232, "hml"]
f3 <- Finance[1:232, "smb"]
f4<-Finance[1:232,"NHP"]
f5<-Finance[1:232,"TDW"]
f6<-Finance[1:232,"AA"]
f7<-Finance[1:232,"EBF"]
y <- Finance[1:232,"WMK"]

res <- gmm(y ~ f1, ~ f1 + f2 + f3+f4+f5)
summary(res) 
Finance 

res2 <- gmm(y ~ f1 + f2 + f3, ~ f1 + f2 + f3+f4+f5, eqConst = matrix(c(1,2,0,1),2,2))
res2

res4 <- gmm(y~ f1 + f2 + f3, ~ f1 + f2 + f3+f5, t0=c(0,1,.5,.5), eqConst = c(1,2),
            eqConstFullVcov=TRUE)
summary(res4)

GMMA_data_STATA$High_Tech_TCI<-y
head(GMMA_data_STATA$High_Tech_TCI)
GMMA_data_STATA$GDP_per_capita<-f1
GMMA_data_STATA$GDP_product<-f2
GMMA_data_STATA$distance<-f3
GMMA_data_STATA$Distance_between_major_ports<-f4
GMMA_data_STATA$Free_Trade_Agreement<-f5
GMMA_data_STATA$Inward_FDI_stock__of_GDP<-f6
GMMA_data_STATA$Outward_FDI_stock__of_GDP<-f7

GMM_Model_1 <- gmm(High_Tech_TCI ~ GDP_per_capita + GDP_product +distance+Distance_between_major_ports+Free_Trade_Agreement+Inward_FDI_stock__of_GDP, ~ GDP_per_capita + GDP_product + distance+Distance_between_major_ports+Free_Trade_Agreement+Inward_FDI_stock__of_GDP,data = GMMA_data_STATA, eqConst = matrix(c(1,2,0,1),2,2))
GMM_Model_

GMM_model_2<-gmm(High_Tech_TCI~GDP_per_capita+GDP_product+distance+Distance_between_major_ports+Free_Trade_Agreement+Outward_FDI_stock__of_GDP,~ GDP_per_capita + GDP_product + distance+Distance_between_major_ports+Free_Trade_Agreement+Outward_FDI_stock__of_GDP,data = GMMA_data_STATA, eqConst = matrix(c(1,2,0,1),2,2)) 
GMM_model_2

#After a successfull moment let us now create new repositories
GMM_MODEL_3<-gmm(High_Tech_TCI)