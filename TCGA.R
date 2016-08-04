library("readxl")
TCGA_data <- read_excel("C:/Users/GUser/Desktop/R/data/result_mirna_muta_num.xlsx")
class(TCGA_data)

len = nrow(TCGA_data)

glimpse(TCGA_data)
str(TCGA_data)
head(TCGA_data)
summary(TCGA_data)



summary(TCGA_data$OS)
mean(TCGA_data$OS, na.rm=TRUE)
median(TCGA_data$OS, na.rm=TRUE)
range(TCGA_data$OS, na.rm=TRUE)
quantile(TCGA_data$OS, na.rm=TRUE)

old_par = par(mfrow=c(2,2))
boxplot(TCGA_data$OS)
qqnorm(TCGA_data$OS)
qqline(TCGA_data$OS)
hist(TCGA_data$OS, prob=TRUE)
par(old_par)


par = par(mfrow=c(2,2))
  hist(TCGA_data$percent_normal_cells_BOTTOM)
hist(TCGA_data$percent_stromal_cells_BOTTOM)
hist(TCGA_data$percent_tumor_cells_BOTTOM)
hist(TCGA_data$percent_tumor_nuclei_BOTTOM)



par = par(mfrow=c(2,2))
hist(TCGA_data$percent_normal_cells_TOP)
hist(TCGA_data$percent_stromal_cells_TOP)
hist(TCGA_data$percent_tumor_cells_TOP)
hist(TCGA_data$percent_tumor_nuclei_TOP)

#normal_cells -  보통세포
#stromal cell - 기질세포
#tumor   cell - 종양세포 
#tumor nuclei - 종양핵


#cell vs EVENT
opar = par(mfrow=c(1,2))
plot(log10(TCGA_data$percent_normal_cells_BOTTOM), TCGA_data$EVENT, cex=.5)
plot(TCGA_data$percent_normal_cells_BOTTOM, TCGA_data$EVENT, cex=.5)


plot(log10(TCGA_data$percent_stromal_cells_BOTTOM), TCGA_data$EVENT, cex=.5)
plot(TCGA_data$percent_stromal_cells_BOTTOM, TCGA_data$EVENT, cex=.5)


plot(log10(TCGA_data$percent_tumor_cells_BOTTOM), TCGA_data$EVENT, cex=.5)
plot(TCGA_data$percent_tumor_cells_BOTTOM, TCGA_data$EVENT, cex=.5)


plot(log10(TCGA_data$percent_tumor_nuclei_TOP), TCGA_data$EVENT, cex=.5)
plot(TCGA_data$percent_tumor_nuclei_TOP, TCGA_data$EVENT, cex=.5)


opar = par(mfrow=c(2,2))
plot((TCGA_data$percent_normal_cells_TOP), TCGA_data$EVENT, cex=.5)
plot((TCGA_data$percent_stromal_cells_TOP), TCGA_data$EVENT, cex=.5)
plot((TCGA_data$percent_tumor_cells_TOP), TCGA_data$EVENT, cex=.5)
plot((TCGA_data$percent_tumor_nuclei_TOP), TCGA_data$EVENT, cex=.5)


opar = par(mfrow=c(1,1))
#neoplasm(종양)_histologic(조직이 어떻게 생겨먹었는지)_grade vs EVENT
plot(TCGA_data$neoplasm_histologic_grade, TCGA_data$EVENT, cex=.5)

pairs(TCGA_data[,-1] %>% sample_n(500))



miRNA_HiSeq <- read_excel("C:/Users/GUser/Desktop/R/data/miRNA_HiSeq.xlsx")
len = nrow(miRNA_HiSeq)
summary(miRNA_HiSeq)


