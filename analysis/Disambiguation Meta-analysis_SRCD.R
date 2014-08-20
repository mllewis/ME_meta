## Disambiguation Meta-Analysis for SRCD abstract
# Started 4-16-13
# M. Lewis

# load library
library(ggplot2)
source("/Documents/GRADUATE_SCHOOL/Ranalysis/useful.R")


## define functions
# calculate pooled and weighted estimate of population d
d_pop <- function(N, d) {
  d = sum(N * d)/sum(N)
  return(d)
}

#calculate confidence interval for pop_d
d_error <- function(N, d){
  # calculate the across study variance
  across_study_var = sum(N *((d - mean(d))^2))/sum(N)

  # calculate the sampling error
  samp_error = 4/mean(N) * ((1+mean(d)^2)/8)
  
  # total error
  error = 1.96*sqrt(across_study_var - samp_error)
  
  return(error)
}

# load data
metad = read.csv("/Documents/GRADUATE_SCHOOL/Projects/ME_meta/Analysis/Disambiguation Meta-Analysis Data - Sheet1.csv")

# calculate d for conditions not reported
metad$d_by_t <- metad$t/sqrt(metad$N)
metad$d_by_M <- (metad$M_experimental-metad$M_baseline) / metad$SD

# get a d for each condition
metad$d_calculate <- ifelse(!is.na(metad$d), metad$d, 
                            ifelse(!is.na(metad$d_by_t),metad$d_by_t, 
                                   ifelse(!is.na(metad$d_by_M), metad$d_by_M, NA)))

# look at rows with discrepant d's
metad$d_error = ifelse((abs(metad$d_by_t-metad$d_by_M )> .1) | (abs(metad$d-metad$d_by_M ) > .1) | (abs(metad$d-metad$d_by_t)> .1), 1, 0)
metad[which(metad$d_error == 1),c("paper_key","t","N","M_experimental", "M_baseline" ,"SD", "d", "d_by_t", "d_by_M")] # Diesendruck and demarchena

# remove conditions for which we don't have cohen's d and that we're excluding on other grounds
#metad[!is.na(metad$d_calculate),c("paper_key","t","N","M_experimental", "M_baseline" ,"SD", "d", "d_by_t", "d_by_M",'d_calculate', 'd_error')]
md <- metad[!is.na(metad$d_calculate),]
md <- md[md$include.in.basic.analysis.==1,]

# look at conditions we have d's for
md$notes_short = substr(md$Notes,1,40)
md[, c("paper_key", 'notes_short', "expt_num",'d_calculate', "N")]
md <- md[order(md$paper_key),] 

# look at conditions, sorted by effect size
md[order(md$d_calculate),c("paper_key", 'notes_short', "expt_num",'d_calculate')] 

# total number of unique studies 
length(unique(metad$paper_key))

# number of unique studies usable
length(unique(md$paper_key))#20

# total number of conditions
dim(md)[1] #51

# Plot predictors of d
# AGE
#pdf("d_age_vocab2.pdf", width = 12, height = 6)
par(mfrow=c(2,1))
qplot(age_mean..months., d_calculate, 
      data=md, ylab = "Cohen's d", xlab= "Age (months)") + 
  geom_smooth(method="lm", formula=y~log(x)) +
  theme_bw() + 
  theme(axis.title = element_text(face="bold", size=20),
        axis.text  = element_text(vjust=0.5, size=16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(-1, 2.4)) +
  annotate("text", x=50, y=-.5, size = 10,
           label=paste("r=",round(cor(md$d_calculate, md$age_mean..months., use = "complete"), 2)))
#dev.off()

#cor.test(md$d_calculate, md$age_mean..months., use = "complete.obs")


#VOCAB
#pdf("dbyvocab.pdf", width = 6, height = 6)
qplot(CDI_prod_mean, d_calculate, data=md, ylab = "Cohen's d", xlab= "Mean CDI productive vocabulary") + 
  geom_smooth(method="lm", formula=y~log(x)) +
  theme_bw() + 
  theme(axis.title = element_text(face="bold", size=20),
        axis.text  = element_text(vjust=0.5, size=16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(-1, 2.4)) +
  annotate("text", x=300, y=-.5, size = 10,
           label=paste("r=",round(cor(md$d_calculate, md$CDI_prod_mean, use = "complete"), 2)))

cor.test(md$d_calculate, md$CDI_prod_mean, use = "complete.obs")
#dev.off()

# compare vocab and age as predictors of effect size
m1 = lm(d_calculate ~ CDI_prod_mean + age_mean..months., d=md)
summary(m1)

