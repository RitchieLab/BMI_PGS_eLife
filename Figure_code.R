#Daniel Hui
#Created 11/25/2024

################
# Main Figures #
################
###Figure 2
#a.
d <- read.table("Figure1a_data.txt",head=T)
library(ggplot2)

decr <- subset(d, Covariate != "DBPadj" & Covariate != "TG" & Covariate != "PorkIntake" & Covariate != "ProcMeatIntake" & Covariate != "Townsend" & Covariate != "HbA1c")
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

png(filename = "Figure1a_decr_colorblind.png", width = 2560, height = 2560, res=400)
ggplot(decr, aes(x=Percentile, y=R2, group=Covariate, color=Covariate)) + geom_point() + geom_line() + expand_limits(xmin=0, xmax=100) + expand_limits(ymin=.045, ymax=.095) + theme_classic() + labs(y = bquote('PGS'~R^2)) +
  theme(legend.position = c(.25, .22), axis.text=element_text(size=14), axis.title = element_text(size=14)) +
  scale_color_manual(labels = c("Age", "Drink frequency/wk", "HDL cholesterol", "IPAQ", "Mean drinks/wk", "Moderate-vigorous PA", "Total cholesterol", "Vigorous PA"), values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
dev.off()


incr <- subset(d, Covariate == "DBPadj" | Covariate == "TG" | Covariate == "PorkIntake" | Covariate == "ProcMeatIntake" | Covariate == "Townsend" | Covariate == "HbA1c")

png(filename = "Figure1_incr_colorblind.png", width = 2560, height = 2560, res=400)
ggplot(incr, aes(x=Percentile, y=R2, group=Covariate, color=Covariate)) + geom_point() + geom_line() + expand_limits(xmin=0, xmax=100) + expand_limits(ymin=.045, ymax=.095) + theme_classic() + labs(y = bquote('PGS'~R^2)) +
  scale_color_manual(labels=c("Diastolic blood pressure", "HbA1c", "Pork intake", "Processed meat intake", "Triglycerides", "Townsend index") ,values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  theme(legend.position = c(.8, .26)) + theme(axis.text=element_text(size=14), axis.title = element_text(size=14))
dev.off()


#b.
d <- read.table("Figure1b_data.txt", head=T)

d$Ancestry_Cohort <- paste(d$Ancestry, d$Cohort, sep="_")

toplot <- subset(d, Exposure == "Age" | Exposure == "Sex" | Exposure == "HDL" | Exposure == "TG" | Exposure == "AlcoholDaysPerWeek" | Exposure == "DBP" | Exposure == "HbA1c" | Exposure == "PhysicalActivity" | Exposure == "Socioeconomic")

png(filename = "Figure1b_faceted_colorblind.png", width = 3000, height = 2560, res=300)
plt <- ggplot(toplot, aes(x=as.numeric(as.character(Mean)), y=R2, color=Ancestry, shape=Cohort, group=Ancestry_Cohort, size=N)) + geom_point(alpha=.5) + geom_line(size=.5, alpha=.5) + 
  facet_wrap(~ Exposure, ncol = 3, scales = "free_x", strip.position = "bottom", 
             labeller = as_labeller(c(Age ="Age (years)", HDL="HDL (mg/dL)", TG="TG (mg/dL)", Sex="Sex", 
                                      PhysicalActivity = "Physical Activity (percentile)", AlcoholDaysPerWeek = "Alcohol frequency (percentile)", HbA1c = "HbA1c (mmol/mol)", DBP = "Diastolic blood pressure (mmHg)", Socioeconomic = "Socioeconomic status (percentile)") )) +
  labs(x = NULL) + theme_bw() + geom_smooth(method = "lm", aes(x=as.numeric(as.character(Mean)), y=R2, group=Ancestry, weight=N)) + #without this + x label in grey box, if wanted
  theme(strip.background = element_blank(), strip.placement = "outside") + labs(y = bquote('PRS'~R^2)) +
  scale_color_manual(values=c("#D55E00", "#56B4E9"))
plt
dev.off()



###Figure 3
d <- read.table("Figure3_data.txt", head=T, sep="\t")
d$Ancestry_Cohort <- paste(d$Ancestry, d$Cohort, sep="_")

png(filename = "Figure3_interactionEffects_colorblind.png", width = 2560, height = 2560, res=400)
plt <- ggplot(d, aes(x=interaction_beta_div_prs_beta, y=-log10(Interaction_P), color=Ancestry, shape=Cohort, group=Ancestry_Cohort, size=N)) + geom_point() + 
  facet_wrap(~ Exposure, ncol = 3, scales = "free_x", strip.position = "bottom", 
             labeller = as_labeller(c(Age ="Age (years)", HDL="HDL (mg/dL)", TG="TG (mg/dL)", Sex="Sex (0 female, 1 male)", LDL = "LDL (mg/dL)", TotCholesterol = "Total cholesterol (mg/dL)",
                                      PhysicalActivity = "Physical Activity (SD)", AlcoholDaysPerWeek = "Alcohol frequency (SD)", HbA1c = "HbA1c (mmol/mol)", DBP = "Diastolic BP (mmHg)", Socioeconomic = "Socioeconomic status (SD)", SmokingPackYears = "Smoking (pack years)") )) +
  labs(x = "Proportion change in PRS effect per covariate unit change") + theme_bw() +
  theme(strip.background = element_blank(), strip.placement = "outside") + labs(y = bquote('-log'[10](interaction~p))) +
  theme(text=element_text(size=10), axis.text.x=element_text(angle=45,hjust=1)) + 
  geom_hline(yintercept=3.10, linetype="dashed", color = "green") +
  scale_color_manual(values=c("#D55E00", "#56B4E9"))
plt
dev.off()



###Figure 4
r2 <- read.table("Figure4_r2_data.txt", head=T)
int <- read.table("Figure4_int_data.txt", head=T)
main <- read.table("Figure4_main_data.txt", head=T)

r2$Ancestry_Cohort <- paste(r2$Ancestry, r2$Cohort, sep="_")
int$Ancestry_Cohort <- paste(int$Ancestry, int$Cohort, sep="_")
main$Ancestry_Cohort <- paste(main$Ancestry, main$Cohort, sep="_")

merged <- merge(r2, int)
merged <- merge(merged, main)

logbmi <- subset(merged, BMI == "logBMI")
ordinal <- subset(merged, BMI == "Ordinal")

png(filename = "Figure4_main_vs_r2_colorblind.png", width = 2560, height = 2560, res=400)
ggplot(logbmi, aes(x=Beta, y=R2_diff)) + geom_point(aes(shape=Cohort, color=Ancestry, size=N, alpha=.5)) +
  geom_smooth(method = "lm", aes(weight = N)) + xlab("Main effect") + ylab(bquote('Max'~R^2~'Difference')) + theme_bw() +
  annotate("text", x = -.04, y = -.05, label = "R = .55", size=7) + theme(legend.position="none", text=element_text(size=20)) +
  scale_color_manual(values=c("#D55E00", "#56B4E9"))
dev.off()

png(filename = "Figure4_interaction_vs_r2_colorblind.png", width = 2560, height = 2560, res=400)
ggplot(logbmi, aes(x=Interaction_Beta, y=R2_diff)) + geom_point(aes(shape=Cohort, color=Ancestry, size=N, alpha=.5)) +
  geom_smooth(method = "lm", aes(weight = N)) + xlab("Interaction effect") + ylab(bquote('Max'~R^2~'Difference')) + theme_bw() +
  annotate("text", x = -.01, y = 0, label = "R = .80", size=7) + theme(legend.position="none", text=element_text(size=20)) +
  scale_color_manual(values=c("#D55E00", "#56B4E9"))
dev.off()

png(filename = "Figure4_main_vs_interaction_colorblind.png", width = 3200, height = 2560, res=400)
ggplot(logbmi, aes(x=Beta, y=Interaction_Beta)) + geom_point(aes(shape=Cohort, color=Ancestry, size=N, alpha=.5)) +
  geom_smooth(method = "lm", aes(weight = N)) + xlab("Main effect") + ylab("Interaction effect") + theme_bw() +
  annotate("text", x = -.04, y = -.01, label = "R = .58", size=7) + theme(text=element_text(size=20)) +
  scale_color_manual(values=c("#D55E00", "#56B4E9"))
dev.off()



###Figure 5
library(ggplot2)
df <- read.table("Figure5_data.txt", head=T)

breaks_size <- c(10000, 50000, 100000) # Three breaks: minimum, half, and maximum
df$Cohort <- factor(df$Cohort, levels = c("UKBB", "PMBB", "eMERGE"))
df$ci_fatten <- sqrt(df$N) / sqrt(max(df$N)) * 2.5  # Decrease the multiplier for thinner max CI

png(filename="Figure5_quantile_regression.png", width=2800, height=1800, res=400)

ggplot(df, aes(x=Tau, y=Beta, color=Ancestry, group=Ancestry)) + 
  geom_point(aes(size=N)) + 
  geom_line(aes(size=N/20)) +
  facet_wrap(~Cohort) +      
  theme_minimal() +
  scale_size_continuous(
    name="Sample size (N)", 
    breaks=breaks_size, 
    range = c(0.1, 5), 
    labels=breaks_size, 
    guide = guide_legend(override.aes = list(linetype = 0))) + 
  scale_x_continuous(breaks=c(.20, .40, .60, .80))  # Change x-axis tick labels

dev.off()




###Figure 6
###model performances plot
d <- read.table("Figure6_data.txt", head=T)

library(ggplot2)
d <- subset(d, Which_covariates == "Age_gender_only")
d$Cohort_Ancestry_Model <- paste(d$Cohort, d$Ancestry, d$Model, sep="_")
d$Cohort_Ancestry <- paste(d$Cohort, d$Ancestry, sep="_")

#ggplot(d, aes(x=Cohort_Ancestry_Model, y=R2, fill=Cohort, color=Ancestry)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=75,hjust=1, size=10))

png(filename = "Figure6_colorblind.png", width = 3280, height = 2560, res=300)
ggplot(d, aes(x=Model, y=R2, shape=Cohort, color=Ancestry, group=Cohort_Ancestry)) + geom_point(size=3.5, stroke=2) + geom_line(size=1) + #theme(legend.text=element_text(size=rel(10))) + # + theme(axis.text.x=element_text(angle=75,hjust=1, size=10))
  theme_bw() + scale_x_discrete(labels=c("LASSO (no interactions)", "LASSO (with interactions)", "Neural network")) + theme(text = element_text(size=19)) + labs(y = bquote(Model~R^2)) + 
  theme(axis.title = element_text(size = 20)) + scale_color_manual(values=c("#D55E00", "#56B4E9"))
dev.off()


###Figure 7
d <- read.table("Figure7_data.txt", head=T)

png(filename = "Figure7_PGS-GxAge.png", width = 3200, height = 2560, res=600)
ggplot(d, aes(x=factor(GWAS, level=c('Main', 'GxAge', 'AgeQuartileStratified')), y=PGS_R2, group=Model, fill=Model)) + geom_col(position="dodge") +
  theme_bw() + xlab("GWAS") + scale_x_discrete(labels=c("Main" = "Main effects", "GxAge" = "GxAge", "AgeQuartileStratified" = "Age stratified")) +
  labs(y = bquote(PGS~R^2)) + scale_color_manual(values=c("#D55E00", "#56B4E9")) + scale_fill_discrete(labels=c('Main effects only', 'PGS*Age')) +
  theme(legend.title=element_text(size=10), legend.text=element_text(size=8), legend.key.size = unit(.50, 'cm'))
dev.off()



########################
# Supplemental Figures #
########################

###Supplemmental Figure 3-1
###compare P b/w cov and no cov PRS
no_cov <- read.table("Figure3-1_no_cov_data.txt", head=T)
cov <- read.table("Figure3-1_cov_data.txt", head=T)
cp <- cov[, c("Exposure", "P")]

merged <- merge(cp, no_cov)

mean(-log10(merged$P_noCovariatePRS))
mean(-log10(merged$P))

png(filename = "SFigure3-1_PRScov.png", width = 2560, height = 2560, res=400)
plot(-log10(merged$P_noCovariatePRS), -log10(merged$P),
     xlab=expression("Without covariate PGS (" * -log[10](p) * ")"),
     ylab=expression("With covariate PGS (" * -log[10](p) * ")"))
abline(a=0, b=1)
dev.off()


###Supplemental Figure 5-1
library(ggplot2)

# Set random seed for reproducibility
set.seed(2)

# Number of data points
n <- 5000

# Generate x values from a uniform distribution between 0 and 10
x <- runif(n, min = 0, max = 10)

generate_data <- function(beta) {
  # Define true intercept
  beta_0 <- 0
  
  # Generate random errors
  epsilon <- rnorm(n, mean = 0, sd = 1.25331413732)
  
  # Calculate y values
  y <- beta_0 + beta * x + epsilon
  
  data.frame(x, y, beta)
}

data_5 <- generate_data(.5)
data1 <- generate_data(1)
data2 <- generate_data(2)

all_data <- rbind(data_5, data1, data2)

# Plot all datasets together
png(filename="SFigure5-1_R2_vs_beta.png", width=1600, height=1400, res=400)
ggplot(all_data, aes(x = x, y = y, color = as.factor(beta))) + 
  geom_point(size=.00001) + 
  labs(color = "Beta") +
  xlim(0, 14) +
  geom_text(data = data.frame(x = c(12.5, 12.5, 12.5), y = c(5, 10, 20), 
                              beta = c(0.5, 1, 2), 
                              label = c("R^2~'='~0.57", "R^2~'='~0.84", "R^2~'='~0.95")),
            aes(x = x, y = y, label = label, color = as.factor(beta)),
            parse = TRUE,
            inherit.aes = FALSE, show.legend=F) +
  guides(color = guide_legend(override.aes = list(size = 1)))
dev.off()



###Supplemental Figure 5-2
#0-20th percentile vs 80-100th percentile, age
d <- read.table("INDIVIDUAL_LEVEL_DATA.txt", head=T, sep="\t")
d <- d[!is.na(d$BMI),]

neededVars <- na.omit(d[c("BMI","BMI_PRS", "Age", "Sex", "PC1", "PC2", "PC3", "PC4", "PC5")])
sorted <- neededVars[order(neededVars[["Age"]]),]

lim20 <- as.integer(dim(sorted)*.20)[1]
lim40 <- as.integer(dim(sorted)*.40)[1]
lim60 <- as.integer(dim(sorted)*.60)[1]
lim80 <- as.integer(dim(sorted)*.80)[1]


twenty <- sorted[0:(lim20),]
fourty <- sorted[(lim20+1):(lim40),]
sixty <- sorted[(lim40+1):(lim60),]
eighty <- sorted[(lim60+1):(lim80),]
hundred <- sorted[(lim80+1):dim(sorted)[1],]

twenty$Percentile <- "0-20"
hundred$Percentile <- "80-100"
merged <- rbind(twenty, hundred)

#plot
set.seed(123)
rand10k <- merged[sample(nrow(merged), 10000), ]

#ggplot(merged, aes(x=BMI_PRS, y=log(BMI), color=Percentile)) + geom_point(alpha=.01) +  geom_smooth(method = "lm", fill = NA) +geom_hex()
#ggplot(rand10k, aes(x=BMI_PRS, y=log(BMI), color=Percentile)) + geom_point(alpha=.50) +  geom_smooth(method = "lm", fill = NA) + theme_bw() +

model_twenty <- lm(log(BMI) ~ BMI_PRS, twenty)
model_hundred <- lm(log(BMI) ~ BMI_PRS, hundred)

summary(model_twenty)
var(log(twenty$BMI))
#Rsq=0.08819
#beta=1.12017
#MSE=0.02712272
#var=0.02974645

summary(model_hundred)
var(log(hundred$BMI))
#Rsq=0.06615
#beta=0.86989
#MSE=0.02206915
#var=0.02363266

png(filename="SFigure5-2_R2_vs_beta_UKBB.png", width=2160, height=1600, res=300)

library(ggplot2)

ggplot(rand10k, aes(x = BMI_PRS, y = log(BMI), color = Percentile)) + 
  geom_point(alpha = .50) +  
  geom_smooth(method = "lm", fill = NA) +
  
  # Second annotations, line by line
  annotate(geom = "text", x = 3.1, y = 4.1, label = bquote("R"^2 ~ "= 0.088"), 
           color = "#F8766D", hjust = 0, vjust = 1, size = 4, family = "sans", fontface = "plain") +
  annotate(geom = "text", x = 3.1, y = 4.025, label = "Beta = 1.12", 
           color = "#F8766D", hjust = 0, vjust = 1, size = 4, family = "sans", fontface = "plain") +
  annotate(geom = "text", x = 3.1, y = 3.95, label = "MSE = 0.027", 
           color = "#F8766D", hjust = 0, vjust = 1, size = 4, family = "sans", fontface = "plain") +
  
  # First annotations, line by line
  annotate(geom = "text", x = 3.1, y = 3.80, label = bquote("R"^2 ~ "= 0.066"), 
           color = "#00BFC4", hjust = 0, vjust = 1, size = 4, family = "sans", fontface = "plain") +
  annotate(geom = "text", x = 3.1, y = 3.725, label = "Beta = 0.87", 
           color = "#00BFC4", hjust = 0, vjust = 1, size = 4, family = "sans", fontface = "plain") +
  annotate(geom = "text", x = 3.1, y = 3.650, label = "MSE = 0.022", 
           color = "#00BFC4", hjust = 0, vjust = 1, size = 4, family = "sans", fontface = "plain") +
  
  labs(x = expression(PGS["BMI"]), color="Age percentile") +  # x-axis label with subscript
  theme_bw()

dev.off()