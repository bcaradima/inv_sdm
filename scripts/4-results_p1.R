### RESULTS: Paper 1 ####
# Variable selection ####
K <- c("A10m", "A100m", "A1km", "A.EDO", "IAR", "TU.Dm", "TU.Cr", "LUD", "Urban", "UI", "F10m", "F100m", "F1km", "F.EDO","Forest", "FRI", "Temp", "FV", "WV", "BM", "Morph", "WW", "HP", "Temp2")

predictors <- prepare.inputs(K, sample.bdms, center = TRUE)

# Plot the correlations between potential explanatory variables
cor.plot(select(predictors, -SiteId, -SampId), numbers=TRUE)

vs.bdms <- extract.vsr("outputs/paper 1 submission 2 results/variable selection/bdms_untransformed", sample.bdms)

# Get mean relative deviance for each model over entire community (excluding rare taxa)
vs.bdms.mean <- vs.bdms %>%
  filter(!is.infinite(rdev.test) & n > 56) %>%
  group_by(Model) %>%
  summarise(mrd.train = mean(rdev.train, na.rm=TRUE), mrd.test = mean(rdev.test, na.rm=TRUE)) %>%
  arrange(mrd.test) %>%
  mutate(Parameters = vapply(strsplit(Model, " "), length, integer(1)) + 1) # Add one more parameter for the intercept

# Count the number of explanatory variables (as opposed to number of parameters)
vs.bdms.mean$ExpVar <- sapply(vs.bdms.mean$Model, function(i){
  i <- strsplit(i," ")[[1]]
  i <- i[!i %in% c("FV2", "Temp2")]
  uniqueN(i)
})

plot.data <- vs.bdms.mean %>%
  arrange(Parameters) %>%
  group_by(Parameters) %>%
  mutate(Label = paste(Parameters, " (", formatC(uniqueN(Model), big.mark=",")," models)", sep="")) %>%
  ungroup() %>%
  mutate(Label = factor(Label, levels = unique(Label)))

g <- ggplot(plot.data, aes(x = mrd.train, y = mrd.test, color = as.factor(Label)))
g <- g + geom_point()
g <- g + geom_abline(intercept = 0, slope = 1, color="red", size=1.25)
g <- g + theme_bw(base_size = 23)
g <- g + labs(y = "Mean standardized deviance during prediction",
              x = "Mean standardized deviance during calibration",
              color = "Number of\nparameters*",
              caption = expression("* including"~alpha["j"]^"taxa"))
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
g <- g + coord_cartesian(ylim = c(0.7, 1.2), xlim = c(0.7, 1.2))
# sequential palette from http://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=9
g <- g + scale_color_manual(values=c("#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"))
# experimental palettes
# g <- g + scale_color_brewer(palette = "Spectral", direction=-1)
# g <- g + scale_color_brewer(type='seq', palette=2)
# g <- g + scale_color_gradient2(low='#05D9F6', high='#5011D1')
print(g)

# svg("P1 - variable selection.svg", width=11, height=8.5)
pdf("P1 - variable selection.pdf", width=11, height=8.5)
print(g)
dev.off()

# Explanatory variables ####
# Prepare the explanatory variables based on the results of the variable selection procedure
K <- c("Temp", "Temp2", "FV", "F100m", "LUD", "IAR")
predictors <- prepare.inputs(K, sample.bdms, center = TRUE)
write.csv(predictors, 'outputs/predictors.csv', row.names = FALSE)

### Individual models ####
# Calibrate and cross-validate the individual models
isdm <- run.isdm(sample.bdms, predictors, trace = FALSE)

# How many models lowered the deviance?
table(sapply(isdm$models, function(m){
  m$deviance < m$null.deviance
}))

# How many taxa used optim?
table(sapply(isdm$models, function(m){
  m$optim
}))

cv.iSDM <- cv.isdm(predictors)

# Prepare models ####
# deploy.jsdm() ####
# Prepare folders, inputs, and scripts for the calibration and cross-validation
# of the hmSDMs and jSDMs
deploy.jsdm(K, sample.bdms, "FF0", center=T, cv=0)
deploy.jsdm(K, sample.bdms, "FF0_train1", center=T, cv=1)
deploy.jsdm(K, sample.bdms, "FF0_train2", center=T, cv=2)
deploy.jsdm(K, sample.bdms, "FF0_train3", center=T, cv=3)

deploy.jsdm(K, sample.bdms, "TF0", center=T, cv=0)
deploy.jsdm(K, sample.bdms, "TF0_train1", center=T, cv=1)
deploy.jsdm(K, sample.bdms, "TF0_train2", center=T, cv=2)
deploy.jsdm(K, sample.bdms, "TF0_train3", center=T, cv=3)

deploy.jsdm(K, sample.bdms, "TT0", center=T, cv=0)
deploy.jsdm(K, sample.bdms, "TT0_train1", center=T, cv=1)
deploy.jsdm(K, sample.bdms, "TT0_train2", center=T, cv=2)
deploy.jsdm(K, sample.bdms, "TT0_train3", center=T, cv=3)

deploy.jsdm(K, sample.bdms, "TT1", center=T, cv=0)
deploy.jsdm(K, sample.bdms, "TT1_train1", center=T, cv=1)
deploy.jsdm(K, sample.bdms, "TT1_train2", center=T, cv=2)
deploy.jsdm(K, sample.bdms, "TT1_train3", center=T, cv=3)

deploy.jsdm(K, sample.bdms, "TT2", center=T, cv=0)
deploy.jsdm(K, sample.bdms, "TT2_train1", center=T, cv=1)
deploy.jsdm(K, sample.bdms, "TT2_train2", center=T, cv=2)
deploy.jsdm(K, sample.bdms, "TT2_train3", center=T, cv=3)

deploy.jsdm(K, sample.bdms, "TT3", center=T, cv=0)
deploy.jsdm(K, sample.bdms, "TT3_train1", center=T, cv=1)
deploy.jsdm(K, sample.bdms, "TT3_train2", center=T, cv=2)
deploy.jsdm(K, sample.bdms, "TT3_train3", center=T, cv=3)

deploy.jsdm(K, sample.bdms, "TT4", center=T, cv=0)
deploy.jsdm(K, sample.bdms, "TT4_train1", center=T, cv=1)
deploy.jsdm(K, sample.bdms, "TT4_train2", center=T, cv=2)
deploy.jsdm(K, sample.bdms, "TT4_train3", center=T, cv=3)

# Plot traces of the Markov chains for a given model workspace
plot.trace(directory = "outputs/paper 1 submission 2 results", folder = 'FF0')
plot.trace(directory = "outputs/paper 1 submission 2 results", folder = 'TF0')
plot.trace(directory = "outputs/paper 1 submission 2 results", folder = 'TT0')
plot.trace(directory = "outputs/paper 1 submission 2 results", folder = 'TT1')
plot.trace(directory = "outputs/paper 1 submission 2 results", folder = 'TT2')

# Extract the results of the fitted hierarchical multi-species/joint model
fit.FF0 <- extract.jsdm('outputs/paper 1 submission 2 results', 'FF0') # simplest multi-species model
fit.TF0 <- extract.jsdm('outputs/paper 1 submission 2 results', 'TF0') # correlated beta.comm
fit.TT0 <- extract.jsdm('outputs/paper 1 submission 2 results', 'TT0') # correlated beta.comm, site effects
fit.TT1 <- extract.jsdm('outputs/paper 1 submission 2 results', 'TT1')
fit.TT2 <- extract.jsdm('outputs/paper 1 submission 2 results/TT2 modified chains', 'TT2')
# fit.TT3 <- extract.jsdm('outputs/paper 1 submission 2 results', 'TT3')
# fit.TT4 <- extract.jsdm('outputs/paper 1 submission 2 results', 'TT4')

# Combine model results ####
# Warning: manually check columns being dropped!
deviance <- bind_rows(isdm$deviance, fit.FF0$deviance, fit.TF0$deviance, fit.TT0$deviance, fit.TT1$deviance, fit.TT2$deviance)
#jsdm.TT3$deviance, jsdm.TT4$deviance

deviance$Model <- ifelse(deviance$Model=="FF0", "UF0", deviance$Model)
deviance$Model <- ifelse(deviance$Model=="FT0", "UT0", deviance$Model) # random effect model
deviance$Model <- ifelse(deviance$Model=="TF0", "CF0", deviance$Model)
deviance$Model <- ifelse(deviance$Model=="TT0", "CT0", deviance$Model)

deviance$Model <- ifelse(deviance$Model=="TT1", "CT1", deviance$Model)
deviance$Model <- ifelse(deviance$Model=="TT2", "CT2", deviance$Model)

# MAIN RESULTS ####
# Quality of fit ####
# Prepare datasets for plotting quality of fit
plot.data <- deviance %>%
  mutate(prevalence = n.present / n.samples * 100,
         ModelLabel = factor(Model, levels=c("Null model", "iSDM", "UF0", "UT0", "CF0", "CT0", "CT1", "CT2")),
         TaxonLabel = factor(Taxon, levels=c(names(sort(n.bdms, decreasing=FALSE)))))

null.deviance <- plot.data %>%
  filter(Model=="iSDM") %>%
  mutate(std.deviance = null.deviance/n.samples,
         Model = "Null model",
         ModelLabel = factor(Model, levels=c("Null model", "iSDM", "UF0", "UT0", "CF0", "CT0", "CT1", "CT2"))
         )


# Construct data for connecting iSDM and mSDM points
data1 <- plot.data %>%
  filter(Model %in% c("iSDM", "UF0")) %>%
  select(Taxon, Model, prevalence) %>%
  spread(Model, prevalence) %>%
  rename("Taxon"="Taxon", "x1"="iSDM", "x2"="UF0")

data2 <- plot.data %>%
  filter(Model %in% c("iSDM", "UF0")) %>%
  select(Taxon, Model, std.deviance) %>%
  spread(Model, std.deviance) %>%
  rename("Taxon"="Taxon", "y1"="iSDM", "y2"="UF0")

dev.segments <- left_join(data1, data2, by = "Taxon")

# Construct data for analytical solution of null deviance 
n <- 580
m <- 1:(n-1)

null.dev.line <- tibble(null.deviance = -2*(m/n*log(m/n)+(n-m)/n*log(1-m/n)), n.samples = m)
null.dev.line$prevalence <- (m/n)*100
null.dev.line$Model <- "Null model"
null.dev.line$ModelLabel <- factor(null.dev.line$Model, levels=c("Null model", "iSDM", "UF0", "UT0", "CF0", "CT0", "CT1", "CT2"))
plot.data <- plot.data %>%
  filter(Model %in% c("iSDM", "UF0")) %>%
  bind_rows(null.deviance)

g1 <- ggplot()
g1 <- g1 + geom_point(data = plot.data, aes(x=prevalence, y=std.deviance, color=ModelLabel), size=3, alpha=0.4)
g1 <- g1 + geom_point(data = plot.data, aes(x=prevalence, y=std.deviance, color=ModelLabel), size = 3, alpha=0.4)
g1 <- g1 + geom_line(data = null.dev.line, aes(x=prevalence, y=null.deviance, color=ModelLabel), linetype = "dashed", alpha=0.4, show.legend = FALSE)
#
g1 <- g1 + geom_segment(data = dev.segments, aes(x=x1, y=y1, xend=x2, yend=y2), alpha = 0.3)

g1 <- g1 + scale_colour_manual(values=c("Null model" = "#000000", "iSDM" = "#790FBF", "UF0" = "#048504"))
g1 <- g1 + theme_bw(base_size = 20)
g1 <- g1 + theme(axis.text=element_text(size=14),
                 plot.title = element_blank())
g1 <- g1 + labs(y = "Standardized deviance",
                x = "Prevalence (%)",
                color = "Model")
g1 <- g1 + guides(colour = guide_legend(override.aes = list(size=6)), shape = FALSE, color = FALSE)

rm(data1, data2, dev.segments, n, m)

# Plot P1 - D2 vs std dev ###
# Point segments
data1 <- plot.data %>% # prepare y-coordinates
  filter(Model != "Null model") %>%
  select(Taxon, Model, D2) %>%
  spread(Model, D2)
# data1 <- spread(select(plot.data, Taxon, Model, D2), Model, D2) # y coordinates
colnames(data1) <- c("Taxon", "y1", "y2")

data2 <- plot.data %>% # prepare y-coordinates
  filter(Model != "Null model") %>%
  select(Taxon, Model, std.deviance) %>%
  spread(Model, std.deviance)
# data2 <- spread(select(plot.data, Taxon, Model, std.deviance), Model, std.deviance) # x coordinates
colnames(data2) <- c("Taxon", "x1", "x2")
dev.segments <- left_join(data1, data2, by = "Taxon")

plot.data.g2 <- filter(plot.data, Model != "Null model")
g2 <- ggplot()
g2 <- g2 + geom_point(data = plot.data.g2, aes(x = std.deviance, y = D2, color = Model, size = n.present), alpha = 0.4)
g2 <- g2 + geom_segment(data = dev.segments, aes(x = x1, y = y1, xend = x2, yend= y2), alpha = 0.2)
g2 <- g2 + scale_colour_manual(values=c("UF0" = "#048504", "iSDM" = "#790FBF"))
g2 <- g2 + theme_bw(base_size = 20)
g2 <- g2 + theme(axis.text=element_text(size=14),
                 plot.title = element_blank())
g2 <- g2 + labs(x = "Standardized deviance",
                y = expression("D"["j"]^2),
                color = "Model", 
                size = "Number of\noccurrences")
g2 <- g2 + guides(color = guide_legend(override.aes = list(size=6)))

rm(data1, data2, dev.segments)

plot.data <- deviance %>%
  mutate(prevalence = n.present / n.samples * 100,
         ModelLabel = factor(Model, levels=c("Null model", "iSDM", "UF0", "UT0", "CF0", "CT0", "CT1", "CT2")),
         ModelLabel = factor(Model, levels=rev(levels(ModelLabel))),
         TaxonLabel = factor(Taxon, levels=c(names(sort(n.bdms, decreasing=FALSE))))
         )

g3 <- ggplot(data = plot.data)
g3 <- g3 + geom_boxplot(aes(x=ModelLabel, y=std.deviance))
g3 <- g3 + coord_flip()
g3 <- g3 + theme_bw(base_size = 20)
g3 <- g3 + labs(x="Model",
                y="Standardized deviance")

# Points and LOESS curves
plot.data <- deviance %>%
  mutate(prevalence = n.present / n.samples * 100,
         ModelLabel = factor(Model, levels=c("Null model", "iSDM", "UF0", "UT0", "CF0", "CT0", "CT1", "CT2")),
         TaxonLabel = factor(Taxon, levels=c(names(sort(n.bdms, decreasing=FALSE))))
  )

g4 <- ggplot(data = plot.data)
g4 <- g4 + geom_point(aes(x=prevalence, y=std.deviance, color=ModelLabel), alpha=0.4)
g4 <- g4 + stat_smooth(aes(x=prevalence, y=std.deviance, group=ModelLabel, color=ModelLabel), geom='line', size = 2, alpha=0.50, se=TRUE, method = "loess")
g4 <- g4 + geom_point(data = null.deviance, aes(x=prevalence, y=std.deviance, color=ModelLabel), alpha=0.4)
g4 <- g4 + geom_line(data = null.dev.line, aes(x=prevalence, y=null.deviance), color="black", linetype = "dashed", alpha=0.4, show.legend = FALSE)
g4 <- g4 + labs(y = "Standardized deviance\n(fitted with LOESS curves)",
                x = "Prevalence (%)",
                color = "Model")
g4 <- g4 + theme_bw(base_size = 20)
# g4 <- g4 + scale_color_brewer(palette = "Pastel1")

g4 <- g4 + scale_colour_manual(breaks=levels(plot.data$ModelLabel),
                               values=c("Null model" = "#000000", 
                                        "iSDM" = "#790FBF",
                                        "UF0" = "#948B8B",
                                        "CF0" = "#048504",
                                        "CT0" = "#DB1111",
                                        "CT1" = "#030AE8",
                                        "CT2" = "#A84E05"))
print(g4)

pdf('outputs/paper 1 extensions/P1 - quality of fit [all] extended.pdf', height=9, width=14)
ggarrange(g1,g2,g4,g3, align="h", labels="auto", font.label = list(size = 16))
dev.off()

# How do correlated community parameters improve quality of fit?
plot.data.bar <- plot.data %>%
  filter(Trial %in% c("FF0", "TF0")) %>%
  as.tibble()

levels(plot.data.bar$TrialLabel)[levels(plot.data.bar$TrialLabel)=="FF0"] <- "Uncorrelated beta_comm"
levels(plot.data.bar$TrialLabel)[levels(plot.data.bar$TrialLabel)=="TF0"] <- "Correlated beta_comm"

plot.data.point <- plot.data %>%
  filter(Trial=="Null model")

g <- ggplot()
g <- g + geom_bar(data=plot.data.bar, aes(x=TaxonLabel, y=std.res.dev, fill=TrialLabel), stat="identity", position="identity", alpha=0.6)
g <- g + geom_point(data=plot.data.point, aes(x=TaxonLabel, y=std.res.dev), size=0.75, alpha=0.35)
g <- g + theme(axis.text.x = element_blank(),
               axis.ticks.x = element_blank())
g <- g + labs(title = "Standardized deviance of taxa in joint model w/ and w/o correlated beta_comm",
              subtitle = "Null model deviance given by points",
              x = "Taxa (increasing prevalence from left to right)",
              y = "Standardized deviance",
              fill = "Model")
g <- g + scale_fill_brewer(palette = "Set1")

pdf('outputs/paper 1 extensions/P1 - Quality of fit of FF0 vs FT0.pdf', width = 15, paper='a4r')
g
dev.off()

# Tjur coefficient of discrimination
isdm$tjur$Trial <- "iSDM"
tjur <- bind_rows(isdm$tjur, jsdm.FF0$tjur, jsdm.TF0$tjur, jsdm.TT0$tjur, jsdm.TT1$tjur, jsdm.TT2$tjur, jsdm.TT3$tjur, jsdm.TT4$tjur)

boxplot(data = tjur, D ~ Model, horizontal = TRUE, xlab="Tjur's R-squared")


# Predictive performance ####
# Extract the results from the cross-validation of the hm/jSDMs
cv.iSDM <- cv.isdm(predictors)
cv.FF0 <- cv.jsdm(folder = "FF0")
cv.TF0 <- cv.jsdm(folder = 'TF0')
cv.TT0 <- cv.jsdm(folder = 'TT0')
cv.TT1 <- cv.jsdm(folder = 'TT1')
cv.TT2 <- cv.jsdm(directory = "outputs/paper 1 submission 2 results/TT2", folder = 'TT2')

# Combine cross-validation deviance statistics from all models
cv.FF0$deviance$Trial <- "UF0"
cv.iSDM$deviance$Trial <- "iSDM"
cv.iSDM$deviance <- cv.iSDM$deviance[, colnames(cv.FF0$deviance)]

cv.deviance <- bind_rows(cv.iSDM$deviance, cv.FF0$deviance, cv.TF0$deviance, cv.TT0$deviance, cv.TT1$deviance, cv.TT2$deviance)

cv.deviance$Trial <- ifelse(cv.deviance$Trial=="FF0", "UF0", cv.deviance$Trial)
cv.deviance$Trial <- ifelse(cv.deviance$Trial=="FT0", "UT0", cv.deviance$Trial)
cv.deviance$Trial <- ifelse(cv.deviance$Trial=="TF0", "CF0", cv.deviance$Trial)
cv.deviance$Trial <- ifelse(cv.deviance$Trial=="TT0", "CT0", cv.deviance$Trial)

cv.deviance$Trial <- ifelse(cv.deviance$Trial=="TT1", "CT1", cv.deviance$Trial)
cv.deviance$Trial <- ifelse(cv.deviance$Trial=="TT2", "CT2", cv.deviance$Trial)

# cv.deviance$Type <- ifelse(cv.deviance$Type=="Training", "Calibration", cv.deviance$Type)
# cv.deviance$Type <- ifelse(cv.deviance$Type=="Testing", "Prediction", cv.deviance$Type)

# CV iSDM vs hmSDM UF0 ####
plot.data <- cv.deviance %>%
  filter(Trial %in% c("iSDM", "UF0")) %>%
  select(Taxon, Type, Trial, std.deviance) %>%
  group_by(Trial, Type, Taxon) %>%
  summarise(MSD = mean(std.deviance)) %>%
  ungroup() %>%
  spread(Type, MSD)

ymax <- max(plot.data$Training)

plot.data$ymax <- ifelse(plot.data$Testing > ymax, TRUE, FALSE)

plot.data$Shape <- ifelse(plot.data$ymax, 24, 16)
plot.data$Testing <- ifelse(plot.data$ymax, max(plot.data$Training), plot.data$Testing)
plot.data$Shape <- as.factor(plot.data$Shape)
plot.data$n.present <- n.bdms[plot.data$Taxon]

g1 <- ggplot(plot.data)
g1 <- g1 + geom_point(aes(x = Training, y = Testing, colour = Trial, size = n.present, shape = Shape), alpha = 0.3)
g1 <- g1 + geom_abline(intercept = 0, slope = 1, color="black", size=1.25, alpha = 0.4)
g1 <- g1 + theme(strip.background=element_rect(fill="black"), 
                 strip.text=element_text(color="white", face="bold"), 
                 axis.text=element_text(size=18))
g1 <- g1 + theme_bw(base_size = 20)
g1 <- g1 + guides(colour = guide_legend(override.aes = list(size=6)), shape = guide_legend(override.aes = list(size=6)))
g1 <- g1 + labs(x = "Mean standardized deviance for calibration",
                y = "Mean standardized deviance\nfor prediction",
                colour = "Model",
                shape = "Deviance",
                size = "Number of\noccurrences")
g1 <- g1 + scale_colour_manual(values=c("UF0" = "#048504", "iSDM" = "#790FBF"))
g1 <- g1 + scale_size_continuous(range = c(2, 8))
g1 <- g1 + scale_shape_discrete(name  = "Deviance",
                                breaks=c("16", "24"),
                                labels=c("In range", "Out of range"))
g1 <- g1 + coord_cartesian(ylim=c(0, ymax))
g1

# CV max posterior ####
plot.data <- cv.deviance %>%
  select(Taxon, Type, Trial, Fold, std.deviance) %>%
  group_by(Trial, Type, Taxon) %>%
  summarise(MSD = mean(std.deviance)) %>%
  ungroup() %>%
  mutate(Type = factor(Type, levels=c("Training", "Testing")),
         Trial = factor(Trial, levels=c("iSDM", "UF0", "CF0", "CT0", "CT1", "CT2")))

g2 <- ggplot(plot.data)
g2 <- g2 + geom_boxplot(aes(x=Trial, y = MSD, fill=Type), show.legend=FALSE)
g2 <- g2 + theme_bw(base_size = 20)
g2 <- g2 + labs(y="Mean standardized deviance\n(based on maximum posterior)"
                # subtitle=expression(gamma==0~"and"~"u"==0~"during prediction for CT0, CT1, and CT2")
                )
g2 <- g2 + theme(axis.title.x = element_blank(),
                 plot.subtitle=element_text(size=14))
g2 <- g2 + ylim(c(0,1.5))
g2 <- g2 + scale_fill_brewer(palette = "Set1")
g2 



# CV posterior sample ####
# Calculate the mean standardized deviance based on the posterior sample
cv.FF0.sample <- cv.jsdm.sample(folder = "FF0")
cv.TF0.sample <- cv.jsdm.sample(folder = "TF0")
cv.TT0.sample <- cv.jsdm.sample(folder = "TT0")
cv.TT1.sample <- cv.jsdm.sample(folder = "TT1")
cv.TT2.sample <- cv.jsdm.sample(directory = "outputs/paper 1 submission 2 results/TT2", folder = "TT2")

cv.deviance.sample <- bind_rows(cv.FF0.sample$deviance, cv.TF0.sample$deviance, cv.TT0.sample$deviance, cv.TT1.sample$deviance, cv.TT2.sample$deviance)

cv.deviance.sample$Trial <- ifelse(cv.deviance.sample$Trial=="FF0", "UF0", cv.deviance.sample$Trial)
cv.deviance.sample$Trial <- ifelse(cv.deviance.sample$Trial=="FT0", "UT0", cv.deviance.sample$Trial)
cv.deviance.sample$Trial <- ifelse(cv.deviance.sample$Trial=="TF0", "CF0", cv.deviance.sample$Trial)
cv.deviance.sample$Trial <- ifelse(cv.deviance.sample$Trial=="TT0", "CT0", cv.deviance.sample$Trial)

cv.deviance.sample$Trial <- ifelse(cv.deviance.sample$Trial=="TT1", "CT1", cv.deviance.sample$Trial)
cv.deviance.sample$Trial <- ifelse(cv.deviance.sample$Trial=="TT2", "CT2", cv.deviance.sample$Trial)

# Get the mean standardized deviance
plot.data <- cv.deviance.sample %>%
  select(Taxon, Type, Trial, Fold, std.deviance) %>%
  group_by(Trial, Type, Taxon) %>%
  summarise(MSD = mean(std.deviance)) %>%
  ungroup() %>%
  mutate(Type = factor(Type, levels=c("Training", "Testing")),
         Trial = factor(Trial, levels=c("UF0", "CF0", "CT0", "CT1", "CT2")))

g3 <- ggplot(plot.data)
g3 <- g3 + geom_boxplot(aes(x=Trial, y = MSD, fill=Type))
g3 <- g3 + scale_fill_brewer(palette = "Set1")
g3 <- g3 + theme_bw(base_size = 20)
g3 <- g3 + theme(axis.title.x = element_blank())
g3 <- g3 + labs(y = "Mean standardized deviance\n(based on posterior sample)")

g3 <- g3 + ylim(c(0,1.5))
g3

# Arrange plots P1, P2, and P3 into a single figure
pdf('outputs/paper 1 submission 2 results/P1 - predictive performance.pdf', height=11, width=12)
bottom.row <- plot_grid(g2, g3, align="h", labels=c("b", "c"), label_size = 22)
plot_grid(g1, bottom.row, labels=c("a", ""), ncol=1, label_size = 22)
rm(bottom.row)
dev.off()

# plot.comm() [all taxa] ####
# Plot the marginal posterior of the community distributions and their
# respective taxon-specific parameters on one plot (i.e., see section 3.4 figure 5)
plot.comm(fit.FF0, 'P1 - plot_comm [all taxa]')

# Example taxa ####
# Plot the taxa selected as examples in section 3.4 figure 6
# Arrange the plots from prob.prob.taxon() and map.jsdm.pred.taxon() plots
g1 <- plot.prob.taxon(fit.FF0, "Gammaridae", legend=FALSE) # predicted probability vs explanatory variables
g2 <- map.jsdm.pred.taxon(fit.FF0, "Gammaridae", legend=TRUE) # geographic distribution of quality of fit
g3 <- map.jsdm.pred.taxon(fit.FF0, "Nemoura_minima", legend=TRUE) # geographic distribution of quality of fit
g4 <- map.jsdm.pred.taxon(fit.FF0, "Protonemura_lateralis", legend=TRUE) # geographic distribution of quality of fit

# Arrange the plots into a grid: no common legend!
# pdf('outputs/paper 1 extensions/P1 - maps example taxa.pdf', width=13, height=9.5)
# plot_grid(g1,g2,g3,g4, labels=c("a", "b", "c", "d"), align="hv")
# dev.off()

# Plot response curve and map for Gammaridae
svg('outputs/paper 1 submission 2 results/P1 maps example taxon Gammaridae.svg', width=13, height=9.5)
ggarrange(plotlist=list(g1,g2), ncol=2, nrow=1, legend="right",
          labels=c("(a)", "(b)"))
dev.off()

# Plot maps for the other two taxa
# svg('outputs/paper 1 submission 2 results/P1 maps example taxa.svg', width=13, height=9.5)
# ggarrange(plotlist=list(g3,g4), ncol=2, nrow=1, common.legend = TRUE, legend="right",
#           labels=c("(a)", "(b)", "(c)", "(d)"))
# dev.off()

# Richness ####
# Produce plots of predicted vs observed richness for the various models as shown in section 3.5
richness <- bind_rows(cv.FF0.sample$richness, cv.TF0.sample$richness, cv.TT0.sample$richness, cv.TT1.sample$richness, cv.TT2.sample$richness) %>%
  mutate(Trial = ifelse(Trial=="FF0", "UF0", Trial),
         # Trial = ifelse(Trial=="FT0", "UT0", Trial),
         Trial = ifelse(Trial=="TF0", "CF0", Trial),
         Trial = ifelse(Trial=="TT0", "CT0", Trial),
         Trial = ifelse(Trial=="TT1", "CT1", Trial),
         Trial = ifelse(Trial=="TT2", "CT2", Trial),
         Type = factor(Type, levels=c("Training", "Testing")),
         Trial = factor(Trial, levels=c("UF0", "CF0", "CT0", "CT1", "CT2"))) # "UT0" omitted

# Observed richness must be manually incorporated as cv.jsdm.sample() doesn't calculate it
richness.obs <- gather(sample.bdms, Taxon, Obs, -SiteId, -SampId)

# Get the taxonomic resolution of each taxon in the observed richness
taxonomy.bdms <- lapply(unique(richness.obs$Taxon), function(j){
  taxon.rank(j, print = FALSE)
})
taxonomy.bdms <- bind_rows(taxonomy.bdms)

species <- taxonomy.bdms$Taxon[taxonomy.bdms$Rank=="Species"]
families <- taxonomy.bdms$Taxon[taxonomy.bdms$Rank=="Family" | taxonomy.bdms$Rank=="Genus" | taxonomy.bdms$Rank=="Species"]

# Calculate observed species richness
richness.species <- richness.obs %>%
  as.tibble() %>%
  filter(Taxon %in% species) %>%
  group_by(SampId) %>%
  summarise(richness.obs = sum(Obs, na.rm = TRUE)) %>%
  mutate(Rank = "Species")

# Calculate observed family richness
richness.family <- richness.obs %>%
  as.tibble() %>%
  filter(Obs == 1 & Taxon %in% families) %>%
  left_join(taxonomy.bdms, by="Taxon") %>%
  group_by(SampId) %>%
  summarise(richness.obs = uniqueN(Family)) %>%
  mutate(Rank = "Family")

# Combine the observed family and species richness
richness.obs <- bind_rows(richness.family, richness.species)
rm(richness.species, richness.family)

# Calculate and plot richness of all models
plot.data <- richness %>%
  filter(Type=="Testing") %>%
  ungroup() %>%
  left_join(richness.obs, by=c("SampId", "Rank"))

mean.richness <- richness %>%
  filter(Type=="Testing") %>%
  group_by(Type, Trial, Rank, SampId) %>%
  summarise(mean.richness = mean(total.pred.richness)) %>%
  ungroup() %>%
  left_join(richness.obs, by=c("SampId", "Rank"))

# Figure 7b
g2 <- ggplot()
# Plot richness over all posterior subsamples
g2 <- g2 + geom_point(data = plot.data, aes(x = richness.obs, y = total.pred.richness), size = 0.25, color = "black", alpha = 0.1)
g2 <- g2 + geom_point(data = mean.richness, aes(x = richness.obs, y = mean.richness), size = 0.75, alpha = 0.4, color = "red")
g2 <- g2 + geom_abline(intercept = 0, slope = 1, color="black", size=1.25)
g2 <- g2 + facet_grid(Rank ~ Trial)
g2 <- g2 + theme_bw(base_size = 20)
g2 <- g2 + labs(x = "Observed richness",
                y = "Predicted richness")
g2 <- g2 + coord_cartesian(ylim=c(0,75))

# pdf('outputs/paper 1 extensions/P1 - richness.pdf', height=10.5, width=13)
# print(g2)
# dev.off()

# Table S1.9: provide summary tables for the family richness model
# Table S1.10: provide summary tables for the family richness model
data <- spread(richness.obs, Rank, richness.obs)
data <- left_join(data, predictors, by="SampId")
rmf <- glm.nb(Family ~ Temp + Temp2 + FV + F100m + LUD + IAR, data = data, link=log)
rms <- glm.nb(Species ~ Temp + Temp2 + FV + F100m + LUD + IAR, data = data, link=log)

library(broom)
rmf.tidy <- tidy(rmf)
rms.tidy <- tidy(rms)

rmf.tidy[,-1] <- apply(rmf.tidy[,-1],2,function(x){signif(x,2)})
rms.tidy[,-1] <- apply(rms.tidy[,-1],2,function(x){signif(x,2)})
rm(data)
# write.csv(rmf.tidy, 'outputs/paper 1 extensions/rmf_parameters.csv')
# write.csv(rms.tidy, 'outputs/paper 1 extensions/rms_parameters.csv')
# write.csv(glance(rmf), 'outputs/paper 1 extensions/rmf_model.csv')
# write.csv(glance(rms), 'outputs/paper 1 extensions/rms_model.csv')

# Calculate predicted richness of richness model based on 3-fold CV with the custom cv.rm() function
richness.rm <- cv.rm(predictors)

richness.rm <- richness.rm %>%
  group_by(SampId, Rank, Type) %>%
  summarise(richness.pred = mean(richness.pred)) %>%
  ungroup() %>%
  mutate(Trial = "Richness model") %>%
  left_join(richness.obs, by=c("SampId", "Rank"))

richness.family.FF0 <- cv.FF0$probability %>%
  left_join(taxonomy.bdms, by = "Taxon") %>%
  # Filter the obs/taxonomy and join the joint model probabilities 
  filter(!is.na(Obs), Rank == 'Family' | Rank == 'Genus' | Rank == 'Species') %>%
  group_by(SampId, Taxon, Type) %>%
  summarise(richness.pred = mean(Pred)) %>%
  ungroup() %>%
  left_join(taxonomy.bdms, by = "Taxon") %>%
  # Calculate the predicted richness by family at each sample
  group_by(SampId, Family, Type) %>%
  summarise(richness.pred = 1-prod(1-richness.pred)) %>%
  group_by(SampId, Type) %>%
  summarise(richness.pred = sum(richness.pred)) %>% 
  # Remove grouping, select columns, and add additional group information
  ungroup() %>%
  select(SampId, richness.pred, Type) %>%
  mutate(Rank = "Family", Trial = "UF0")

richness.species.FF0 <- cv.FF0$probability %>%
  left_join(taxonomy.bdms, by = "Taxon") %>%
  filter(!is.na(Obs), Rank == "Species") %>%
  group_by(SampId, Taxon, Type) %>%
  summarise(richness.pred = mean(Pred)) %>%
  group_by(SampId, Type) %>%
  summarise(richness.pred = sum(richness.pred)) %>%
  ungroup() %>%
  select(SampId, richness.pred, Type) %>%
  mutate(Rank = "Species", Trial = "UF0")

richness.FF0 <- bind_rows(richness.family.FF0, richness.species.FF0) %>% left_join(richness.obs, by = c("SampId", "Rank"))
rm(richness.family.FF0, richness.species.FF0)

table(colnames(richness.FF0) %in% colnames(richness.rm))

richness.FF0 <- richness.FF0[, colnames(richness.rm)]

plot.data <- bind_rows(richness.FF0, richness.rm)
plot.data <- filter(plot.data, Type == "Testing")

# Figure 7a
g1 <- ggplot(plot.data)
g1 <- g1 + geom_point(aes(x = richness.obs, y = richness.pred, color = Trial), alpha = 0.3)
# g1 <- g1 + facet_grid(Type ~ Rank)
g1 <- g1 + facet_grid(. ~ Rank)
g1 <- g1 + geom_abline(intercept = 0, slope = 1, color="black", size=1.25)
g1 <- g1 + theme_bw(base_size = 20)
g1 <- g1 + labs(y = "Predicted richness",
                x = "Observed richness",
                color = "Model")
g1 <- g1 + guides(colour = guide_legend(override.aes = list(size=6)))
g1 <- g1 + scale_color_brewer(palette = "Set1")

# Plot output as a high-resolution TIFF image since PDF filesize is too large
tiff("outputs/paper 1 submission 2 results/P1 - richness.tiff", height = 10.5, width = 13, units = 'in', compression = "lzw", res = 400)
plot_grid(g1, g2, labels=c("a", "b"), ncol=1, label_size = 20)
dev.off()

# APPENDIX ####
# Figure S1.1 ####
# Map was made in ArcGIS, here is the plot for the number of occurrences per taxon in the BDMs dataset
pdf('P1 - BDM prevalence.pdf', height = 10, width = 13)
par(cex=2.5)
plot(x = 1:length(n.bdms), y = n.bdms, axes=F, 
     xlab="Taxa (ordered by decreasing number of occurrences)", 
     ylab="Number of occurrences")
axis(1, c(0,50,100,150,200,245))
axis(2, c(0,100,200,300,400,500,580))
dev.off()

# Table S1.2 ####
# correlations of the selected explanatory variables
cor(predictors[,K])
write.csv(cor(predictors[,K]), 'outputs/paper 1 extensions/correlations.csv')

# Figure S1.2: map the selected explanatory variables
x <- prepare.inputs(K, sample.bdms, center = FALSE)
x <- gather(x, Variable, Value, -SiteId, -SampId)
x <- filter(x, Variable != "Temp2")
x$Trial <- 'BDM species'
map.inputs(x, 'P1 - maps influence factors')

# Table S1.7 ####
# produced manually from the variable selection section of this script

# Table S1.8 ####
# Effective number of parameters
ep.FF0 <- effective.parameters(folder = "FF0")
ep.TF0 <- effective.parameters(folder = "TF0")
ep.TT0 <- effective.parameters(folder = "TT0")
ep.TT1 <- effective.parameters(folder = "TT1")
ep.TT2 <- effective.parameters(directory = "outputs/paper 1 submission 2 results/TT2", folder = "TT2")


pd.FF0 <- calc.pd(ep.FF0)
pd.TF0 <- calc.pd(ep.TF0)
pd.TT0 <- calc.pd(ep.TT0)
pd.TT1 <- calc.pd(ep.TT1)
pd.TT2 <- calc.pd(ep.TT2) # TT2 includes ~1400 Inf deviance values out of 122500 samples

pd.data <- tibble(
  Model = c("UF0", "CF0", "CT0", "CT1", "CT2"),
  mean.deviance = c(pd.FF0$mean.deviance, pd.TF0$mean.deviance, pd.TT0$mean.deviance, pd.TT1$mean.deviance, pd.TT2$mean.deviance),
  mean.posterior.par.deviance = c(pd.FF0$mean.posterior.par.deviance, pd.TF0$mean.posterior.par.deviance, pd.TT0$mean.posterior.par.deviance, pd.TT1$mean.posterior.par.deviance, pd.TT2$mean.posterior.par.deviance),
  pD1 = c(pd.FF0$pD.1, pd.TF0$pD.1, pd.TT0$pD.1, pd.TT1$pD.1, pd.TT2$pD.1),
  pD2 = c(pd.FF0$pD.2, pd.TF0$pD.2, pd.TT0$pD.2, pd.TT1$pD.2, pd.TT2$pD.2)
)

write.csv(pd.data, "outputs/paper 1 submission 2 results/effective_parameters.csv", row.names = FALSE)

# Figure S1.3 ####
# relative importance of exp. variables based on the range of the linear predictor per taxon
# Linear predictor ####
z.FF0 <- linear.predictor(jsdm.FF0)
z.FF0$Trial <- "UF0"

z.TF0 <- linear.predictor(jsdm.TF0)
z.TF0$Trial <- "CF0"

z.TT0 <- linear.predictor(jsdm.TT0)
z.TT0$Trial <- "CT0"

z.TT1 <- linear.predictor(jsdm.TT1)
z.TT1$Trial <- "CT1"

z.TT2 <- linear.predictor(jsdm.TT2)
z.TT2$Trial <- "CT2"


# z.TT3 <- linear.predictor(jsdm.TT3)
# z.TT3$Trial <- "TT3"
# 
# z.TT4 <- linear.predictor(jsdm.TT4)
# z.TT4$Trial <- "TT4"

z.jsdm <- bind_rows(z.FF0, z.TF0, z.TT0, z.TT1, z.TT2)
# rm(z.FF0, z.TF0, z.TT0, z.TT1, z.TT2)

# Get the max and min z-values by variable and by trial
plot.data <- z.jsdm %>%
  group_by(Taxon, Variable, Trial) %>%
  summarise(z.min = min(z, na.rm=T), z.max = max(z, na.rm=T)) %>%
  ungroup() %>%
  mutate(z.range = z.max-z.min,
         # Variable = factor(Variable, levels=c("Temp", "FV", "F100m", "IAR", "LUD", "Site effect", "TT1", "TT2")),
         Trial = factor(Trial, levels=c("UF0", "CF0", "CT0", "CT1", "CT2"))
  )

z.jsdm %>% filter(Trial=="CT0") %>% count(Variable)
test <- filter(z.jsdm, Variable=="Site effect", Trial=="CT0")
test <- filter(plot.data, Variable=="Site effect", Trial=="CT0")

g <- ggplot(data = plot.data) 
g <- g + geom_boxplot(aes(y=z.range, fill=Variable))
g <- g + facet_grid(. ~ Trial)
g <- g + coord_cartesian(ylim=c(0,16)) 
g <- g + theme_bw(base_size = 24)
g <- g + labs(title="Range of linear predictor for all taxa",
              y = expression(paste("z"["range"])),
              fill = "Variable")
g <- g + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid.minor = element_blank())
g <- g + scale_fill_brewer(palette = "Set1")
g

pdf('outputs/paper 1 extensions/P1 - slopes.pdf', width = 15)
g
dev.off()

# Figure S1.4 ####
# Community and taxon-specific parameter plots
# Produced from plot.comm() [all taxa] section in this script
# plot.comm() [examples] ####

# Figure S1.5: community and taxon-specific parameter plots for selected taxa in UF0
# Modified plot.comm() code for specific taxa.
response.bdms <- extract.resp(fit.FF0)
beta.samples <- extract.beta(fit.FF0)
taxa <- c("Gammaridae", "Nemoura_minima", "Protonemura_lateralis")

pdf('outputs/paper 1 extensions/P1 - plot_comm [example taxa].pdf', onefile = TRUE)
par(cex=1.25)
for (k in 1:length(K)){
  variable <- K[k]
  responses <- response.bdms[response.bdms$Taxon %in% taxa, ]
  
  response <- responses[[variable]]
  names(response) <- responses$Taxon
  
  response[response==0] <- "grey55"
  response[response==1] <- "blue"
  response[response==-1] <- "red"
  cat("Plotting: ",variable,"\n")
  # Test temperature as starting variable
  samples <- beta.samples[Variable == variable,]
  
  # Find the maximum density among the posterior taxon-specific posterior parameters
  samples.sd <- samples %>%
    group_by(Taxon) %>%
    summarise(SD = sd(Value)) %>%
    arrange(SD) # arrange from min to max
  
  sd <- samples.sd[1,]$Taxon
  ymax.sample <- samples[Taxon == sd, ]
  ymax <- density(ymax.sample$Value)
  ymax <- max(ymax$y)
  
  # Plot the community parameter distribution
  mu <- fit.FF0$mu.beta.comm.maxpost[variable]
  sd <- fit.FF0$sigma.beta.comm.maxpost[variable]
  x <- seq(mu-4*sd,mu+4*sd,length.out=201)
  beta.comm.density <- dnorm(x, mu, sd)
  beta.taxa.maxpost.density <- density(fit.FF0$beta.taxa.maxpost[variable, ])
  
  # Match expressions to influence factors
  labels <- c("A10m" = expression(paste(beta["A10m"], " (1/%)")),
              "Temp" = expression(paste(beta["Temp"], " (1/", degree, "C)")),
              "Temp2" = expression(paste(beta["Temp"^2], " (1/", degree, "C"^2,")")),
              "FV" = expression(paste(beta["FV"], " (s/m)")),
              "bFRI" = expression(paste(beta["bFRI"], " (1/%)")),
              "FRI" = expression(paste(beta["FRI"], " (1/%)")),
              "F10m" = expression(paste(beta["F10m"], " (1/%)")),
              "F100m" = expression(paste(beta["F100m"], " (1/%)")),
              # "IAR" = expression(paste(beta["IAR"], " 1/(w"["c"]%*%"f"["c"],")")),
              "IAR" = expression(paste(beta["IAR"])),
              "Urban" = expression(paste(beta["Urban"], " (1/%)")),
              "LUD" = expression(paste(beta["LUD"], " (km"^2,"/CE)"))
  )
  
  plot(numeric(0), numeric(0),
       xlim = c(min(x), max(x)),
       ylim=c(0, ymax-(ymax*0.25)),
       xlab = labels[variable],
       ylab = "Density")
  abline(v=0)
  # Plot the taxon-specific parameters
  
  l <- c("G", "N", "P")
  
  for (j in 1:length(taxa)){
    taxon <- taxa[j]
    sample <- samples[Taxon==taxon,]
    sample <- sample$Value
    
    lines(density(sample), type="l", col=alpha(response[taxon], 1), lwd=1.25)
    text(x=mean(sample), y=max(density(sample)$y)+0.1*max(density(sample)$y), labels = l[j])
  }
  # Plot community parameter distribution
  lines(x, beta.comm.density, type="l", col = "grey50", lwd = 5)
  # Plot maximum posterior values over all taxa
  lines(beta.taxa.maxpost.density, type="l", col="black", lwd=2, lty='longdash')
}
dev.off()

# Figure S1.6: plot correlated beta_comm (community parameter) as ellipses and beta_taxa as points (see functions.R for plot.commcorr())
plot.commcorr(fit.TF0) 

# Figure S1.6 ####
# SD of beta_taxa based on exp. variable
# Extract the entire posterior beta sample
beta.samples <- extract.beta(fit.FF0)

# Fast aggregation of 10M row dataset
plot.data <- beta.samples %>%
  group_by(Taxon, Variable) %>%
  summarise(SD = sd(Value), Mean = mean(Value), rSD = SD/Mean) %>%
  mutate(n = n.bdms[Taxon], Label = factor(Variable, levels=c("Temp", "Temp2", "FV", "F100m",  "LUD", "IAR")), rSD = SD/abs(Mean))

levels(plot.data$Label) <- labeller(levels(plot.data$Label))

g <- ggplot(data=plot.data, aes(x = n, y = SD, size = n))
g <- g + geom_point(alpha = 0.5)
g <- g + facet_grid(Label ~ ., scales = "free", labeller=label_parsed)
g <- g + theme_bw(base_size = 14)
g <- g + theme(strip.background=element_rect(fill="grey"),strip.text=element_text(color="black", face="bold"),
               plot.title = element_text(hjust = 0.5, size = 12))
g <- g + labs(title = expression(paste("Standard deviation of posterior taxon-specific parameter distributions ", beta["kj"]^"taxa", " in UF0")),
              x = "Occurrence frequency",
              y = expression(paste("Standard deviation (", sigma[beta["kj"]^"taxa"],")")),
              size = "Occurrence frequency")
g <- g + scale_y_continuous(limits=c(0,NA))

pdf('outputs/paper 1 extensions/P1 - parameter SD.pdf', height = 12.5, width = 9)
print(g)
dev.off()

# Figure S1.7 ####
# Plot the relative uncertainty of beta_taxa
beta.samples <- extract.beta(fit.FF0)

plot.data <- beta.samples %>%
  group_by(Taxon, Variable) %>%
  summarise(SD = sd(Value), Mean = mean(Value), rSD = SD/Mean) %>%
  mutate(n = n.bdms[Taxon], Label = factor(Variable, levels=c("Temp", "Temp2", "FV", "F100m",  "LUD", "IAR")), rSD = SD/abs(Mean))

plot.data$Variable <- factor(plot.data$Variable, levels=K) # Order the labels

g <- ggplot(data=plot.data)
g <- g + geom_boxplot(aes(x=Variable, y=rSD, fill=Variable))
g <- g + coord_cartesian(ylim=c(0,10))
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(hjust = 0.5, size = 12), 
               axis.text = element_text(size = 12),
               axis.text.x = element_text(angle=45, hjust=1, vjust=0.5))
g <- g + scale_fill_brewer(palette = "Set1")
g <- g + guides(fill=FALSE)
g <- g + labs(title = expression(paste("Relative uncertainty of posterior ", beta["jk"]^"taxa", " in UF0")),
              x = "Explanatory variable",
              y = expression(paste("Relative standard deviation (", sigma[beta["jk"]^"taxa"]," / |",mu[beta["jk"]^"taxa"],"|)")))

pdf('outputs/paper 1 extensions/P1 - parameter uncertainty.pdf')
print(g)
dev.off()

# Figure S1.8 ####
# Warning: really bad code ahead...
# Extract the entire posterior beta sample
beta.samples <- extract.beta(fit.FF0)

beta.samples.quantiles <- beta.samples %>%
  group_by(Variable, Taxon) %>%
  summarise(quantile10=quantile(Value, 0.10), quantile90=quantile(Value, 0.90)) %>%
  setDT()
colnames(beta.samples.quantiles)<- c("Variable", "Taxon", "quantile10", "quantile90")

isdm.beta <- isdm$parameters
if ("Model" %in% colnames(isdm.beta)){
  isdm.beta$Model <- NULL
}
colnames(isdm.beta) <- c("Taxon", "Variable", "iSDM.parameter")


# Number of pages per taxa (ntpp)
pages <- 4
ntpp <- length(n.bdms)/pages
tpp <- split(n.bdms, ceiling(seq_along(n.bdms)/ntpp)) # bin the number of taxa per page
pdf(paste("outputs/paper 1 submission 2 results/P1 - parameter dotplots.pdf",sep=""), paper = "special", height = 10, width = 9, onefile = TRUE)
for (page in 1:pages){
  
  plot.data <- beta.samples.quantiles[beta.samples.quantiles$Taxon %in% names(tpp[page][[1]]), ]
  
  # Prepare the jSDM parameter values
  beta.max <- select(fit.FF0$parameters, Taxon, Variable, Parameter)
  colnames(beta.max) <- c("Taxon", "Variable", "jSDM.parameter")
  beta.max$Variable <- as.character(beta.max$Variable)
  
  # Join the quantiles, jSDM, and iSDM parameters together
  plot.data <- left_join(plot.data, beta.max, by = c("Taxon", "Variable"))
  plot.data <- left_join(plot.data, isdm.beta, by = c("Taxon", "Variable"))
  
  # Model (variable) and Parameter (value) gather the two columns, jSDM.parameter and iSDM.parameter
  # into a variable/value pair
  plot.data <- gather(plot.data, Model, Parameter, jSDM.parameter:iSDM.parameter)
  
  # Process iSDM outliers
  # Get min and max posterior parameter values within the 10th-90th quantiles
  lim.min <- aggregate(quantile10 ~ Variable, plot.data, min)
  vmin <- lim.min$quantile10; names(vmin) <- lim.min$Variable
  
  lim.max <- aggregate(quantile90 ~ Variable, plot.data, max)
  vmax <- lim.max$quantile90; names(vmax) <- lim.max$Variable
  
  # Set colors for each parameter
  # g <- g + scale_colour_manual(values=c("jSDM" = "#048504", "iSDM" = "#790FBF"))
  plot.data$Col <- "#048504"
  plot.data$Col[plot.data$Model == 'iSDM.parameter'] <- "#790FBF"
  plot.data$Col <- ifelse(plot.data$Parameter < vmin[plot.data$Variable], "#6B6B6B", plot.data$Col)
  plot.data$Parameter <- ifelse(plot.data$Parameter < vmin[plot.data$Variable], vmin[plot.data$Variable], plot.data$Parameter)
  
  plot.data$Col <- ifelse(plot.data$Parameter > vmax[plot.data$Variable], "#6B6B6B", plot.data$Col)
  plot.data$Parameter <- ifelse(plot.data$Parameter > vmax[plot.data$Variable], vmin[plot.data$Variable], plot.data$Parameter)
  rm(lim.min, lim.max, vmin, vmax)
  
  # Order labels by occurrence frequency
  plot.data$Labels <- factor(paste(plot.data$Taxon, ' - ', n.bdms[plot.data$Taxon]), levels = paste(names(sort(n.bdms)), ' - ', sort(n.bdms)))
  
  # Order variable facets and pass expressions for units in facet labels
  plot.data$Variable <- factor(plot.data$Variable, levels = c("Temp", "Temp2", "FV", "F100m", "IAR", "LUD"))
  levels(plot.data$Variable) <- labeller(c("Temp", "Temp2", "FV", "F100m", "IAR", "LUD"))
  
  # Build the plot
  g <- ggplot(plot.data)
  g <- g + geom_hline(yintercept=0, alpha=0.4)
  g <- g + geom_linerange(aes(x = Labels, ymin = quantile10, ymax = quantile90), color = 'black', alpha = 0.6)
  g <- g + geom_point(aes(x = Labels, y = Parameter, colour = Col), stroke=0, size = 2.5, alpha = 0.5)
  g <- g + facet_grid(. ~ Variable, scales = "free", labeller=label_parsed)
  g <- g + coord_flip()
  g <- g + theme_bw()
  g <- g + labs(title = "Taxon-specific parameters",
                subtitle = "MLEs (iSDM) and maximum posterior (UF0) with 10th-90th percentile interval",
                x = "Taxon and number of occurrences",
                y = "Value")
  g <- g + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  g <- g + scale_colour_identity()
  print(g)
}
dev.off()

# A.5 Community Properties ####
# Summarise the overall richness at different taxonomic levels
# Gather observations into narrow dataset
obs <- data.table(gather(sample.bdms, Taxon, Obs, -SiteId, -SampId))

rank <- lapply(unique(obs$Taxon), function(j){
  taxon.rank(j) # ID the taxonomic resolution (i.e., "rank") of each taxon
})
rank <- rbindlist(rank)

# Obtain all observations along with full taxonomy and taxonomic level of identification
obs <- merge(obs, rank, by = "Taxon", all.x = TRUE)

# Table: Community richness by taxonomic resolution
# Overall richness? EPT species richness? Family richness?
# Taxa richness
rank %>%
  summarise(n = n_distinct(Taxon)) 
# EPT species richness
rank %>%
  filter(Rank == 'Species' , Order == 'Ephemeroptera' | Order == 'Plecoptera' | Order == 'Trichoptera') %>%
  summarise(n = n_distinct(Species)) 
# Family richness (including lower levels)
rank %>%
  filter(Rank == "Family" | Rank == "Genus" | Rank == "Species") %>%
  summarise(n = n_distinct(Family)) 
# Family richness (excluding lower levels)
rank %>%
  filter(Rank == "Family") %>%
  summarise(n = n_distinct(Family)) 
# Number of taxa ID'ed at each taxonomic rank
rank %>%
  group_by(Rank) %>%
  summarise(n = n_distinct(Taxon))

# Figure S1.10 ####
# Map family and species richness
# Gather observations into narrow dataset
obs <- data.table(gather(sample.bdms, Taxon, Obs, -SiteId, -SampId))

rank <- lapply(unique(obs$Taxon), function(j){
  taxon.rank(j) # ID the taxonomic resolution (i.e., "rank") of each taxon
})
rank <- rbindlist(rank)

# Obtain all observations along with full taxonomy and taxonomic level of identification
obs <- merge(obs, rank, by = "Taxon", all.x = TRUE)

# Observed site richness
obs.site.richness.species <- obs %>%
  filter(Obs == 1, Rank == "Species") %>%
  group_by(SiteId) %>%
  mutate(ObsSpecies = n_distinct(Species)) %>%
  ungroup() %>%
  select(SiteId, ObsSpecies) %>%
  distinct()

obs.site.richness <- obs %>%
  filter(Obs == 1, Rank == "Family" | Rank == "Genus" | Rank == "Species") %>%
  group_by(SiteId) %>%
  mutate(ObsFamilies = n_distinct(Family)) %>%
  ungroup() %>% # remove grouping information from data frame
  select(SiteId, ObsFamilies) %>%
  distinct() %>%
  left_join(obs.site.richness.species, by = "SiteId") %>%
  left_join(inputs$xy, by = "SiteId")
rm(obs.site.richness.species)

g <- ggplot()
g <- g + geom_sf(data = inputs$ch, fill=NA, color="black")
g <- g + geom_sf(data = inputs$rivers.major, fill=NA, color="lightblue", show.legend = FALSE)
g <- g + geom_sf(data = inputs$lakes.major, fill="lightblue", color="lightblue", show.legend = FALSE)

g <- g + geom_point(data = obs.site.richness, aes(X, Y, size = ObsFamilies), alpha = 0.35)

g <- g + scale_radius(limits = c(0,40), breaks = seq(10, 40, 10))
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
g <- g + labs(size = "Observed family\nrichness")
g <- g + theme_void(base_size = 15)
g <- g + theme(plot.title = element_blank(),
               panel.grid.major = element_line(colour="transparent"))

pdf('P1 - map family richness.pdf', paper = 'special', width = 11, onefile = TRUE)
print(g)
dev.off()

g <- ggplot()
g <- g + geom_sf(data = inputs$ch, fill=NA, color="black")
g <- g + geom_sf(data = inputs$rivers.major, fill=NA, color="lightblue", show.legend = FALSE)
g <- g + geom_sf(data = inputs$lakes.major, fill="lightblue", color="lightblue", show.legend = FALSE)

g <- g + geom_point(data = obs.site.richness, aes(X, Y, size = ObsSpecies), alpha = 0.35)
g <- g + labs(size = "Observed species\nrichness")

g <- g + scale_radius(limits = c(0,30), breaks =seq(5, 30, 5))
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
g <- g + theme_void(base_size = 15)
g <- g + theme(plot.title = element_blank(),
               panel.grid.major = element_line(colour="transparent"))

pdf('P1 - map species richness.pdf', paper = 'special', width = 11, onefile = TRUE)
print(g)
dev.off()

# Figure S1.11 ####
# Map the site-specific effect (gamma_i)
dt <- tibble(SiteId = names(jsdm.TT0$gamma.maxpost), gamma.maxpost = jsdm.TT0$gamma.maxpost)
dt <- left_join(dt, inputs$xy, by="SiteId")
dt$color <- ifelse(dt$gamma.maxpost > 0, "Positive", "Negative")

g <- ggplot()
g <- g + geom_sf(data = inputs$ch, fill=NA, color="black")
g <- g + geom_sf(data = inputs$rivers.major, fill=NA, color="lightblue", show.legend = FALSE)
g <- g + geom_sf(data = inputs$lakes.major, fill="lightblue", color="lightblue", show.legend = FALSE)

g <- g + geom_point(data = dt, aes(X, Y, size = abs(gamma.maxpost), color = color), alpha = 0.35)
g <- g + scale_size_continuous(breaks=seq(0, 3.5, 0.5), limits=c(0,3.5), range = c(1, 8))

g <- g + scale_color_manual(values= c("Positive" = "blue", "Negative" = "red"))
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
g <- g + labs(title = "Site effect (CT0)",
              size = expression(paste(gamma["i"], " (absolute value)")),
              color = "")
g <- g + theme_void(base_size = 15)
g <- g + theme(plot.title = element_text(hjust = 0.5),
               panel.grid.major = element_line(colour="transparent"))
print(g)

pdf('P1 - map site effect.pdf', paper = 'special', width = 10.5, onefile = TRUE)
print(g)
dev.off()

# Figure S1.12 ####
# Pairwise scatterplot of exp. variables versus site-specific effect (gamma_i)
dt <- tibble(SiteId = names(jsdm.TT0$gamma.maxpost), gamma.maxpost = jsdm.TT0$gamma.maxpost)
dt <- left_join(dt, inputs$xy, by="SiteId")
dt$color <- ifelse(dt$gamma.maxpost > 0, "Positive", "Negative")

x <- prepare.inputs(K, sample.bdms, center = FALSE)
plot.data <- left_join(x, dt, by="SiteId")
pairs.panels(select(plot.data, -SiteId, -SampId), density = TRUE, scale=FALSE, hist.col="grey", cex.cor=1.5, cex.labels=1.5)

# Figure S1.13 ####
# Residual correlations between taxa (ordered by decreasing number of occurrences
# from top) for the joint model CT2 (two latent variables).
# This is standalone copy of Peter Reichert's script for extracting residual correlations between taxa and plotting the result as seen in paper 1.
dir.res <- "outputs/paper 1 submission 2 results/TT2/TT2"
dir.ana <- "outputs/paper 1 submission 2 results/TT2"
traceplot.nrow <- 10
traceplot.ncol <-  5

submodels <- "corr_site_2latvarsamp"

if ( !require("corrplot") ) { install.packages("corrplot"); library("corrplot") }
if ( !require("ellipse") )  { install.packages("ellipse");  library("ellipse") }
if ( !require("psych") )    { install.packages("psych");    library("psych") }
if ( !require("coda") )     { install.packages("coda");     library("coda") }
if ( !require("rstan") )    { install.packages("rstan");    library("rstan") }
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

for ( submodel in submodels )
{
  file.name.core <- paste("Inv_JSDM_",submodel,sep="")
  if ( !file.exists(paste(dir.res,"/",file.name.core,".RData",sep="")) )
  {
    print(paste("File \"",paste(dir.res,"/",file.name.core,".RData",sep=""),"not found"))
  }
  else
  {
    # load and extract data:
    load(file=paste(dir.res,"/",file.name.core,".RData",sep=""))
    
    res.orig <- res
    res.extracted.trace <- extract(res,permuted=FALSE,inc_warmup=FALSE)
    res.extracted       <- extract(res,permuted=TRUE,inc_warmup=FALSE)
    # print and save some characteristics:
    
    mod.char <- matrix(c("Model",submodel),nrow=1)
    mod.char <- rbind(mod.char,c("Number of taxa",ncol(occur.taxa)))
    mod.char <- rbind(mod.char,c("Number of sites",n.sites))
    mod.char <- rbind(mod.char,c("Number of samples",n.samples))
    mod.char <- rbind(mod.char,c("Number of ext. influence factors",ncol(data$x)))
    mod.char <- rbind(mod.char,c("Influence factors",paste(names(data$x),collapse=",")))
    n.par <- 0
    pars  <- ""
    for ( i in 1:(length(res.extracted)-1))
    {
      d <- dim(res.extracted[[i]])
      n <- 1; if ( length(d)>1 ) n <- prod(d[-1]); n.par <- n.par+n
      if ( nchar(pars)>0 ) pars <- paste(pars,",",sep="")
      pars <- paste(pars,names(res.extracted)[i],"(",n,")",sep="")
    }
    mod.char <- rbind(mod.char,c("Number of parameters",n.par))
    mod.char <- rbind(mod.char,c("Parameters",pars))
    
    # Convergence diagnostics:
    # traceplot
    # dims <- dim(res.extracted.trace)
    # pdf(paste(dir.ana,"/",file.name.core,"_traces.pdf",sep=""),width=8,height=12)
    # par(mfrow=c(traceplot.nrow,traceplot.ncol),mar=c(2,2,2,0.5)+0.2) # c(bottom, left, top, right)
    # for ( i in 1:ceiling(length(names(res))/(traceplot.nrow*traceplot.ncol)) )
    # {
    #   start <- (i-1)*traceplot.nrow*traceplot.ncol+1
    #   end   <- min(start+traceplot.nrow*traceplot.ncol-1,length(names(res)))
    #   for ( j in start:end )
    #   {
    #     plot(numeric(0),numeric(0),type="n",cex.axis=0.8,
    #          xlim=c(0,dims[1]),ylim=range(res.extracted.trace[,,j]),xlab="",ylab="",
    #          main=dimnames(res.extracted.trace)[[3]][j])
    #     for ( k in 1:dims[2] ) lines(1:dims[1],res.extracted.trace[,k,j],col=k)
    #   }
    # }
    # dev.off()
    
    # some maximum posterior results:
    cat("Model",submodel,"\n")
    cat("=====\n\n")
    ind.maxpost <- which.max(res.extracted[["lp__"]])
    
    mu_beta_maxpost <- res.extracted$mu_beta_comm[ind.maxpost,]
    names(mu_beta_maxpost) <- colnames(data$x)
    sigma_beta_maxpost <- res.extracted$sigma_beta_comm[ind.maxpost,]
    names(sigma_beta_maxpost) <- colnames(data$x)
    print(mu_beta_maxpost)
    print(sigma_beta_maxpost)
    
    alpha_maxpost <- res.extracted$alpha_taxa[ind.maxpost,]
    names(alpha_maxpost) <- colnames(data$y)
    beta_maxpost  <- res.extracted$beta_taxa[ind.maxpost,,]
    colnames(beta_maxpost) <- colnames(data$y)
    rownames(beta_maxpost) <- colnames(data$x)
    
    corr_maxpost <- diag(length(mu_beta_maxpost))
    if ( comm.corr )
    {
      corr_maxpost <- res.extracted$corrfact_comm[ind.maxpost,,] %*% t(res.extracted$corrfact_comm[ind.maxpost,,])
      colnames(corr_maxpost) <- colnames(data$x)
      rownames(corr_maxpost) <- colnames(data$x)
      print(corr_maxpost)
      corrplot(corr_maxpost)
      
      
      cov_maxpost <- diag(sigma_beta_maxpost) %*% corr_maxpost %*% diag(sigma_beta_maxpost)
    }
    
    panel.ellipse <- function(x,y,...)
    {
      # decode dimension, variable index, mean and covariance information from the scatterplot data
      # (encoded with a factor to avoid scale distortions of the plots as all elements are used to 
      # determine the plot extension)
      fact <- x[1]
      n <- x[2]/fact
      ind.x <- round(x[3]/fact)
      ind.y <- round(y[3]/fact)
      mu.x  <- x[4]/fact
      mu.y  <- y[4]/fact
      sd.x  <- x[5]/fact
      sd.y  <- y[5]/fact
      corr  <- x[5+ind.y]/fact
      points(x[-(1:(5+n))],y[-(1:(5+n))],...)
      abline(h=0,v=0)
      e <- ellipse(x=matrix(c(1,corr,corr,1),nrow=2),scale=c(sd.x,sd.y),centre=c(mu.x,mu.y),level=0.9)
      lines(x=e[,1],y=e[,2])
    }
    # encode dimension, variable index, mean and covariance information into the scatterplot data
    # (use a factor to avoid scale distortions of the plots as all elements are used to determine
    # the plot extension)
    fact <- 0.001
    cov.info <- rbind(rep(fact,length(mu_beta_maxpost)),
                      fact*rep(length(mu_beta_maxpost),length(mu_beta_maxpost)),
                      fact*(1:length(mu_beta_maxpost)),
                      fact*mu_beta_maxpost,
                      fact*sigma_beta_maxpost,
                      fact*corr_maxpost)
    pdf(paste(dir.ana,"/",file.name.core,"_scatter.pdf",sep=""),width=12,height=12)
    pairs(rbind(cov.info,t(beta_maxpost)),
          pch=19,cex=0.5,main="", labels = c("Temp", "FV", "F100m", "LUD", "IAR", "Temp^2"),
          lower.panel=panel.ellipse,upper.panel=panel.ellipse)
    dev.off()
    
    cat("\n\n")
    
    # write short summary of model and results:
    # cat("Model",submodel,"\n")
    # cat("========\n\n")
    # print(mod.char)
    # write.table(mod.char,
    #             paste(dir.ana,"/",file.name.core,"_char.dat",sep=""),
    #             row.names=FALSE,col.names=FALSE,sep="\t")
    
    # calculate taxa correlation matrix:
    iter.eff <- round((res@sim$iter-res@sim$warmup)/res@sim$thin)*res@sim$chains
    if ( n.latent > 0 )
    {
      if ( n.latent == 1 )
      {
        cov.resid  <- array(0,dim=c(iter.eff,n.taxa,n.taxa))
        corr.resid <- array(0,dim=c(iter.eff,n.taxa,n.taxa))
        for ( i in 1:iter.eff ) 
        {
          cov.resid[i,,]  <- res.extracted$beta_lat[i,] %*% t(res.extracted$beta_lat[i,])
          corr.resid[i,,] <- cov2cor(cov.resid[i,,])
        }
      }
      else
      {
        cov.resid  <- array(0,dim=c(iter.eff,n.taxa,n.taxa))
        corr.resid <- array(0,dim=c(iter.eff,n.taxa,n.taxa))
        for ( i in 1:iter.eff ) 
        {
          cov.resid[i,,]  <- t(res.extracted$beta_lat[i,,]) %*% res.extracted$beta_lat[i,,]
          corr.resid[i,,] <- cov2cor(cov.resid[i,,])
        }
      }
      cov.resid.maxpost  <- cov.resid[ind.maxpost,,]
      corr.resid.maxpost <- corr.resid[ind.maxpost,,]
      # print(corr.resid.maxpost[1:10,1:10])
      n <- apply(data$y, 2, function(j){
        sum(abs(j), na.rm=TRUE)
      })
      
      colnames(corr.resid.maxpost) <- names(n)
      rownames(corr.resid.maxpost) <- names(n)
      
      plot.data <- corr.resid.maxpost
      n <- sort(n, decreasing = TRUE)
      
      plot.data <- plot.data[, names(n)]
      plot.data <- plot.data[names(n), ]
      
      # corrplot(plot.data, method="square", type="lower", tl.pos='n', outline=FALSE, addgrid.col = NA)
      
      pdf('outputs/paper 1 submission 2 results/P1 - correlation matrix CT2.pdf', width = 12, height = 13)
      corrplot(plot.data, method="square", type="lower", tl.pos='n', outline=FALSE, addgrid.col = NA, tl.cex=4, cl.cex = 2)
      dev.off()
    }
  }
}

# Appendix S2-S6 ####
map.isdm(isdm, "P1 - maps iSDM") # Appendix S2
plot.prob(isdm, "P1 - prob iSDM") # Appendix S3

map.jsdm.pred(jsdm = fit.FF0, "P1 - maps UF0") # Appendix S4
plot.prob(jsdm = fit.FF0, "P1 - prob UF0") # Appendix S5

# Appendix S6
# Plot significant responses of all taxa in UF0
response.bdms <- extract.resp(fit.UF0)

btr.all <- response.bdms
btr.all <- as.data.table(btr.all)
btr.all <- arrange(btr.all, desc(n))
btr.all$TaxonLabel <- factor(paste(btr.all$Taxon, " - ", btr.all$n), levels=paste(names(sort(n.bdms)), " - ", sort(n.bdms)))

plot.data <- gather(btr.all, Variable, Value, -Taxon, -TaxonLabel, -n)
rm(btr.all)

plot.data$VariableLabel <- factor(plot.data$Variable, levels=c("Temp", "Temp2", "FV", "F10m", "IAR", "Urban", "LUD"))

g <- ggplot(plot.data, aes(x = VariableLabel, y = TaxonLabel))
g <- g + geom_tile(aes(fill = as.factor(Value)), colour = "white")
g <- g + scale_fill_manual(values=c("tomato3", "snow3", "royalblue4"))
g <- g + theme_minimal(base_size = 15)
g <- g + labs(fill = "Response",
              y = "Taxon and prevalence",
              x = "Explanatory Variable")
pdf('P1 - FF0 heatmap ALL.pdf', paper='special', height=56, width=8.5)
print(g)
dev.off()


# Map probabilities vs observations in individual models
# Get the mean standardized deviance
plot.data <- cv.deviance %>%
  select(Taxon, Type, Trial, std.deviance) %>%
  group_by(Trial, Type, Taxon) %>%
  summarise(MSD = mean(std.deviance)) %>%
  ungroup() %>%
  # spread(Type, MSD) %>%
  mutate(n = n.bdms[Taxon])

# plot.data[is.infinite(plot.data)] <- NA
g <- ggplot(plot.data)
# g <- g + geom_point(aes(x=Training, y=Testing, size=n, color=Trial))
g <- g + geom_boxplot(aes(x=Trial, y = MSD, fill=Type))
g <- g + scale_fill_brewer(palette = "Set1")
g <- g + labs(title="predictive performance under k-fold cross-validation",
              y="Mean standardized deviance")
g <- g + ylim(c(0,2.5))
g

plot.data <- cv.deviance %>%
  select(Taxon, Type, Trial, std.deviance) %>%
  group_by(Trial, Type, Taxon) %>%
  summarise(MSD = mean(std.deviance)) %>%
  ungroup() %>%
  spread(Type, MSD) %>%
  filter(Trial %in% c("FF0", "iSDM"))

ymax <- max(plot.data$Training)

# cv.plot$Infinite <- is.infinite(cv.plot$Testing)
plot.data$ymax <- ifelse(plot.data$Testing > ymax, TRUE, FALSE)

plot.data$Shape <- ifelse(plot.data$ymax, 24, 16)
plot.data$Testing <- ifelse(plot.data$ymax, max(plot.data$Training), plot.data$Testing)
plot.data$Shape <- as.factor(plot.data$Shape)
plot.data$n.present <- n.bdms[plot.data$Taxon]

g <- ggplot(plot.data, aes(x = Training, y = Testing, colour = Trial, size = n.present, shape = Shape))
g <- g + geom_point(alpha = 0.3)
g <- g + geom_abline(intercept = 0, slope = 1, color="black", size=1.25, alpha = 0.4)
g <- g + theme(strip.background=element_rect(fill="black"), strip.text=element_text(color="white", face="bold"), axis.text=element_text(size=18))
g <- g + theme_bw(base_size = 18)
g <- g + guides(colour = guide_legend(override.aes = list(size=6)), shape = guide_legend(override.aes = list(size=6)))
g <- g + labs(x = "Mean standardized deviance for calibration",
              y = "Mean standardized deviance for prediction",
              colour = "Model",
              shape = "Deviance",
              size = "Prevalence")
g <- g + scale_colour_manual(values=c("FF0" = "#048504", "iSDM" = "#790FBF"))
g <- g + scale_size_continuous(range = c(2, 8))
g <- g + scale_shape_discrete(name  = "Deviance",
                              breaks=c("16", "24"),
                              labels=c("In range", "Out of range"))
g <- g + coord_cartesian(ylim=c(0, ymax))
print(g)

library(ggalt)
plot.data$TaxonLabel <- factor(plot.data$Taxon, levels=c(names(sort(n.bdms, decreasing=FALSE))))
plot.data$TrialLabel <- as.factor(plot.data$Trial)
levels(plot.data$TrialLabel)[levels(plot.data$TrialLabel)=="FF0"] <- "Uncorrelated"
levels(plot.data$TrialLabel)[levels(plot.data$TrialLabel)=="TF0"] <- "Correlated"

g <- ggplot(plot.data)
g <- g + geom_dumbbell(aes(x=Training, xend=Testing, y=TaxonLabel, group=TrialLabel, color=TrialLabel), size=1.25)
g <- g + labs(title = "Predictive performance w/ and w/o correlated community parameters",
              x = "Mean standardized deviance",
              y = "Taxa (ordered by occurrence frequency)")
g <- g + theme(axis.text.x = element_blank())
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
g <- g + scale_color_brewer(palette = "Set1")
g <- g + coord_flip()
g


plot.data <- cv.jsdm.deviance %>%
  select(Taxon, Type, Trial, std.deviance) %>%
  group_by(Trial, Type, Taxon) %>%
  summarise(MSD = mean(std.deviance)) %>%
  ungroup()

g <- ggplot(plot.data)
g <- g + geom_boxplot(aes(x = Type, y = MSD, fill=Trial))
g <- g + scale_color_brewer(palette = "Set1")
g <- g + labs(y = "Mean standardized deviance",
              fill = "Model type")
g

# Map x_lat
# Problem: for multiple latent variables, multiple maps are generated but they size their points differently. Solution would combine geom_sf with facet_grid, which requires editing the input object
plot.data <- as.tibble(melt(fit.TT2$x.lat.maxpost))
colnames(plot.data) <- c("SampId", "x.lat", "Value")
plot.data$SiteId <- fit.TT2$sites[plot.data$SampId]
plot.data <- left_join(plot.data, inputs$xy, by="SiteId")

plot.data$abs.value <- abs(plot.data$Value)
plot.data$Color <- ifelse(plot.data$Value > 0, 1, 0)
plot.data$Color <- as.factor(plot.data$Color)

plot.data1 <- filter(plot.data, x.lat==1)
g1 <- ggplot()
g1 <- g1 + geom_sf(data = inputs$ch, fill=NA, size=1.25, color="black", show.legend = FALSE)
g1 <- g1 + geom_sf(data = inputs$rivers.major, fill=NA, color="lightblue", show.legend = FALSE)
g1 <- g1 + geom_sf(data = inputs$lakes.major, fill="lightblue", color="lightblue", show.legend = FALSE)

g1 <- g1 + geom_point(data = plot.data1, aes(x=X, y=Y, size = abs.value, color = Color), alpha=0.35)
g1 <- g1 + labs(title = expression(paste(xi[paste("i","t"["i"],"l")],", l = 1")),
                color = "",
                size = "Absolute\nvalue")
g1 <- g1 + theme_void(base_size = 15)
g1 <- g1 + theme(panel.grid.major = element_line(colour="transparent"))
g1 <- g1 + guides(colour = guide_legend(override.aes = list(size=6)))
g1 <- g1 + scale_color_manual(values=c("0" = "#FF0000", "1" = "#0077FF"), labels = c("Negative", "Positive"))
g1 <- g1 + scale_radius(range = c(2, 8))

# plot(c(0,0), main = expression(paste(xi[paste("i","t"["i"],"l")],", l=2")))
plot.data2 <- filter(plot.data, x.lat==2)
g2 <- ggplot()
g2 <- g2 + geom_sf(data = inputs$ch, fill=NA, size=1.25, color="black", show.legend = FALSE)
g2 <- g2 + geom_sf(data = inputs$rivers.major, fill=NA, color="lightblue", show.legend = FALSE)
g2 <- g2 + geom_sf(data = inputs$lakes.major, fill="lightblue", color="lightblue", show.legend = FALSE)

g2 <- g2 + geom_point(data = plot.data2, aes(x=X, y=Y, size = abs.value, color = Color), alpha=0.35)
g2 <- g2 + labs(title = expression(paste(xi[paste("i","t"["i"],"l")],", l = 2")),
                color = "",
                size = "Absolute\nvalue")
g2 <- g2 + theme_void(base_size = 15)
g2 <- g2 + theme(panel.grid.major = element_line(colour="transparent"))
g2 <- g2 + guides(colour = guide_legend(override.aes = list(size=6)))
g2 <- g2 + scale_color_manual(values=c("0" = "#FF0000", "1" = "#0077FF"), labels = c("Negative", "Positive"))
g2 <- g2 + scale_radius(range = c(2, 8))

pdf('outputs/paper 1 extensions/P1 - maps latent variable.pdf', width = 10, paper='a4r', onefile = TRUE)
print(g1)
print(g2)
dev.off()

# Plot the latent variable and its significance per taxon
beta.lat <- jsdm.TT1$TT1$beta.lat
beta.lat <- as.tibble(beta.lat)
row.names(beta.lat) <- 1:nrow(beta.lat)
colnames(beta.lat) <- colnames(jsdm.TT1$TT1$occur.taxa)

# Identify the taxon with the lowest SD for beta_lat
beta.lat.sd <- apply(beta.lat, 2, sd)
beta.lat.sd <- sort(beta.lat.sd, decreasing = FALSE)
beta.lat.sd.taxon <- names(which.min(beta.lat.sd))

beta.lat <- melt(beta.lat)
colnames(beta.lat) <- c("Taxon", "Value")
beta.lat <- as.tibble(beta.lat)

beta.lat.ymax <- filter(beta.lat, Taxon==beta.lat.sd.taxon)
ymax <- max(density(beta.lat.ymax$Value)$y)

pdf('outputs/paper 1 extensions/P1 - latent variables.pdf', onefile = TRUE)
plot(numeric(0), numeric(0),
     xlim = c(min(beta.lat$Value), max(beta.lat$Value)),
     ylim=c(0, ymax),
     xlab = "Marginal posterior beta_lat",
     ylab = "Density")
abline(v=0)
rm(beta.lat.sd, beta.lat.sd.taxon, beta.lat.ymax, ymax)
taxa <- occur.freq(jsdm.TT1$TT1$occur.taxa)

for (j in 1:length(taxa)){
  taxon <- names(taxa)[j]
  sample <- filter(beta.lat, Taxon==taxon)
  
  # Fill matrix: beta.taxa.response
  significance <- quantile(sample$Value, probs = c(0.05, 0.95))
  
  # If 5th quantile greater than 0, set positive
  # If 95th quantile less than 0, set negative
  if (significance[1] > 0){ # significant positive
    sig <- "blue"
  }
  if (significance[2] < 0){ # significant negative
    sig <- "red"
  }
  # If posterior is !(positive) AND !(negative), set grey
  if (!(significance[1] > 0) & !(significance[2] < 0)){
    sig <- "grey55"
  }
  
  lines(density(sample$Value), type="l", col=alpha(sig, 0.30), lwd=1)
  # cat("Taxon: ", taxon, " | significance: ", sig, "\n")
}
dev.off()

# Map number of significant interactions: experimental, not included in functions.R script
extract.lat.resp <- function(jsdm){
  n <- occur.freq(jsdm$occur.taxa)
  sites <- jsdm$env.cond$SiteId
  
  x.lat <- jsdm$x.lat
  beta.lat <- jsdm$beta.lat
  
  significance.beta.lat <- apply(beta.lat, 2, function(j){
    quantiles <- quantile(j, probs = c(0.05, 0.95))
    # If 5th quantile greater than 0, set positive
    # If 95th quantile less than 0, set negative
    if (quantiles[1] > 0){ # significant positive
      significance <- 1
    }
    if (quantiles[2] < 0){ # significant negative
      significance <- -1
    }
    # If posterior is !(positive) AND !(negative), set grey
    if (!(quantiles[1] > 0) & !(quantiles[2] < 0)){
      significance <- 0
    }
    return(significance)
  })
  names(significance.beta.lat) <- names(n)
  
  significance.x.lat <- apply(x.lat, 2, function(i){
    quantiles <- quantile(i, probs = c(0.05, 0.95))
    # If 5th quantile greater than 0, set positive
    # If 95th quantile less than 0, set negative
    if (quantiles[1] > 0){ # significant positive
      significance <- 1
    }
    if (quantiles[2] < 0){ # significant negative
      significance <- -1
    }
    # If posterior is !(positive) AND !(negative), set grey
    if (!(quantiles[1] > 0) & !(quantiles[2] < 0)){
      significance <- 0
    }
    return(significance)
  })
  names(significance.x.lat) <- names(jsdm$x.lat.maxpost)
  
  output <- list("significance.x.lat" = significance.x.lat,
                 "significance.beta.lat" = significance.beta.lat)
  return(output)
}

jsdm.TT1$significance <- extract.lat.resp(jsdm.TT1)

# plot.data <- tibble(SiteId = names(jsdm.TT1$significance$significance.x.lat),
#                         x.lat = jsdm.TT1$TT1$x.lat.maxpost,
#                         sig.x.lat = jsdm.TT1$significance$significance.x.lat,
#                         sig.beta.lat.positive = length(jsdm.TT1$significance$significance.beta.lat[jsdm.TT1$significance$significance.beta.lat==1]),
#                         sig.beta.lat.negative = length(jsdm.TT1$significance$significance.beta.lat[jsdm.TT1$significance$significance.beta.lat==-1]),
#                         sig.beta.lat.neutral = length(jsdm.TT1$significance$significance.beta.lat[jsdm.TT1$significance$significance.beta.lat==0])
#                         )

# Parameter uncertainty: Compare the parameter uncertainty of FF0 and TF0
beta.samples.FF0 <- extract.beta(jsdm.FF0)
beta.samples.TF0 <- extract.beta(jsdm.TF0)

plot.data <- bind_rows(beta.samples.FF0, beta.samples.TF0) %>%
  group_by(Taxon, Variable, Trial) %>%
  summarise(SD = sd(Value), Mean = mean(Value)) %>%
  mutate(n = n.bdms[Taxon], 
         VariableLabel = factor(Variable, levels=c("Temp", "Temp2", "FV", "F100m",  "IAR", "LUD")),
         relativeSD = SD/abs(Mean))

# levels(plot.data$VariableLabel) <- labeller(levels(plot.data$VariableLabel))
g <- ggplot(data=plot.data)
g <- g + geom_boxplot(aes(x=Variable, y=relativeSD, fill=Variable))
g <- g + coord_cartesian(ylim=c(0,10))
g <- g + facet_grid(.~Trial)
g <- g + theme_bw()
# g <- g + theme(plot.title = element_text(hjust = 0.5, size = 12), 
#                axis.text = element_text(size = 12),
#                axis.text.x = element_text(angle=45, hjust=1, vjust=0.5))
g <- g + scale_fill_brewer(palette = "Set1")
g <- g + guides(fill=FALSE)
g <- g + labs(title = expression(paste("Relative uncertainty of posterior taxon-specific parameter distributions ", beta["jk"]^"taxa")),
              x = "Explanatory variable",
              y = expression(paste("Relative standard deviation (", sigma[beta["jk"]^"taxa"]," / |",mu[beta["jk"]^"taxa"],"|)")))

