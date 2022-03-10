# variable selection ####

vs.bdms <- extract.vsr("outputs/paper 2 results/variable selection/variable_selection_bdms", sample.bdms)
vs.bdmf <- extract.vsr("outputs/paper 2 results/variable selection/variable_selection_bdmf_submodel", sample.bdmf)

vs.invf <- extract.vsr("outputs/paper 2 results/variable selection/variable_selection_invf", sample.invf)
vs.invf.complex <- extract.vsr("outputs/paper 2 results/variable selection/variable_selection_invf_complex", sample.invf)

vs.invfp <- extract.vsr("outputs/paper 2 results/variable selection/variable_selection_invf_plateau_submodel", sample.invfp)

mc.bdms <- uniqueN(vs.bdms$Model)
mc.bdmf <- uniqueN(vs.bdmf$Model)
mc.invf <- uniqueN(vs.invf$Model)
mc.invfc <- uniqueN(vs.invf.complex$Model)

mc.invfp <- uniqueN(vs.invfp$Model)

vs.invf$ExpVar <- sapply(vs.invf$Model, function(i){
  i <- strsplit(i," ")[[1]]
  length(i)
})

vs.invf.complex$ExpVar <- sapply(vs.invf.complex$Model, function(i){
  i <- strsplit(i," ")[[1]]
  length(i)
})

unique(vs.invf$ExpVar)
unique(vs.invf.complex$ExpVar)

vs.invf.complex <- filter(vs.invf.complex, ExpVar > 9)
mc.invfc <- uniqueN(vs.invf.complex$Model)
cat("Number of models:", mc.bdmf + mc.invfp + mc.invf + mc.invfc)


# deploy.jsdm() ####
K <- c("A10m", "IAR", "LUD", "Urban", "bFRI", "FRI", "FV", "WV", "Temp", "Temp2")
deploy.jsdm(K, sample.bdms, "bdms", center=T, cv=0)
deploy.jsdm(K, sample.bdms, "bdms_train1", center=T, cv=1)
deploy.jsdm(K, sample.bdms, "bdms_train2", center=T, cv=2)
deploy.jsdm(K, sample.bdms, "bdms_train3", center=T, cv=3)

deploy.jsdm(K, sample.bdmf, "bdmf", center=T, cv=0)
deploy.jsdm(K, sample.bdmf, "bdmf_train1", center=T, cv=1)
deploy.jsdm(K, sample.bdmf, "bdmf_train2", center=T, cv=2)
deploy.jsdm(K, sample.bdmf, "bdmf_train3", center=T, cv=3)

deploy.jsdm(K, sample.invf, "invf", center=T, cv=0)
deploy.jsdm(K, sample.invf, "invf_train1", center=T, cv=1)
deploy.jsdm(K, sample.invf, "invf_train2", center=T, cv=2)
deploy.jsdm(K, sample.invf, "invf_train3", center=T, cv=3)

deploy.jsdm(K, sample.invfp, "invfp", center=T, cv=0)
deploy.jsdm(K, sample.invfp, "invfp_train1", center=T, cv=1)
deploy.jsdm(K, sample.invfp, "invfp_train2", center=T, cv=2)
deploy.jsdm(K, sample.invfp, "invfp_train3", center=T, cv=3)

# occur.freq(sample) ####
n.bdmf <- occur.freq(sample.bdmf)
n.bdms <- occur.freq(sample.bdms)
n.invf <- occur.freq(sample.invf)
n.invfp <- occur.freq(sample.invfp)

# get.taxonomy(sample) ####
taxonomy.bdmf <- get.taxonomy(sample.bdmf)
taxonomy.bdms <- get.taxonomy(sample.bdms)
taxonomy.invf <- get.taxonomy(sample.invf)
taxonomy.invfp <- get.taxonomy(sample.invfp)
taxonomy.ept <- taxonomy.bdms[Order %in% c("Ephemeroptera", "Plecoptera", "Trichoptera"),]

# extract.jsdm(jsdm) ####
# Results: paper 2 BDM family/species
# fit.FF0 <- extract.jsdm('outputs/paper 1 extensions', 'FF0')
directory <- 'outputs/paper 2 results/full model'
folder <- 'FF0_bdmf'
full.path <- paste(getwd(), directory, folder, sep="/")

fit.bdmf <- extract.jsdm('outputs/paper 2 results/full model', 'FF0_bdmf')
fit.bdms <- extract.jsdm('outputs/paper 2 results/full model', 'FF0_bdms')

# Calibration: combined families
fit.invf <- extract.jsdm('outputs/paper 2 results/full model', 'FF0_invf')
fit.invfp <- extract.jsdm('outputs/paper 2 results/full model', 'FF0_invfp')

# cv.jsdm(jsdm) ####
cv.bdms <- cv.jsdm('outputs/paper 2 results/full model', 'FF0_bdms')
cv.bdmf <- cv.jsdm('outputs/paper 2 results/full model', 'FF0_bdmf')

cv.cfch <- cv.jsdm('outputs/paper 2 results/full model', 'FF0_invf')
cv.cfp <- cv.jsdm('outputs/paper 2 results/full model', 'FF0_invfp')

# plot.comm(jsdm) ####
plot.comm(fit.bdmf, 'outputs/paper 2 results/P2 parameters bdmf')
plot.comm(fit.bdms, 'outputs/paper 2 results/P2 parameters bdms')
plot.comm(fit.invf, 'outputs/paper 2 results/P2 parameters invf')
plot.comm(fit.invfp, 'outputs/paper 2 results/P2 parameters invfp')

# RESULTS ####
# Figure 1 ####
# Geographic distribution of federal and cantonal biomonitoring sites in Switzerland
# This map was made in ArcGIS and is relatively undocumented but extremely simple to make
# Table 1: obtained from the sample.invf (CFCH), sample.invfp (CFp), sample.bdmf (BDMf), and sample.bdms (BDMs) datasets

# Figure 2 ####
# Plot the Strahler stream order frequencies for each dataset
stream.order.bdm <- prepare.inputs("FLOZ", y = sample.bdms, center = FALSE)
stream.order.bdm$Trial <- "SGf/SGs"
stream.order.invf <- prepare.inputs("FLOZ", y = sample.invf, center = FALSE)
stream.order.invf$Trial <- "SCf"

stream.order.invfp <- prepare.inputs("FLOZ", y = sample.invfp, center = FALSE)
stream.order.invfp$Trial <- "PCf"

stream.order <- bind_rows(stream.order.bdm, stream.order.invf, stream.order.invfp)
rm(stream.order.bdm, stream.order.invf, stream.order.invfp)

plot.data <- select(stream.order, -SampId)
plot.data <- unique(plot.data)

plot.data <- plot.data %>%
  group_by(Trial, FLOZ) %>%
  summarise(Frequency = dplyr::n()) %>%
  ungroup()
plot.data$Trial <- factor(plot.data$Trial, levels = c("SGf/SGs", "SCf", "PCf"))

g <- ggplot()
g <- g + geom_line(data=plot.data, aes(x=FLOZ, y = Frequency, color=Trial), size= 3)
g <- g + geom_point(data=plot.data, aes(x=FLOZ, y = Frequency), color="black", size=3)
g <- g + theme_classic(base_size = 20)
g <- g + theme(plot.title = element_blank(),
               strip.background = element_blank(),
               strip.text = element_blank(),
               panel.grid.minor.x = element_blank())
g <- g + labs(x = "Strahler stream order",
              y = "Number of sites",
              color = "Dataset")
g <- g + scale_x_continuous(limits=c(1,9), breaks=c(1:9))
g <- g + scale_color_manual(values=c("SGf/SGs"="#377eb8", "SCf"="#4daf4a", "PCf"="#984ea3"))

# g <- g + lims(y=c(0,500))

pdf('outputs/paper 2 results/P2 stream order by dataset.pdf', height=4, width=8)
print(g)
dev.off()

# Figure 3 ####
# network representation of the hSDM is constructed using Inkscape

# Table 2 ####
# Potential explanatory variables outlined in MS Word manuscript

# Table 3 ####
# reproduced from model_dev\outputs\paper 2 results\variable selection\compromize_model.R script

# Figure 4 ####
# Plot selected explanatory variables by dataset
plot.data <- tibble()
datasets <- c("sample.bdmf", "sample.invf", "sample.invfp")
for (d in 1:length(datasets)){
  dataset <- datasets[d]
  K <- c("A10m", "IAR", "LUD", "Urban", "bFRI", "FRI", "FV", "WV", "Temp")
  dt <- prepare.inputs(K, get(dataset), center = F)
  dt <- gather(dt, Variable, Value, -SiteId, -SampId)
  dt$Trial <- dataset
  plot.data <- bind_rows(plot.data, dt)
}


plot.data$Trial <- ifelse(plot.data$Trial=="sample.bdmf", "SGf/SGs", plot.data$Trial)
plot.data$Trial <- ifelse(plot.data$Trial=="sample.invf", "SCf", plot.data$Trial)
plot.data$Trial <- ifelse(plot.data$Trial=="sample.invfp", "PCf", plot.data$Trial)

plot.data$Trial <- factor(plot.data$Trial, levels = c("SGf/SGs", "SCf", "PCf"))

plot.data <- filter(plot.data, Variable != "Temp2")
plot.data$Label <- factor(plot.data$Variable, levels = K[K !="Temp2"])
levels(plot.data$Label) <- labeller(levels(plot.data$Label))

# Boxplots
g <- ggplot(plot.data)
g <- g + geom_boxplot(aes(x=Trial, y=Value, fill=Trial), position = position_dodge2(preserve = "total"))
g <- g + stat_summary(aes(x=Trial, y=Value), fun.y=median, colour="black", fill="black", geom="point", 
                      shape=21, size=3, show.legend = FALSE)
g <- g + facet_wrap(~Label, scales="free", labeller=label_parsed, strip.position="bottom")

g <- g + theme_classic(base_size=26)
g <- g + labs(fill = "Dataset")
g <- g + theme(strip.background = element_blank(), strip.placement = "outside",
               plot.title = element_blank(),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank())
g <- g + scale_x_discrete(limits=c("PCf", "SCf", "SGf/SGs"))
g <- g + scale_fill_manual(values=c("SGf/SGs"="#377eb8", "SCf"="#4daf4a", "PCf"="#984ea3"))
g <- g + coord_flip()
g

pdf('outputs/paper 2 results/P2 explanatory variables.pdf', width=13, height=7)
print(g)
dev.off()

# Figure 5 ####
# Plot the quality of fit (deviance) and explanatory power (D2) based on the dataset
t <- bind_rows(taxonomy.bdms, taxonomy.bdmf, taxonomy.invf, taxonomy.invfp)
t <- unique(t)

plot.data <- bind_rows(fit.bdmf$deviance, fit.bdms$deviance, fit.invf$deviance, fit.invfp$deviance)
plot.data$Model <- ifelse(plot.data$Model=="FF0_bdmf", "SGf", plot.data$Model)
plot.data$Model <- ifelse(plot.data$Model=="FF0_bdms", "SGs", plot.data$Model)
plot.data$Model <- ifelse(plot.data$Model=="FF0_invf", "SCf", plot.data$Model)
plot.data$Model <- ifelse(plot.data$Model=="FF0_invfp", "PCf", plot.data$Model)
plot.data$Model <- factor(plot.data$Model, levels=c("SGs", "SGf", "SCf", "PCf"))

g1 <- ggplot(data = plot.data)
g1 <- g1 + geom_violin(aes(x = Model, y = D2, fill = Model))
g1 <- g1 + geom_boxplot(aes(x = Model, y = D2), width=0.20)
# expression(paste("D"^2))
# expression(paste("Marginal posterior ",beta["kj"]^"taxa"))
g1 <- g1 + labs(y = expression(paste("D"^2)),
                fill = "Dataset")
g1 <- g1 + geom_hline(yintercept = 0)
g1 <- g1 + scale_fill_brewer(palette = "Set1")
g1 <- g1 + theme_classic(base_size = 25)
g1 <- g1 + theme(plot.title = element_blank(),
                 legend.position = "none")
g1 <- g1 + labs(x="Dataset")
# print(g1)

g2 <- ggplot(data = plot.data)
g2 <- g2 + geom_violin(aes(x = Model, y = std.deviance, fill = Model), show.legend = FALSE)
g2 <- g2 + geom_boxplot(aes(x = Model, y = std.deviance), width=0.20, show.legend = FALSE)
g2 <- g2 + labs(y = "Standardized deviance")
g2 <- g2 + scale_fill_brewer(palette = "Set1")
g2 <- g2 + theme_classic(base_size = 25)
g2 <- g2 + theme(plot.title = element_blank(),
                 legend.position = "none")
g2 <- g2 + labs(x="Dataset")
# print(g2)

# pdf('outputs/paper 2 results/P2 quality of fit.pdf', width=16, height=7)
# plot_grid(g2, g1, align="v", labels = c("A", "B"), label_size = 18)
# dev.off()


# x.grob <- text_grob("Dataset", face="bold", color="black", size=18)
# x.grob <- textGrob("Dataset", gp=gpar(fontface="bold", col="black", fontsize=18))

pdf('outputs/paper 2 results/P2 quality of fit.pdf', width=16, height=7)
plot_grid(g2, g1, align="v", labels = c("A", "B"), label_size = 18)
# grid.arrange(arrangeGrob(plot, bottom = x.grob))
dev.off()


# # # P2 D2 vs dev by trial ###
# dev.family <- bind_rows(jsdm.bdmf$deviance, jsdm.invf$deviance, jsdm.invfp$deviance)
# dev.family$rel.freq[dev.family$Trial=='bdmf'] <- dev.family$n[dev.family$Trial=='bdmf']/nrow(sample.bdmf)
# dev.family$rel.freq[dev.family$Trial=='invf'] <- dev.family$n[dev.family$Trial=='invf']/nrow(sample.invf)
# dev.family$rel.freq[dev.family$Trial=='invf_plat'] <- dev.family$n[dev.family$Trial=='invf_plat']/nrow(sample.invfp)
# dev.family <- dev.family[Taxon %in% names(n.invf),]
# 
# g <- ggplot(dev.family)
# g <- g + geom_path(lineend="round", linejoin="round", aes(x = std.res.dev, y = D2, group = Taxon, size=rel.freq), alpha=0.3, show.legend=FALSE)
# g <- g + geom_point(aes(x = std.res.dev, y = D2, size = rel.freq, color=Model), alpha=0.5)
# g <- g + scale_radius(range = c(2,7), breaks=seq(0,1,0.2), limits=c(0,1))
# g <- g + theme_bw(base_size = 15)
# g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
# g <- g + labs(title="Explanatory power vs quality of fit",
#               y=expression(paste("D"^2)),
#               # y="D2",
#               x="Standardized deviance",
#               size="Relative occurrence\nfrequency")
# print(g)

# Figure 6 #### 
# plot the relative importance of the explanatory variables by 
# with the range of the linear predictor for each taxon, dataset, variable
# linear.predictor(jsdm)
slopes.bdmf <- linear.predictor(fit.bdmf)
slopes.bdms <- linear.predictor(fit.bdms)
slopes.invf <- linear.predictor(fit.invf)
slopes.invfp <- linear.predictor(fit.invfp)

slopes.bdmf$Trial <- "SGf"
slopes.bdms$Trial <- "SGs"
slopes.invf$Trial <- "SCf"
slopes.invfp$Trial <- "PCf"

slopes.all <- bind_rows(slopes.bdmf, slopes.bdms, slopes.invf, slopes.invfp)

# Drop taxonomy.ept taxa?
# dt <- dt[Taxon %in% names(n.bdms.taxonomy.ept),]
# dt <- dt[Taxon %in% names(n.bdmf.taxonomy.ept),]

# Get the max and min z-values by variable and by trial
plot.data <- slopes.all %>%
  group_by(Taxon, Trial, Variable) %>%
  summarise(z.min = min(z, na.rm=T), z.max = max(z, na.rm=T)) %>%
  mutate(z.range = z.max-z.min) %>%
  ungroup()

plot.data$Variable <- factor(plot.data$Variable, levels=c("A10m", "IAR", "LUD", "Urban", "bFRI", "FRI", "FV", "WV", "Temp"))
plot.data$Trial <- factor(plot.data$Trial, levels=c("SGs", "SGf", "SCf", "PCf"))
plot.data$Group <- ifelse(plot.data$Trial=="BDMf" | plot.data$Trial=="BDMs", "BDM", "CF")

g <- ggplot(data = plot.data) 
g <- g + geom_boxplot(aes(x=Group, y=z.range, fill=Trial)) # position = position_dodge2(preserve = "total")
g <- g + facet_grid(. ~ Variable)
g <- g + coord_cartesian(ylim=c(0,16)) #

g <- g + theme_bw(base_size = 24)
g <- g + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid.minor = element_blank()
)
g <- g + scale_fill_brewer(palette = "Set1")
g <- g + labs(y = expression(paste("z"["kj"]^"range")),
              fill = "Dataset")
print(g)


g <- ggplot(data = plot.data) 
g <- g + geom_boxplot(aes(x=Variable, y=z.range, fill=Trial)) # position = position_dodge2(preserve = "total")
g <- g + coord_cartesian(ylim=c(0,16)) #
g <- g + theme_classic(base_size = 24)
g <- g + scale_fill_brewer(palette = "Set1")
g <- g + labs(y = expression(paste("z"["kj"]^"range")),
              x = "Variable",
              fill = "Dataset")
print(g)

pdf('outputs/paper 2 results/P2 z-range by dataset.pdf', width = 14)
print(g)
dev.off()



# P2 pred by dataset
dev.cv <- bind_rows(cv.bdm$deviance, cv.inv$deviance)
plot.data <- dev.cv %>%
  group_by(Taxon, Type, Model) %>%
  summarise(mean.std.dev = mean(std.deviance)) %>%
  # spread(Type, mean.std.dev) %>%
  ungroup()

g <- ggplot(data = plot.data)
g <- g + geom_boxplot(aes(x = Model, y = mean.std.dev, fill = Model))
g <- g + facet_grid(. ~ Type)
g <- g + labs(title="Cross-validation performance by trial",
              subtitle="3-fold cross-validation",
              y = "Mean standardized deviance",
              x = "Trial")
g <- g + theme_bw(base_size = 15)
g <- g + scale_fill_brewer(palette = "Set1")
print(g)

# Pred by dataset ####
plot.data <- bind_rows(cv.bdmf$deviance, cv.bdms$deviance, cv.cfch$deviance, cv.cfp$deviance)
plot.data <- plot.data %>%
  group_by(Taxon, Type, Model) %>%
  summarise(mean.std.deviance = mean(std.deviance)) %>%
  ungroup() %>%
  mutate(Model = ifelse(Model == "FF0_bdmf", "SGf", Model),
         Model = ifelse(Model == "FF0_bdms", "SGs", Model),
         Model = ifelse(Model == "FF0_invf", "SCf", Model),
         Model = ifelse(Model == "FF0_invfp", "PCf", Model))

plot.data$Model <- factor(plot.data$Model, levels = c("SGs", "SGf", "SCf", "PCf"))
plot.data$Type <- factor(plot.data$Type, levels = c("Training", "Testing"))

g <- ggplot(data = plot.data)
g <- g + geom_boxplot(aes(x = Type, y = mean.std.deviance, fill = Model))
# g <- g + facet_grid(. ~ Type)
g <- g + theme_bw(base_size = 18)
g <- g + scale_fill_brewer(palette = "Set1")
g <- g + labs(y = "Mean standardized deviance\nduring 3-fold cross-validation",
              fill = "Dataset")
g <- g + theme(axis.title.x = element_blank())
print(g)

pdf('outputs/paper 2 results/P2 pred by dataset.pdf', width = 8, height = 7)
print(g)
dev.off()

# parameter distributions
jsdm.beta <- bind_rows(jsdm.bdmf$parameters, jsdm.invf$parameters, jsdm.invfp$parameters)

# do the same taxa show different responses in different spatial distributions?
plot.data <- jsdm.beta[Taxon %in% Reduce(intersect, list(names(n.bdmf),names(n.invf),names(n.invfp))),]
# slopes.plot$Label <- factor(slopes.plot$Variable, levels = v)
# levels(slopes.plot$Label) <- labeller(levels(slopes.plot$Label))

plot.data$Label <- factor(plot.data$Variable, levels = K)
levels(plot.data$Label) <- labeller(levels(plot.data$Label))
plot.data$Trial[plot.data$Trial=="bdmf"] <- "BDM families"
plot.data$Trial[plot.data$Trial=="invf"] <- "Combined families CH"
plot.data$Trial[plot.data$Trial=="invf_plat"] <- "Combined families plateau"

g <- ggplot(data=plot.data)
g <- g + geom_density(aes(x = Parameter, fill = Trial), alpha = 0.4)
g <- g + facet_wrap(~ Label, scales='free', labeller=label_parsed)
g <- g + scale_fill_brewer(palette = "Set1")
g <- g + theme_bw(base_size=16)
g <- g + labs(title="Max. posterior taxon-specific distributions of 92 common families",
              x="Influence Factor", y="Density")
print(g)

# Figure 7 ####
# Plot the explanatory power (during calibration; see ggplot g1) and predictive performance (during testing under cross-validation; see g2) based on taxonomic resolution in BDMf and BDMs
# Plots are arranged using ggarrange()

# Filter BDMs by taxonomy.ept orders
taxonomy.ept <- taxonomy.bdms[Order %in% c("Ephemeroptera", "Plecoptera", "Trichoptera"),]

# Select taxa in BDMf and BDMs that belong to taxonomy.ept orders
n.bdmf.ept <- n.bdmf[names(n.bdmf) %in% taxonomy.ept$Family]
n.bdms.ept <- n.bdms[names(n.bdms) %in% taxonomy.ept$Taxon]

# Get BDMf taxonomy.ept deviance
dev.bdmf <- filter(fit.bdmf$deviance, Taxon %in% names(n.bdmf.ept))
dev.bdmf$Family <- dev.bdmf$Taxon
dev.bdmf$Rank <- "Family"

# Obtain data for BDMs
dev.bdms <- taxonomy.ept %>%
  left_join(fit.bdms$deviance, by = "Taxon") %>%
  filter(Rank != "Family") %>%
  select_(.dots = c(colnames(dev.bdmf), "Family", "Rank"))

dev.taxonomy.bdm <- bind_rows(dev.bdmf, dev.bdms)
dev.taxonomy.bdm <- filter(dev.taxonomy.bdm, !(Taxon %in% c("Goeridae", "Hydroptilidae", "Lepidostomatidae")))
plot.data <- dev.taxonomy.bdm
rm(dev.bdmf, dev.bdms)

# Assign occurrence frequency based on whether taxon is in BDMf or not
plot.data$n.present <- ifelse(plot.data$Model == 'FF0_bdmf', n.bdmf.ept[plot.data$Taxon], n.bdms.ept[plot.data$Taxon])
# Label and order based on family occurrence frequency in BDM families
plot.data$Label <- factor(paste(plot.data$Family, ' - ', n.bdmf.ept[plot.data$Family]), levels = paste(names(sort(n.bdmf.ept, decreasing = TRUE)), ' - ', sort(n.bdmf.ept, decreasing = TRUE)))

# Figure S1.9 ####
# P2 Deviance per family and by rank
g <- ggplot(plot.data, aes(x = Label, y = std.deviance, color = Rank, size = n.present, label = Taxon))
g <- g + geom_point(alpha=0.6)
# g <- g + scale_size_continuous(range = c(2,7))
g <- g + scale_radius(range = c(2,8))
g <- g + theme_classic(base_size = 18)
g <- g + theme(axis.text.x = element_text(angle=45,hjust=1),
               plot.title = element_blank())
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
g <- g + labs(title="Quality of fit for EPT taxa in BDM",
              y="Standardized deviance",
              x="Family (labelled and ordered by number of occurrences)",
              size="Number of\noccurrences",
              color="Rank")
# g <- ggplotly(g)
pdf('outputs/paper 2 results/P2 fit by resolution.pdf', height=8.5, width=15)
print(g)
dev.off()

# Figure 7A ####
# P2 D2 per family and by rank
# Two theme layers control whether the x-axis titles and text appear
g1 <- ggplot(plot.data, aes(x = Label, y = D2, color = Rank, size = n.present, label = Taxon))
g1 <- g1 + geom_point(alpha=0.6)
g1 <- g1 + scale_radius(range = c(2,8))
g1 <- g1 + theme_classic(base_size = 18)
# g1 <- g1 + theme(plot.title = element_blank(),
#                  axis.text.x = element_text(angle=45,hjust=1))
g1 <- g1 + theme(plot.title = element_blank(),
               axis.text.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.title.x = element_blank())
g1 <- g1 + guides(colour = guide_legend(override.aes = list(size=6)))
g1 <- g1 + labs(y=expression(paste("Explanatory power (D"^2,")")),
              size="Number of\noccurrences",
              color="Taxonomic\nresolution",
              x = "Family (labelled and ordered by number of occurrences)")

pdf('outputs/paper 2 results/P2 D2 by resolution.pdf', height=8.5, width=15)
print(g1)
dev.off()

# PP by resolution
dev.bdmf <- cv.bdmf$deviance %>%
  group_by(Type, Trial, Taxon) %>%
  summarise(mean.std.deviance = mean(std.deviance)) %>%
  ungroup() %>%
  filter(Trial == 'FF0_bdmf', Taxon %in% names(n.bdmf.ept)) %>%
  mutate(Family = Taxon, Rank = "Family")

dev.bdms <- cv.bdms$deviance %>%
  group_by(Type, Trial, Taxon) %>%
  summarise(mean.std.deviance = mean(std.deviance)) %>%
  ungroup() %>%
  filter(Trial == 'FF0_bdms', Taxon %in% names(n.bdms.ept)) %>%
  left_join(taxonomy.ept, by = "Taxon")

plot.data <- dev.bdms[, colnames(dev.bdmf)] %>%
  bind_rows(dev.bdmf) %>%
  filter(!(Taxon %in% c("Goeridae", "Hydroptilidae", "Lepidostomatidae")))
# rm(dev.bdmf, dev.bdms)

# Assign occurrence frequency based on whether taxon is in BDMf or not
plot.data$n.present <- ifelse(plot.data$Trial == 'FF0_bdmf', n.bdmf.ept[plot.data$Taxon], n.bdms.ept[plot.data$Taxon])
# Label and order based on family occurrence frequency in BDM families
plot.data$TaxonLabel <- factor(paste(plot.data$Family, ' - ', n.bdmf.ept[plot.data$Family]), levels = paste(names(sort(n.bdmf.ept, decreasing = TRUE)), ' - ', sort(n.bdmf.ept, decreasing = TRUE)))
plot.data$Type <- factor(plot.data$Type, levels=c("Training", "Testing"))

# Figure S1.11 ####
# Facet by Training and Testing
g <- ggplot(data = plot.data)
g <- g + geom_point(aes(x = TaxonLabel, y = mean.std.deviance, color = Rank, size = n.present), alpha = 0.6)
g <- g + facet_grid(Type ~ .)
g <- g + scale_radius(range = c(2,8))
g <- g + theme_bw(base_size = 18)
g <- g + theme(plot.title = element_blank(),
               axis.text.x = element_text(angle=45,hjust=1))
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
g <- g + labs(y="Mean standardized deviance",
              x="Family (labelled and ordered by number of occurrences)",
              size="Number of\noccurrences",
              color="Rank")

pdf('outputs/paper 2 results/P2 pred by resolution [facet].pdf', height=8.5, width=15)
print(g)
dev.off()

# Figure 7B ####
# Subset the cross-validation results to performance during testing only
plot.data <- filter(plot.data, Type=="Testing")

g2 <- ggplot(data = plot.data)
g2 <- g2 + geom_point(aes(x = TaxonLabel, y = mean.std.deviance, color = Rank, size = n.present), alpha = 0.6)
g2 <- g2 + scale_radius(range = c(2,8))
g2 <- g2 + theme_bw(base_size = 18)
g2 <- g2 + theme(plot.title = element_blank(),
               axis.text.x = element_text(angle=45,hjust=1))
g2 <- g2 + guides(colour = guide_legend(override.aes = list(size=6)))
g2 <- g2 + labs(y="Mean standardized deviance",
              x="Family (labelled and ordered by number of occurrences)",
              size="Number of\noccurrences",
              color="Taxonomic\nresolution")

# svg('outputs/paper 2 results/P2 performance by resolution.svg', width=14, height=12)
pdf('outputs/paper 2 results/P2 performance by resolution.pdf', width=14, height=12)
ggarrange(plotlist=list(g1,g2), ncol=1, nrow=2, common.legend = TRUE, legend="right", labels=c("A", "B"), font.label=list(size=18))
dev.off()

# ggarrange(plotlist=list(g1,g2,g3,g4), ncol=2, nrow=2, common.legend = TRUE, legend="right",
#           labels=c("(a)", "(b)", "(c)", "(d)"))
# pdf('outputs/paper 2 results/P2 pred by resolution [testing only].pdf', height=8.5, width=15)
# print(g)
# dev.off()





# Figure 8 ####
# Baetidae grid-arranged maps; by default D-squared label and legend are displayed
g1 <- map.jsdm.pred.taxon(fit.bdmf, "Baetidae", italic.title = FALSE)
g2 <- map.jsdm.pred.taxon(fit.bdms, "Baetis_alpinus", italic.title = TRUE)
g3 <- map.jsdm.pred.taxon(fit.bdms, "Baetis_rhodani", italic.title = TRUE)
g4 <- map.jsdm.pred.taxon(fit.bdms, "Baetis_muticus", italic.title = TRUE)

# # plot_grid() SVG: doesn't produce a common legend
# svg('outputs/paper 2 results/P2 maps example taxa.svg', width=13, height=9.5)
# plot_grid(g1,g2,g3,g4, labels=c("(a)", "(b)", "(c)", "(d)"), align="hv")
# dev.off()

# ggarrange() gives grid lines whereas plot_grid() does not
# See: https://github.com/tidyverse/ggplot2/issues/2071
ggarrange(g1,g2,g3,g4, ncol=2, nrow=2, common.legend = TRUE, legend="right")

svg('outputs/paper 2 results/P2 maps example taxa.svg', width=13, height=9.5)
ggarrange(plotlist=list(g1,g2,g3,g4), ncol=2, nrow=2, common.legend = TRUE, legend="right",
          labels=c("A", "B", "C", "D"))
dev.off()

pdf('outputs/paper 2 results/P2 maps example taxa.pdf', width=13, height=9.5)
ggarrange(plotlist=list(g1,g2,g3,g4), ncol=2, nrow=2, common.legend = TRUE, legend="right",
          labels=c("A", "B", "C", "D"))
dev.off()

# Figure 8 ####
# Produce detailed ridge plots of marginal posterior taxon-specific parameters based on taxonomic resolution

# Given a vector of species in BDM, do a fancy automated plot:
# - get the family and species parameters, responses, rank, and occurrence frequency
# - get the full sample of the marginal posterior taxon-specific parameters
# - do plot parameters with specific order (family first, then species by frequency), formatted labels, and filled significance
# e.g., taxa <- c("Baetis_alpinus", "Baetis_rhodani", "Baetis_muticus", "Baetis_lutheri")
# - ASSUMES fitted model for BDMf and BDMs in environment for extract.beta(), extract.resp()
# scale argument controls degree of vertical overlap of density distributions
plot.beta.taxa <- function(taxa, columns, scale, size, legend.position){
  n <- c(n.bdmf, n.bdms)
  # reorder the taxa and ID the family
  j <- names(sort(n[taxa], decreasing=TRUE))
  taxonomy <- taxonomy.ept[Taxon %in% j, ]
  family <- unique(taxonomy$Family)
  
  # Extract parameters
  beta.samples.bdmf <- extract.beta(fit.bdmf)
  beta.samples.bdms <- extract.beta(fit.bdms)
  
  beta.samples.bdmf <- beta.samples.bdmf %>%
    filter(Taxon == family) %>%
    mutate(Rank = "Family", Family = Taxon)
  
  # BDMs: get Rank, Family  for each taxon, limit to EPT taxa
  beta.samples.bdms <- beta.samples.bdms %>%
    filter(Taxon %in% j) %>%
    left_join(select(taxonomy.ept, Taxon, Rank, Family), by="Taxon")
  
  beta.samples <- bind_rows(beta.samples.bdmf, beta.samples.bdms)
  
  # Extract and join significant responses
  response.bdmf <- extract.resp(fit.bdmf)
  response.bdmf <- filter(response.bdmf, Taxon != "Goeridae")
  response.bdms <- extract.resp(fit.bdms)
  
  responses <- bind_rows(response.bdmf, response.bdms)
  responses <- unique(responses) # eliminate duplicate responses in BDMf and BDMs
  
  responses.tidy <- gather(responses, Variable, Response, -Taxon)
  responses.tidy$Response[responses.tidy$Response==0] <- "Not significant"
  responses.tidy$Response[responses.tidy$Response==1] <- "Significant positive"
  responses.tidy$Response[responses.tidy$Response==-1] <- "Significant negative"
  
  plot.data <- beta.samples
  plot.data <- left_join(plot.data, responses.tidy, by=c("Variable", "Taxon"))
  setDT(plot.data)
  
  # Clean environment (for tidy troubleshooting only)
  rm(beta.samples.bdmf, beta.samples.bdms, beta.samples)
  rm(response.bdmf, response.bdms, responses)
  
  # Labels for taxon, variable, response
  taxonomy$n <- n[taxonomy$Taxon]
  taxonomy <- arrange(taxonomy, n)
  
  taxonomy.taxa <- c(taxonomy$Rank, "Family")
  names(taxonomy.taxa) <- c(taxonomy$Taxon, family)
  taxonomy.taxa <- substring(taxonomy.taxa, 1, 1)
  
  taxon.labels <- paste(names(taxonomy.taxa), " (",taxonomy.taxa," ", n[names(taxonomy.taxa)],")", sep="")
  taxon.labels <- unique(taxon.labels)
  names(taxon.labels) <- names(taxonomy.taxa)
  
  plot.data$TaxonLabel <- taxon.labels[plot.data$Taxon]
  plot.data$TaxonLabel <- factor(plot.data$TaxonLabel, levels=taxon.labels)
  levels(plot.data$TaxonLabel) <- sub("_", " ", levels(plot.data$TaxonLabel))
  
  plot.data$VariableLabel <- factor(plot.data$Variable, levels = unique(plot.data$Variable))
  levels(plot.data$VariableLabel) <- labeller.beta(levels(plot.data$VariableLabel))
  
  # Plot the data
  g <- ggplot(plot.data)
  # g <- g + stat_density_ridges(aes(x = Value, y = TaxonLabel, fill = Response, group=Taxon), scale = scale, alpha = 0.5)
  g <- g + geom_density_ridges(aes(x = Value, y = TaxonLabel, fill = Response, group=Taxon), rel_min_height=0.01, scale = scale, alpha = 0.5)
  
  # quantile_lines = TRUE, quantiles = c(0.05, 0.95)
  g <- g + geom_vline(xintercept = 0, alpha=0.7)
  
  # Parse facet labels as expressions
  # Facetting for all taxa in family
  # g <- g + facet_grid(. ~ VariableLabel, scales="free", shrink=TRUE, labeller=label_parsed, switch="x")
  # Facetting for only specific taxa in family
  g <- g + facet_wrap(~ VariableLabel, scales="free_x", ncol=columns, labeller = label_parsed, strip.position = "bottom")
  g <- g + theme_classic(base_size = size)
  g <- g + theme(strip.background = element_blank(), 
                 strip.placement = "outside",
                 strip.text = element_text(size = 16, vjust=1.5), # facet strip text placement
                 
                 plot.title=element_blank(),
                 axis.text.x = element_text(size=16, angle=35, hjust = 1), # placement of the x-axis text (not the label)
                 axis.text.y = element_text(size=18, vjust=-0.4), # placement of the y-axis text (not the label)
                 axis.title.y = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position=legend.position)
  g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
  g <- g + labs(fill="") # expression(paste("Marginal posterior ",beta["kj"]^"taxa"))
  g <- g + scale_fill_manual(values=c("Significant positive"="blue", "Not significant"="grey", "Significant negative"="red"))
  g <- g + scale_y_discrete(expand = c(0.01, 0))
  g
}

pdf('outputs/paper 2 results/P2 parameters by resolution Baetidae.pdf', height=8, width=14)
g <- plot.beta.taxa(c("Baetis_alpinus", "Baetis_muticus", "Baetis_rhodani"), columns=5, scale=2, size=20, legend.position = "top")
print(g)
dev.off()

# plot.beta.taxon(c("Habroleptoides_confusa", "Habrophlebia_lauta"))
plot.beta.taxon(c("Protonemura_lateralis", "Nemoura_mortoni", "Amphinemura", "Nemoura_minima", "Protonemura_brevistyla", "Protonemura_intricata"))

# Calculate how often taxon responses are significant per variable
# and now for my most shameful hack...
dt <- tibble()
setDT(plot.data)
for (k in 1:length(unique(plot.data$Variable))){
  variable <- unique(plot.data$Variable)[k]
  for (j in 1:length(unique(plot.data$Taxon))){
    taxon <- unique(plot.data$Taxon)[j]
    test <- plot.data[Taxon==taxon & Variable==variable, ]
    
    test <- t(as.matrix(test[, c("beta.mean", "quantile5", "quantile95")]))
    # lazy subsetting like an anti-christ...
    test <- ifelse(test["beta.mean",][1] < test[c("quantile95"),][2] & test["beta.mean",][1] > test[c("quantile5"),][2], "similar", "different")
    d <- tibble(Taxon=taxon, Variable=variable, Similarity=test)
    dt <- bind_rows(dt, d)
    rm(taxon, test, d)
  }
  rm(variable)
}

test <- dt %>%
  group_by(Variable, Similarity) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

test$Variable <- factor(test$Variable, levels=c(unique(test$Variable)))
g <- ggplot(test)
g <- g + geom_bar(aes(x=Variable, y=count, fill=Similarity), stat="identity")
print(g)

dt.heatmap <- dt
dt.heatmap$n <- n.invf[dt.heatmap$Taxon]
dt.heatmap$Labels <- factor(paste(dt.heatmap$Taxon, ' - ', n.invf[dt.heatmap$Taxon]), levels = paste(names(sort(n.invf)), ' - ', sort(n.invf)))

g <- ggplot(dt.heatmap)
g <- g + geom_tile(aes(x=Variable, y=Labels, fill=Similarity), alpha=0.4)
g <- g + scale_fill_manual(values=c("red", "blue"))
g <- g + scale_x_discrete(position = "top") 
g <- g + labs(title="Significant differences in response between combined families CH vs plateau",
              subtitle="A parameter is statistically similar if the posterior mean is within the 95% CI of its counterpart")

# Figure 10 ####
# Grid-arranged plots of Epeorus species
map.alpicola <- map.jsdm.pred.taxon(jsdm = fit.bdms, taxon = "Epeorus_alpicola", legend = TRUE)
map.assimilis <- map.jsdm.pred.taxon(jsdm = fit.bdms, taxon = "Epeorus_assimilis", legend = TRUE)

# E. alpicola has significant responses to Urban, bFRI, FV, and Temp
p.alpicola <- list()
p.alpicola$urban <- plot.prob.taxon(fit.bdms, "Epeorus_alpicola", variables = "Urban", marginal.density = TRUE, legend = FALSE)
p.alpicola$bfri <- plot.prob.taxon(fit.bdms, "Epeorus_alpicola", variables = "bFRI", marginal.density = TRUE, legend = FALSE)
p.alpicola$fv <- plot.prob.taxon(fit.bdms, "Epeorus_alpicola", variables = "FV", marginal.density = TRUE, legend = FALSE)
p.alpicola$temp <- plot.prob.taxon(fit.bdms, "Epeorus_alpicola", variables = "Temp", marginal.density = TRUE, legend = FALSE)

# E. assimilis has significant responses to A10m, IAR, LUD, Urban, and WV
p.assimilis <- list()
p.assimilis$a10m <- plot.prob.taxon(fit.bdms, "Epeorus_assimilis", variables = "A10m", marginal.density = TRUE, legend = FALSE)
p.assimilis$iar <- plot.prob.taxon(fit.bdms, "Epeorus_assimilis", variables = "IAR", marginal.density = TRUE, legend = FALSE)
p.assimilis$lud <- plot.prob.taxon(fit.bdms, "Epeorus_assimilis", variables = "LUD", marginal.density = TRUE, legend = FALSE)
p.assimilis$urban <- plot.prob.taxon(fit.bdms, "Epeorus_assimilis", variables = "Urban", marginal.density = TRUE, legend = FALSE)
p.assimilis$temp <- plot.prob.taxon(fit.bdms, "Epeorus_assimilis", variables = "Temp", marginal.density = TRUE, legend = FALSE)

plot_grid(plotlist = p.alpicola)

plot_grid(plotlist = p.assimilis)


# Interesting results for E. alpicola
# pdf('outputs/paper 2 results/P2 Epeorus comparison.pdf', height=9.5, width=14.5)
svg('outputs/paper 2 results/P2 Epeorus comparison.svg', height=9.5, width=14.5)
plot_grid(plotlist = list(p.alpicola$temp, p.alpicola$fv, p.alpicola$urban, map.alpicola,
                          p.assimilis$temp, p.assimilis$lud, p.assimilis$urban, map.assimilis
),labels=list("A","","","", "B","","",""), 
ncol=4, rel_widths = c(1,1,1,2))
dev.off()

svg('outputs/paper 2 results/P2 Epeorus comparison.svg', height=8.75, width=24)
g1 <- plot_grid(plotlist = list(p.alpicola$temp, p.alpicola$fv, p.alpicola$urban), ncol=3)
g2 <- plot_grid(plotlist = list(map.alpicola), ncol=1)
g3 <- plot_grid(plotlist = list(p.assimilis$temp, p.assimilis$lud, p.assimilis$urban, p.assimilis$iar), ncol=4)
g4 <- plot_grid(plotlist = list(map.assimilis), ncol=1)

plot_grid(g1,g2,g3,g4, labels=list("(a)", "", "(b)", ""))
dev.off()

# test 
g <- plot.prob.taxon(fit.bdms, "Epeorus_assimilis", variables = "IAR", marginal.density = TRUE, legend = FALSE)

# Occurrence frequency ####
t.bdms <- taxonomy.bdms
t.bdms$Trial <- "BDMs"
t.bdms$n <- n.bdms[t.bdms$Taxon]

t.bdmf <- taxonomy.bdmf
t.bdmf$Trial <- "BDMf"
t.bdmf$n <- n.bdmf[t.bdmf$Taxon]
t.bdm <- bind_rows(t.bdmf, t.bdms)
rm(t.bdmf, t.bdms)

t.bdm <- t.bdm[t.bdm$Rank %in% c("Family", "Genus", "Species"),]
t.bdm <- arrange(t.bdm, Family, desc(n))

t.bdm <- t.bdm %>%
  group_by(Family) %>%
  mutate(n.taxa = n()) %>%
  filter(n.taxa > 2) %>%
  select(-n.taxa) %>%
  mutate(ID = row_number()+1) %>%
  mutate(alpha = n.bdmf[Family]/581)


g <- ggplot(t.bdm, aes(x=ID, y=n))
g <- g + geom_line(aes(group=Family, alpha=alpha), color="grey", show.legend=FALSE)
# g <- g + geom_smooth(aes(group=Family, alpha=alpha), method="lm", formula=(y~sqrt(x)), se=FALSE, color="grey", show.legend=FALSE)
# g <- g + geom_smooth(aes(group=Family, alpha=alpha), method="loess", se=F, color="grey", size=1.25, show.legend=F)
g <- g + geom_point(aes(color=Rank))
g <- g + theme_bw(base_size = 20)
g <- g + theme(plot.title=element_blank())
g <- g + labs(x="Taxon (ordered by occurrence frequency)",
              y="Occurrence frequency")
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
print(g)

# plot.data$Label <- factor(paste(plot.data$Family, ' - ', plot.data$n.family), levels = paste(names(sort(n.bdmf.ept, decreasing = TRUE)), ' - ', sort(n.bdmf.ept, decreasing = TRUE)))

# APPENDIX ####
# Figure S1.1-S1.3 ####
library(corrplot)
# Plot the pairwise correlations between the *potential* explanatory variables
plot.data <- prepare.inputs(K = c("A10m", "A100m", "A1km", "F10m", "F100m", "F1km", "IAR", "LUD", "Urban", "FRI", "bFRI", "FV", "WV", "Temp", "BM", "Morph", "F.EDO", "A.EDO", "TU.Dm", "TU.Cr", "UI", "WW", "HP"), sample.invf, center=TRUE)

plot.data <- na.omit(plot.data)
plot.data <- as.matrix(select(plot.data, -SiteId, -SampId))
plot.data <- cor(plot.data)
corrplot(plot.data, method = "number", type="upper", tl.srt=45)
corrplot.mixed(plot.data, tl.srt=45, order="hclust", lower.col = "black", number.cex = .7)

pdf('P2 correlations CFCH.pdf', width=11, height=10)
par(oma = c(0,0,0,0)+0.5, mar=c(0,0,0,0)+0.1)
corrplot(plot.data, method = "number", type="upper", tl.srt=45)
dev.off()

plot.data <- prepare.inputs(K = c("A10m", "A100m", "A1km", "F10m", "F100m", "F1km", "IAR", "LUD", "Urban", "FRI", "bFRI", "FV", "WV", "Temp", "BM", "Morph", "F.EDO", "A.EDO", "TU.Dm", "TU.Cr", "UI", "WW", "HP"), sample.invfp, center=TRUE)

plot.data <- na.omit(plot.data)
plot.data <- as.matrix(select(plot.data, -SiteId, -SampId))
plot.data <- cor(plot.data)
corrplot(plot.data, method = "number", type="upper", tl.srt=45)
corrplot.mixed(plot.data, tl.srt=45, order="hclust", lower.col = "black", number.cex = .7)

pdf('P2 correlations CFp.pdf', width=11, height=10)
par(oma = c(0,0,0,0)+0.5, mar=c(0,0,0,0)+0.1)
corrplot(plot.data, method = "number", type="upper", tl.srt=45)
dev.off()

plot.data <- prepare.inputs(K = c("A10m", "A100m", "A1km", "F10m", "F100m", "F1km", "IAR", "LUD", "Urban", "FRI", "bFRI", "FV", "WV", "Temp", "BM", "Morph", "F.EDO", "A.EDO", "TU.Dm", "TU.Cr", "UI", "WW", "HP"), sample.bdms, center=TRUE)
plot.data <- na.omit(plot.data)
plot.data <- as.matrix(select(plot.data, -SiteId, -SampId))
plot.data <- cor(plot.data)
corrplot(plot.data, method = "number", type="upper", tl.srt=45)

pdf('P2 correlations BDM.pdf', width=11, height=10)
par(oma = c(0,0,0,0)+0.5, mar=c(0,0,0,0)+0.1)
corrplot(plot.data, method = "number", type="upper", tl.srt=45)
dev.off()

#  Figure S1.4-S1.6 ####
# Plot: pairwise scatterplots
pdf('outputs/paper 2 results/P2 scatterplot by trial.pdf', onefile = TRUE)
pairs.panels(select(prepare.inputs(c("A10m", "IAR", "LUD", "Urban", "bFRI", "FRI", "FV", "Temp", "Temp2"), sample.bdmf, center=TRUE), -SiteId, -SampId), density = TRUE, scale=FALSE, hist.col="grey", cex.cor=1.5, cex.labels=1.5, main = "SGf/SGs")
pairs.panels(select(prepare.inputs(c("A10m", "IAR", "LUD", "Urban", "bFRI", "FRI", "FV", "Temp", "Temp2"), sample.invf, center=TRUE), -SiteId, -SampId), density = TRUE, scale=FALSE, hist.col="grey", cex.cor=1.5, cex.labels=1.5, main = "SCf")
pairs.panels(select(prepare.inputs(c("A10m", "IAR", "LUD", "Urban", "bFRI", "FRI", "FV", "Temp", "Temp2"), sample.invfp, center=TRUE), -SiteId, -SampId), density = TRUE, scale=FALSE, hist.col="grey", cex.cor=1.5, cex.labels=1.5, main = "PCf")
dev.off()

# Figure S1.7 ####
# Plot the predictive performance of the potential models in BDMs
vs.bdms <- extract.vsr("variable_selection/paper 2 results/variable_selection_bdms", sample.bdms)

# Get mean relative deviance for each model over entire community (excluding rare taxa)
vs.bdms.mean <- vs.bdms %>%
  filter(!is.infinite(rdev.test) & n > 56) %>%
  group_by(Model) %>%
  summarise(mrd.train = mean(rdev.train, na.rm=TRUE), mrd.test = mean(rdev.test, na.rm=TRUE)) %>%
  arrange(mrd.test) %>%
  mutate(Parameters = vapply(strsplit(Model, " "), length, integer(1)) + 1)

# count the number of explanatory variables (as opposed to number of parameters)
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
g <- g + geom_point(alpha=0.35)
g <- g + geom_abline(intercept = 0, slope = 1, color="black", size=1.25)
g <- g + theme_bw(base_size = 18)
# g <- g + theme_gray(base_size = 17) # gray improves contrast
g <- g + labs(y = "Mean standardized deviance during prediction",
              x = "Mean standardized deviance during calibration",
              color = "Number of\nparameters")
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
g <- g + scale_colour_brewer(palette = "Set1")
print(g)

# NETWORK plots
# Experimental code for plotting networks using D3
# library(htmlwidgets)
# JS <- htmlwidgets::JS

# # Example data.tree
# acme <- Node$new("Acme Inc.")
# accounting <- acme$AddChild("Accounting")
# software <- accounting$AddChild("New Software")
# standards <- accounting$AddChild("New Accounting Standards")
# research <- acme$AddChild("Research")
# newProductLine <- research$AddChild("New Product Line")
# newLabs <- research$AddChild("New Labs")
# it <- acme$AddChild("IT")
# outsource <- it$AddChild("Outsource")
# agile <- it$AddChild("Go agile")
# goToR <- it$AddChild("Switch to R")
# 
# print(acme)

# Plot a radial network of significant responses to temperature based on taxonomy
test <- bind_rows(response.bdmf[, c("Taxon", "Temp")], response.bdms[, c("Taxon", "Temp")])
test <- unique(test)
testr <- test$Temp; names(testr) <- test$Taxon

test <- left_join(taxonomy.bdms, response.bdms[, c("Taxon", "Temp")], by="Taxon")
# test$response[test$Temp==1] <- "blue"
# test$response[test$Temp==0] <- "grey"
# test$response[test$Temp==-1] <- "red"
test$pathString <- paste5("Animalia", test$Phylum,test$Class,test$Order,test$Family, test$Genus, test$Taxon, sep = "/", na.rm = TRUE)
tree <- as.Node(test)
tree.df <- ToDataFrameNetwork(tree, "name")
test.response <- testr[tree.df$name]

test.response[test.response==1] <- "skyblue"
test.response[test.response==0] <- "grey"
test.response[test.response==-1] <- "red"
test.response[is.na(test.response)] <- "black"

rnw <- ToListExplicit(tree, unname = TRUE) # Convert to data.tree to list-of-list structure

# radialNetwork(rnw)


# Custom colors for the nodes
# https://stackoverflow.com/questions/42568997/r-networkd3-color-node-stroke-for-radialnetwork/42574609#42574609
jsarray <- paste0('["', paste(c("black", test.response), collapse = '", "'), '"]')
nodeStrokeJS <- JS(paste0('function(d, i) { return ', jsarray, '[i]; }'))

radialNetwork(rnw, 
              linkColour = "#ccc",
              nodeColour = nodeStrokeJS, # #fff
              nodeStroke = nodeStrokeJS,
              textColour = "black",
              opacity = 0.9,
              fontSize = 12)

# diagonalNetwork(rnw,
#                 linkColour = "#ccc",
#                 nodeColour = "#fff",
#                 nodeStroke = nodeStrokeJS,
#                 textColour = "black")

# radialNetwork(acmeNetwork)
# d3ForceNetwork(Links = MisLinks, Nodes = MisNodes,
#                Source = "source", Target = "target",
#                Value = "value", NodeID = "name",
#                Group = "group", width = 550, height = 400,
#                opacity = 0.9, zoom = TRUE)

# 
# Figure S1.8 ####
# Initial code creates a test plot showing response of taxa in BDM based on explanatory variable
# Later code colors nodes of a radial network plot of BDM taxonomy based on whether the taxon is in BDMf or BDMs
dt <- get.taxonomy(sample.bdms)
dt$Dataset[!(dt$Taxon %in% taxonomy.bdmf$Taxon)] <- "BDM Species"

dt$Dataset[dt$Taxon %in% taxonomy.bdmf$Taxon] <- "Both datasets"

test <- filter(taxonomy.bdmf, !(Taxon %in% dt$Taxon))
test$Dataset <- "BDM Family"
dt <- bind_rows(dt, test)
rm(test)

dt$Species[!is.na(dt$Species)] <- labeller.species(dt$Species[!is.na(dt$Species)])
dt$pathString <- paste5("Animalia", dt$Phylum,dt$Class,dt$Order,dt$Family,dt$Genus,dt$Species, sep = "/", na.rm = TRUE)

dt$Taxon[!is.na(dt$Species)] <- labeller.species(dt$Taxon[!is.na(dt$Species)])
taxa <- dt$Dataset; names(taxa) <- dt$Taxon
taxa[taxa=="BDM Species"] <- "blue"
taxa[taxa=="Both datasets"] <- "orange"
taxa[taxa=="BDM Family"] <- "red"

dt <- dt[!(Taxon %in% c("Silo", "Stactobia")),]
tree <- as.Node(dt)
tree.df <- ToDataFrameNetwork(tree, "name")

tree.df$color <- taxa[tree.df$name]
tree.df$color[is.na(tree.df$color)] <- "grey"

rnw <- ToListExplicit(tree, unname = TRUE)

jsarray <- paste0('["', paste(c("grey", tree.df$color), collapse = '", "'), '"]')
nodeStrokeJS <- JS(paste0('function(d, i) { return ', jsarray, '[i]; }'))

n <- radialNetwork(rnw, 
                   linkColour = "#ccc",
                   nodeColour = nodeStrokeJS, # "#fff"
                   nodeStroke = nodeStrokeJS,
                   textColour = "black",
                   opacity = 0.9,
                   fontSize = 10,
                   height = 1080,
                   width = 1080,
                   margin=0.1)

saveNetwork(n, file = "radialnw_BDM_fillednodes.html")

# Combine BDMf and BDMs taxonomy and responses to explanatory variable 'k'
plot.network <- function(k){
  # Combine taxonomy/responses of BDMf and BDMs datasets 
  taxonomy.combined <- bind_rows(taxonomy.bdmf[!(Taxon %in% taxonomy.bdms$Taxon),], taxonomy.bdms)
  response.combined <- bind_rows(response.bdmf[!(Taxon %in% taxonomy.bdms$Taxon),], response.bdms)
  dt <- left_join(taxonomy.combined, response.combined, by="Taxon")
  rm(taxonomy.combined, response.combined)
  
  # Label formatting for all species
  dt$Species[!is.na(dt$Species)] <- labeller.species(dt$Species[!is.na(dt$Species)])
  dt$Taxon[!is.na(dt$Species)] <- labeller.species(dt$Taxon[!is.na(dt$Species)])
  
  dt$pathString <- paste5("Animalia", dt$Phylum,dt$Class,dt$Order,dt$Family, dt$Genus, dt$Species, sep = "/", na.rm = TRUE)
  tree <- as.Node(dt, pathDelimiter="/")
  
  # Add responses to the network data frame
  tree.df <- ToDataFrameNetwork(tree, "name")
  
  response <- dt[,c("Taxon", k)]
  colnames(response) <- c("name", "response")
  tree.df <- left_join(tree.df, response, by="name")
  rm(response)
  
  tree.df$color[tree.df$response==1] <- "blue"
  tree.df$color[tree.df$response==0] <- "grey"
  tree.df$color[tree.df$response==-1] <- "red"
  tree.df$color[is.na(tree.df$response)] <- "black"
  
  # First element of vector colors the root node, which is not contained in data tree conversion to data frame
  jsarray <- paste0('["', paste(c("black", tree.df$color), collapse = '", "'), '"]')
  nodeStrokeJS <- JS(paste0('function(d, i) { return ', jsarray, '[i]; }'))
  
  rnw <- ToListExplicit(tree, unname = TRUE)
  n <- radialNetwork(rnw,
                     # width=900,
                     # height=900,
                     margin=c("top"=0.1, "right"=0.1, "bottom"=0.1, "left"=0.1),
                     linkColour = "#ccc",
                     nodeColour = "#fff",
                     nodeStroke = nodeStrokeJS,
                     textColour = "black",
                     opacity = 0.9,
                     fontSize = 10)
  
  saveNetwork(n, file = paste("radialnw_BDM_",k,".html", sep=''))
  cat("Plot:", k,"\n")
}

for (k in 1:length(K)){
  plot.network(K[k])
}


# Construct a network for BDM species
# Additional features to implement:
# - integrate BDM family responses from response.bdmf
# - size the points according to occurrence frequency (Nodesize	argument:
# character string specifying a column in the Nodes data frame to vary the node radius)
# - functional implementation
# - Shiny implementation with 
test <- left_join(taxonomy.bdms, response.bdms, by = "Taxon")
# test <- filter(test, Order == "Ephemeroptera")
test$pathString <- paste5("Animalia", test$Phylum,test$Class,test$Order,test$Family, test$Genus, test$Taxon, sep = "/", na.rm = TRUE)
tree <- as.Node(test)

# Convert the data.tree to a data.frame
tree.df <- ToDataFrameNetwork(tree, "name")
tree.df$source <- sapply(strsplit(tree.df$from, "/"), function(n){
  tail(n, n=1) # Get the last element from each vector in list
})

tree.df$target <- sapply(strsplit(tree.df$to, "/"), function(n){
  tail(n, n=1) # Get the last element from each vector in list
})


network <- data.table(source = tree.df$source, target = tree.df$target)

nodes <- data.table(name = unique(c(network$source, network$target)))
nodes$id <- 0:(nrow(nodes) - 1)

# create a data frame of the edges that uses id 0:9 instead of their names
edges <- network %>%
  left_join(nodes, by = c("source" = "name")) %>%
  select(-source) %>%
  rename(source = id) %>%
  left_join(nodes, by = c("target" = "name")) %>%
  select(-target) %>%
  rename(target = id)

# nodes$group <- 1
edges$width <- 1

# Color by response (0 = negative, 1 = not significant, 2 = positive, 3 = not modelled)
nodes <- nodes %>%
  left_join(select(response.bdms, Taxon, Temp), by = c("name" = "Taxon"))
nodes$group <- nodes$Temp; nodes$Temp <- NULL

nodes <- nodes %>%
  left_join(select(response.bdmf, Taxon, Temp), by = c("name" = "Taxon"))

nodes$group <- ifelse(is.na(nodes$Temp), nodes$group, nodes$Temp)
nodes$group <- nodes$group + 1
nodes$group[is.na(nodes$group)] <- 3 # replace these with BDM family responses

nodes$group[nodes$group==0] <- "significant negative"
nodes$group[nodes$group==1] <- "not significant"
nodes$group[nodes$group==2] <- "significant positive"
nodes$group[nodes$group==3] <- "taxonomy node"


ColourScale <- 'd3.scaleOrdinal()
.domain(["significant negative", "not significant", "significant positive", "taxonomy node"])
.range(["#D93434", "#B0B0B0", "#4545C7", "#A960D1"]);'

nodes$size <- (n.bdmf[nodes$name]/max(n.bdmf)*10)^2
nodes$size <- (n.bdms[nodes$name]/max(n.bdmf)*10)^2
nodes$size[is.na(nodes$size)] <- 1

forceNetwork(Links = edges, Nodes = nodes, 
             Source = "source",
             Target = "target",
             NodeID ="name",
             Group = "group",
             Value = "width",
             Nodesize = "size",
             opacity = 0.9,
             fontSize = 22,
             zoom = TRUE,
             legend = TRUE,
             charge = -60, # positive or negative node "attraction" to each other
             # linkDistance = networkD3::JS("function(d) { return 5*d.value; }"),
             colourScale = JS(ColourScale))

# Construct simple network
simpleNetwork(tree.df)


# # Construct a more complex network from BDM
# # links <- select(tree.df, from, to)
# 
# # links$source <- as.numeric(as.factor(links$from))
# # links$target <- as.numeric(as.factor(links$to))
# 
# # Convert from-to linkages from factor (altogether) to numeric
# taxa <- c(tree.df$from, tree.df$to)
# x <- as.numeric(as.factor(c(tree.df$from, tree.df$to)))-1
# links <- data.table(source = x[1:(length(x)/2)], target = x[((length(x)/2)+1):length(x)])
# links$source <- links$source - 1
# links$target <- links$target - 1
# 
# nodes <- data.table(name = unique(taxa), group = 1)
# # nodes <- data.frame(name = as.numeric(as.factor(test$Taxon)), group = 1)
# # nodes$name <- nodes$name-1
# 
# forceNetwork(Links=links,Nodes = nodes,
#              Source = 'source', Target = 'target', 
#              NodeID = 'name', Group = 'group', zoom = TRUE)

# # Advanced network example
# # Load data
# data(MisLinks)
# data(MisNodes)
# 
# # Plot
# forceNetwork(Links = MisLinks, Nodes = MisNodes,
#              Source = "source", Target = "target",
#              Value = "value", NodeID = "name",
#              Group = "group", opacity = 0.8)

# # Example from: https://stackoverflow.com/questions/45179424/r-networkd3-package-forcenetwork-coloring
# # Load package
# library(networkD3)
# library(dplyr) # to make the joins easier
# 
# # Create fake data
# source <- c("A", "A", "A", "A",
#             "B", "B", "C", "C", "D")
# target <- c("B", "C", "D", "J",
#             "E", "F", "G", "H", "I")
# networkData <- data.frame(source, target, stringsAsFactors = FALSE)
# 
# nodes <- data.frame(name = unique(c(source, target)), stringsAsFactors = FALSE)
# nodes$id <- 0:(nrow(nodes) - 1)
# 
# 
# # create a data frame of the edges that uses id 0:9 instead of their names
# edges <- networkData %>%
#   left_join(nodes, by = c("source" = "name")) %>%
#   select(-source) %>%
#   rename(source = id) %>%
#   left_join(nodes, by = c("target" = "name")) %>%
#   select(-target) %>%
#   rename(target = id)
# 
# edges$width <- 1
# 
# # make a grouping variable that will match to colours
# nodes$group <- ifelse(nodes$name %in% source, "lions", "tigers")
# 
# 
# ColourScale <- 'd3.scaleOrdinal()
# .domain(["lions", "tigers"])
# .range(["#FF6900", "#694489"]);'
# 
# forceNetwork(Links = edges, Nodes = nodes, 
#              Source = "source",
#              Target = "target",
#              NodeID ="name",
#              Group = "group",
#              Value = "width",
#              opacity = 0.9,
#              zoom = TRUE,
#              colourScale = JS(ColourScale))

# PP by dataset
plot.data <- bind_rows(cv.bdmf$deviance, cv.bdms$deviance, cv.cfch$deviance, cv.cfp$deviance)

plot.data <- plot.data %>%
  select(Taxon, Type, Fold, Trial, std.deviance) %>%
  group_by(Taxon, Type, Trial) %>%
  summarise(mean.std.deviance = mean(std.deviance)) %>%
  ungroup()

plot.data$Trial <- ifelse(plot.data$Trial=="FF0_bdmf", "BDMf", plot.data$Trial)
plot.data$Trial <- ifelse(plot.data$Trial=="FF0_bdms", "BDMs", plot.data$Trial)
plot.data$Trial <- ifelse(plot.data$Trial=="FF0_invf", "CFCH", plot.data$Trial)
plot.data$Trial <- ifelse(plot.data$Trial=="FF0_invfp", "CFp", plot.data$Trial)

plot.data$TrialLabel <- factor(plot.data$Trial, levels=c("BDMs", "BDMf", "CFCH", "CFp"), ordered = TRUE)
plot.data$TypeLabel <- factor(plot.data$Type, levels=c("Training", "Testing"), ordered = TRUE)

g <- ggplot(data=plot.data)
g <- g + geom_boxplot(aes(x=TypeLabel, y=mean.std.deviance, fill=TrialLabel)) # position = position_dodge2(preserve = "total")
g <- g + theme_bw(base_size=18)
g <- g + labs(fill = "Dataset", y = "Mean standardized deviance")
g <- g + theme(plot.title = element_blank(),
               axis.title.x = element_blank())
g <- g + scale_fill_brewer(palette="Set1")
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))







# Figure S1.12-S1.13 ####
# Plot means and 95% CI of the marginal posterior beta^taxa distributions
beta.bdmf <- extract.beta(fit.bdmf)
beta.bdmf <- left_join(beta.bdmf, taxonomy.bdmf, by="Taxon")
beta.invf <- extract.beta(fit.invf)
beta.invf <- left_join(beta.invf, taxonomy.invf, by="Taxon")

beta.bdmf <- beta.bdmf %>%
  group_by(Trial, Taxon, Variable) %>%
  summarise(beta.mean = mean(Value), quantile5 = quantile(Value, probs=0.05), quantile95 = quantile(Value, probs=0.95)) %>%
  ungroup() %>%
  left_join(taxonomy.bdmf, by="Taxon") %>%
  mutate(n = n.bdmf[Taxon]/nrow(sample.bdmf))

beta.invf <- beta.invf %>%
  group_by(Trial, Taxon, Variable) %>%
  summarise(beta.mean = mean(Value), quantile5 = quantile(Value, probs=0.05), quantile95 = quantile(Value, probs=0.95)) %>%
  ungroup() %>%
  left_join(taxonomy.invf, by="Taxon") %>%
  mutate(n = n.invf[Taxon]/nrow(sample.invf))

beta.taxa.families <- bind_rows(beta.bdmf, beta.invf)
rm(beta.bdmf, beta.invf)

plot.data <- filter(beta.taxa.families, Rank == 'Family', Taxon %in% intersect(names(n.bdmf), names(n.invf)))
plot.data$Labels <- factor(paste(plot.data$Taxon, ' - ', n.invf[plot.data$Taxon]), levels = paste(names(sort(n.invf)), ' - ', sort(n.invf)))
plot.data$Trial[plot.data$Trial == "FF0_bdmf"] <- "SGf"
plot.data$Trial[plot.data$Trial == "FF0_invf"] <- "SCf"
plot.data$VariableLabel <- labeller.beta(plot.data$Variable)

g <- ggplot(plot.data)
g <- g + geom_pointrange(aes(x=Labels, y=beta.mean, ymin=quantile5, ymax=quantile95, color=Trial))
g <- g + geom_hline(yintercept=0)
g <- g + facet_grid(. ~ Variable, scales="free")
g <- g + theme_bw(base_size=12)
g <- g + theme(axis.text.x = element_text(angle=45,hjust=1))
g <- g + coord_flip()
g <- g + guides(colour = guide_legend(override.aes = list(size=3))) # guides(colour=FALSE)
# RColorBrewer "Set1" palette:
# brewer.pal(n=3, "Set1")
g <- g + scale_color_manual(values = c("SGf" = "#377EB8", "SCf" = "#4DAF4A")) 
g <- g + labs(title = "Marginal posterior taxon-specific parameter distributions",
              subtitle = "Means (points) and 95% confidence intervals (lines)",
              x = "Taxon and number of occurrences (in SCf)",
              y = "",
              color = "Dataset")

pdf('outputs/paper 2 results/P2 response dotplot SCf_SGf.pdf', height=12, width=14)
print(g)
dev.off()

# Appendix S2 ####
# Loop and plot all families
# Filter EPT families by number of taxa
families <- taxonomy.ept %>% 
  group_by(Family) %>% 
  summarise(species=dplyr::n()) %>% 
  filter(Family %in% names(n.bdmf.ept)) %>%
  mutate(n.family = n.bdmf[Family]) %>%
  arrange(-n.family)


pdf("P2 parameters by family.pdf", height=9, width=18, onefile = TRUE)
for (j in 1:length(families$Family)){
  family <- families$Family[j]
  taxonomy <- taxonomy.ept[Family == family & Rank != "Family", ]
  
  if (!(family %in% c("Goeridae", "Hydroptilidae", "Lepidostomatidae"))){
    v <- taxonomy$Taxon
    # taxa <- taxa[taxa != "Family"]
    g <- plot.beta.taxa(v, scale = 3, size = 15, legend.position="bottom")
    cat("Plot: ", family,"\n")
    print(g)
  }
}
dev.off()

families <- "Baetidae"
pdf("P2 parameters Baetidae [all].pdf", height=10, width=22, onefile = TRUE)
for (j in 1:length(families)){
  family <- families[j]
  taxonomy <- taxonomy.ept[Family == family & Rank != "Family", ]
  
  if (!(family %in% c("Goeridae", "Hydroptilidae", "Lepidostomatidae"))){
    v <- taxonomy$Taxon
    # taxa <- taxa[taxa != "Family"]
    g <- plot.beta.taxa(v, scale = 3, size = 17, legend.position="bottom")
    cat("Plot: ", family,"\n")
    print(g)
  }
}
dev.off()

# Appendix S3-S6 ####
# Map model predictions vs observations for each dataset
# map.jsdm(results) ###
map.jsdm.pred(fit.bdmf, 'P2 maps bdmf')
map.jsdm.pred(fit.bdms, 'P2 maps bdms')
map.jsdm.pred(fit.invf, 'P2 maps invf')
map.jsdm.pred(fit.invfp, 'P2 maps invfp')

# Appendix S7-S10 ####
# Plot model predictions vs explanatory variables for each dataset
# plot.prob(results) ###
plot.prob(fit.bdmf, 'outputs/paper 2 results/P2 prob bdmf')
plot.prob(fit.invf, 'outputs/paper 2 results/P2 prob invf')
plot.prob(fit.bdms, 'outputs/paper 2 results/P2 prob bdms')
plot.prob(fit.invfp, 'outputs/paper 2 results/P2 prob invfp')

# Appendix S11 ####
# Map the selected explanatory variables
map.inputs(K, sample.bdmf, plot.title = "SGf/SGs", fileName = "outputs/paper 2 results/P2 maps inputs BDM")
map.inputs(K, sample.invf, plot.title = "SCf", fileName = "outputs/paper 2 results/P2 maps inputs CFCH")
map.inputs(K, sample.invfp, plot.title = "PCf", fileName = "outputs/paper 2 results/P2 maps inputs CFp")

# Other results ####
# Produce additional exploratory results

# CH vs plateau predictive deviance
# Get predictive performance for Invf CH (filter by plateau)
# Does all CH help predictive performance for combined families?

# Calculate deviance for 'invf' only in the plateau
invf.dev <- jsdm.cv.inv$probability %>%
  filter(SiteId %in% na.omit(env$SiteId[env$BIOGEO=="Mittelland"]), Trial=='invf') %>%
  group_by(Taxon, Fold) %>% # get deviance per taxon in each fold
  summarise(residual.deviance = sum(-2*log(ifelse(Obs == 1, Pred, 1-Pred)), na.rm = TRUE),
            n.samples = length(na.omit(Obs))) %>%
  mutate(std.deviance = residual.deviance/n.samples, Trial = "invf") %>%
  ungroup()

plot.data <- jsdm.cv.inv$deviance %>%
  filter(Trial =='invf_plat') %>%
  select(Taxon, Fold, residual.deviance, n.samples, std.deviance, Trial) %>%
  bind_rows(invf.dev) %>%
  group_by(Taxon, Trial) %>%
  summarise(mean.std.dev = mean(std.deviance)) %>%
  ungroup()
# mutate(Trial = ifelse(Trial=='invf', 'families CH \n(plateau only)', 'families plateau \n (plateau only)'))


# test <- plot.data %>%
#   spread(Trial, mean.std.dev) %>%
#   mutate(difference = invf-invf_plat) %>%
#   arrange(desc(difference)) %>%
#   select(Taxon, difference)

plot.data <- plot.data %>%
  left_join(test, by = "Taxon") %>%
  arrange(desc(difference))

boxplot(mean.std.dev ~ Trial , data = plot.data, 
        main = "Predictive performance by trial",
        xlab = "Trial (plateau sites only)",
        ylab = "Mean standardized deviance")

# plot.data$Label <- factor(plot.data$Taxon, levels(na.omit(test$Taxon)))

g <- ggplot(plot.data)
g <- g + geom_point(aes(x = reorder(Taxon, -difference), y = mean.std.dev, color = Trial), size = 3)
g <- g + theme_bw(base_size = 15)
# g <- g + theme(axis.text.x = element_text(angle=45,hjust=1))
g <- g + coord_flip()
g <- g + guides(colour = guide_legend(override.aes = list(size=6)))
g <- g + scale_color_brewer(palette = "Set1")
print(g)

# Select taxa for Nadine Remund
families <- c("Baetidae", "Nemouridae", "Leuctridae", "Heptageniidae", "Taeniopterygidae")

dt <- bind_rows(taxonomy.bdmf[taxonomy.bdmf$Taxon %in% families, ], taxonomy.bdms[taxonomy.bdms$Family %in% families,])
n <- c(n.bdmf, n.bdms)
dt$n <- n[dt$Taxon]
rm(families, n)

dt <- arrange(dt, Family, desc(n))
write.csv(dt, 'P2 families of interest.csv', row.names = FALSE)


# Aqua&Gas ####
plot.data <- tibble()
datasets <- c("sample.bdmf", "sample.invf", "sample.invfp")
for (d in 1:length(datasets)){
  dataset <- datasets[d]
  K <- c("Temp", "IAR", "WW", "Morph")
  dt <- prepare.inputs(K, get(dataset), center = F)
  dt <- gather(dt, Variable, Value, -SiteId, -SampId)
  dt$Trial <- dataset
  plot.data <- bind_rows(plot.data, dt)
}

# I would like to ask you if it is possible for you to produce a plot, similar to the attached one, were you add the NAWA dataset separately as an additional box plot (in the following order BDM, NAWA, CFCH (labelled "CHKf"), CFp (labelled MLKf) for Temperature (labelled "Temperatur"), Insecticide landuse index (labelled "InsLW") , wastewater fraction (labelled "Abwasseranteil") and ecomorphological assessment (labelled "Ökomorphologie").

# Add the monitoring program
sites <- select(env, SiteId, MonitoringProgram)
sotes <- unique(sites)
sites.vector <- sites$MonitoringProgram
names(sites.vector) <- sites$SiteId
plot.data$MP <- sites.vector[plot.data$SiteId]
# Add NAWA as a separate dataset
dt <- filter(plot.data, MP=="NAWA")
dt$Program <- "NAWA"

# Label "Program" accordingly
plot.data$Program <- plot.data$Trial
plot.data <- bind_rows(plot.data, dt)

plot.data$Program <- ifelse(plot.data$Program=="sample.bdmf", "BDM", plot.data$Program)
plot.data$Program <- ifelse(plot.data$Program=="sample.invf", "CHKf", plot.data$Program)
plot.data$Program <- ifelse(plot.data$Program=="sample.invfp", "MLKf", plot.data$Program)

plot.data$Program <- factor(plot.data$Program, levels = c("BDM", "NAWA", "CHKf", "MLKf"))

# Specify variable labels
plot.data <- filter(plot.data, Variable != "Temp2")
plot.data$Variable <- ifelse(plot.data$Variable == "Temp", "Temperatur", plot.data$Variable)
plot.data$Variable <- ifelse(plot.data$Variable == "IAR", "InsLW", plot.data$Variable)
plot.data$Variable <- ifelse(plot.data$Variable == "WW", "Abwasseranteil", plot.data$Variable)
plot.data$Variable <- ifelse(plot.data$Variable == "Morph", "Ökomorphologie", plot.data$Variable)

plot.data$Variable <- factor(plot.data$Variable, levels = c("Temperatur", "InsLW", "Abwasseranteil", "Ökomorphologie"))


# Boxplots
g <- ggplot(plot.data)
g <- g + geom_boxplot(aes(x=Program, y=Value, fill=Program), position = position_dodge2(preserve = "total"))
g <- g + stat_summary(aes(x=Program, y=Value), fun.y=median, colour="black", fill="black", geom="point", 
                      shape=21, size=3, show.legend = FALSE)
g <- g + facet_wrap(~Variable, scales="free", labeller=label_parsed, strip.position="bottom")

g <- g + theme_bw(base_size=26)
g <- g + labs(fill = "Data")
g <- g + theme(strip.background = element_blank(), strip.placement = "outside",
               plot.title = element_blank(),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank())
# g <- g + scale_x_discrete(limits=c("PCf", "SCf", "SGf/SGs"))
g <- g + scale_fill_manual(values=c("BDM"="#377eb8", "NAWA" = "#FF9900", "CHKf"="#4daf4a", "MLKf"="#984ea3"))
g <- g + scale_x_discrete(limits = rev(levels(plot.data$Program)))
g <- g + coord_flip()
g








# P2 dotplot CH vs plateau
beta.invf <- extract.beta(fit.invf)
beta.invf <- left_join(beta.invf, taxonomy.invf, by="Taxon")

beta.invfp <- extract.beta(fit.invfp)
beta.invfp <- left_join(beta.invfp, taxonomy.invfp, by="Taxon")

beta.invf <- beta.invf %>%
  group_by(Trial, Taxon, Variable) %>%
  summarise(beta.mean = mean(Value), quantile5 = quantile(Value, probs=0.05), quantile95 = quantile(Value, probs=0.95)) %>%
  ungroup() %>%
  left_join(taxonomy.invf, by="Taxon") %>%
  mutate(n = n.invf[Taxon]/nrow(sample.invf))

beta.invfp <- beta.invfp %>%
  group_by(Trial, Taxon, Variable) %>%
  summarise(beta.mean = mean(Value), quantile5 = quantile(Value, probs=0.05), quantile95 = quantile(Value, probs=0.95)) %>%
  ungroup() %>%
  left_join(taxonomy.invfp, by="Taxon") %>%
  mutate(n = n.invfp[Taxon]/nrow(sample.invfp))

beta.families.invfp <- bind_rows(beta.invf, beta.invfp)
rm(beta.invf, beta.invfp)

plot.data <- filter(beta.families.invfp, Rank == 'Family', Taxon %in% intersect(names(n.invf), names(n.invf)))
plot.data$Labels <- factor(paste(plot.data$Taxon, ' - ', n.invf[plot.data$Taxon]), levels = paste(names(sort(n.invf)), ' - ', sort(n.invf)))
plot.data$Trial[plot.data$Trial == "FF0_invf"] <- "SCf"
plot.data$Trial[plot.data$Trial == "FF0_invfp"] <- "PCf"

g <- ggplot(plot.data)
g <- g + geom_pointrange(aes(x=Labels, y=beta.mean, ymin=quantile5, ymax=quantile95, color=Trial))
g <- g + geom_hline(yintercept=0)
g <- g + facet_grid(.~Variable, scales="free")
g <- g + theme_bw(base_size=12)
g <- g + theme(axis.text.x = element_text(angle=45,hjust=1))
g <- g + coord_flip()
g <- g + guides(colour = guide_legend(override.aes = list(size=3))) # guides(colour=FALSE)

# RColorBrewer "Set1" palette:
brewer.pal(n=5, "Set1")
g <- g + scale_color_manual(values = c("SCf" = "#4DAF4A", "PCf" = "#984EA3")) 
g <- g + labs(title = "Marginal posterior taxon-specific parameter distributions",
              subtitle = "Means (points) and 95% confidence intervals (lines)",
              x = "Taxon and number of occurrences (in SCf)",
              y = "",
              color = "Dataset")
g <- g + labs(title="Mean of marginal posterior taxon-specific parameters (combined families CH vs plateau)")

pdf('outputs/paper 2 results/P2 response dotplot SCf-PCf.pdf', height=12, width=14)
print(g)
dev.off()

# IBCH families ###
# Indicator Biota CHs
library(ggridges)
taxa.ibch <- read.csv('IBCH_Taxa.dat', head=F, stringsAsFactors = F, sep=',')
taxa.ibch <- taxa.ibch$V1

# Extract parameters
beta.samples.bdmf <- extract.beta(fit.bdmf)
beta.samples.bdmf$Trial <- "SGf"

beta.samples.invf <- extract.beta(fit.invf)
beta.samples.invf$Trial <- "SCf"

beta.samples.ibch <- bind_rows(beta.samples.bdmf, beta.samples.invf)
rm(beta.samples.bdmf, beta.samples.invf)

beta.samples.ibch <- beta.samples.ibch[Taxon %in% taxa.ibch, ]

# Extract significant responses
response.bdmf <- extract.resp(jsdm.bdmf)
response.bdmf$Trial <- "SGf"
response.invf <- extract.resp(jsdm.invf)
response.invf$Trial <- "SCf"

response <- bind_rows(response.bdmf, response.invf)

responses <- gather(response, Variable, Response, -Taxon, -Trial)
responses$Response[responses$Response==0] <- "Not significant"
responses$Response[responses$Response==1] <- "Significant positive"
responses$Response[responses$Response==-1] <- "Significant negative"

plot.data <- beta.samples.ibch
plot.data <- left_join(plot.data, responses, by=c("Variable", "Taxon", "Trial"))


# Format labels
plot.data$TaxonLabel <- factor(plot.data$Taxon, levels=rev(taxa.ibch[taxa.ibch %in% unique(plot.data$Taxon)]))
plot.data$VariableLabel <- factor(plot.data$Variable, levels = K)
levels(plot.data$VariableLabel) <- labeller(levels(plot.data$VariableLabel))
plot.data$Trial <- as.factor(plot.data$Trial)

# Same data, but for CFCH vs CFp
response.invf <- extract.resp(fit.invf)
response.invf$Trial <- "SCf"

response.invfp <- extract.resp(fit.invfp)
response.invfp$Trial <- "PCf"

response <- bind_rows(response.invf, response.invfp)

# Significant responses
responses <- gather(response, Variable, Response, -Taxon, -Trial)
responses$Response[responses$Response==0] <- "Not significant"
responses$Response[responses$Response==1] <- "Significant positive"
responses$Response[responses$Response==-1] <- "Significant negative"
rm(response)

# beta_taxa
beta.samples.invf <- extract.beta(fit.invf)
beta.samples.invf$Trial <- "SCf"
beta.samples.invfp <- extract.beta(fit.invfp)
beta.samples.invfp$Trial <- "PCf"

beta.samples <- bind_rows(beta.samples.invf, beta.samples.invfp)
rm(beta.samples.invf, beta.samples.invfp)

plot.data <- as_tibble(beta.samples)
plot.data <- left_join(plot.data, responses, by=c("Variable", "Taxon", "Trial"))
plot.data <- filter(plot.data, Taxon %in% intersect(names(n.invf), names(n.invfp)))

plot.data$TaxonLabel <- factor(plot.data$Taxon, levels=rev(unique(plot.data$Taxon)))
plot.data$VariableLabel <- factor(plot.data$Variable, levels = K)
levels(plot.data$VariableLabel) <- labeller(levels(plot.data$VariableLabel))

plot.data <- plot.data[!is.na(plot.data$Response),]

g <- ggplot(plot.data)
g <- g + stat_density_ridges(aes(x = Value, y = TaxonLabel, linetype=Trial, fill = Response), alpha=0.7, rel_min_height=0.01)
g <- g + geom_vline(xintercept = 0, alpha=0.7)
g <- g + facet_grid(. ~ VariableLabel, scales="free", shrink=TRUE, labeller = label_parsed)

g <- g + theme_bw(base_size = 20)
g <- g + theme(plot.title=element_blank(),
               axis.text.x = element_text(angle=35,hjust=1),
               axis.text.y = element_text(hjust=1),
               axis.title.y = element_blank(),
               axis.title.x = element_text(size=24),
               strip.text = element_text(size = 14),
               legend.position="top")
g <- g + labs(x=expression(paste("Marginal posterior ",beta["kj"]^"taxa")),
              fill="",
              linetype = "Dataset")
g <- g + scale_y_discrete(expand = c(0.01, 0)) # pad the y-axis limits
g <- g + scale_linetype_manual(values=c("PCf"="dotted", "SCf"="solid"))
g <- g + scale_fill_manual(values=c("Significant positive"="blue", "Not significant"="grey", "Significant negative"="red"))
# g <- g + scale_fill_manual(values=c("BDMf"="red", "CFCH"="blue"))
# g <- g + scale_size_manual(values=c("BDMf"=1, "CFCH"=2))

pdf("outputs/paper 2 results/P2 parameters CFCH vs CFp.pdf", height = 25, width = 18)
print(g)
dev.off()

# Compare responses ###
families <- taxonomy.bdms %>% 
  group_by(Family) %>% 
  summarise(species=n()) %>%
  filter(!is.na(Family) & species > 1) %>%
  arrange(-species)

# Extract parameters
beta.samples.bdmf <- extract.beta(jsdm.bdmf)
setkey(beta.samples.bdmf, Taxon, Variable)
beta.samples.bdms <- extract.beta(jsdm.bdms)
setkey(beta.samples.bdms, Taxon, Variable)

# Extract significant responses
response.bdmf <- extract.resp(fit.bdmf)
response.bdms <- extract.resp(fit.bdms)

responses.diff <- data.table()
list.F <- lapply(families$Family, function(f){
  # Get the family response and taxonomy
  response <- response.bdmf[Taxon==f,]
  response <- select(response, -Taxon)
  response.family <- as.integer(response[1,])
  names(response.family) <- colnames(response)
  
  # Get the family taxonomy
  taxonomy <- taxonomy.bdms[Family==f,]
  
  # Loop through taxa in the family
  list.J <- lapply(taxonomy$Taxon, function(j){
    
    # Get the taxon response
    response <- response.bdms[Taxon==j,]
    response <- select(response, -Taxon)
    response.taxon <- as.integer(response[1,])
    names(response.taxon) <- colnames(response)
    
    # Loop through influence factors
    list.K <- lapply(K, function(k){
      
      # Get posterior distributions of taxon-specific parameters for family and taxon
      beta.sample.family <- beta.samples.bdmf[Taxon==f & Variable==k,]
      beta.sample.taxon <- beta.samples.bdms[Taxon==j & Variable==k,]
      
      # Perform Kolmogorov-Smirnov test
      # ks <- ks.test(x=beta.sample.family$Value, y=beta.sample.taxon$Value, alternative = "two.sided")
      # Perform Mann-Whitney U test (Wilcoxon rank sum test)
      # dt <- data.table(Family=f, Taxon=j, Variable=k, 
      #                  FamilyResponse=response.family[k], 
      #                  TaxonResponse=response.taxon[k], 
      #                  D=ks$statistic, 
      #                  p.value=ks$p.value)
      
      mwu <- wilcox.test(x=beta.sample.family$Value, y=beta.sample.taxon$Value)
      dt <- data.table(Family=f, Taxon=j, Variable=k,
                       FamilyResponse=response.family[k],
                       TaxonResponse=response.taxon[k],
                       W = mwu$statistic)
    })
    dt.K <- rbindlist(list.K)
  })
  dt.J <- rbindlist(list.J)
  cat("Statistics calculated", match(f, families$Family), "/", length(families$Family),"\n")
  return(dt.J)
})
response.diff <- rbindlist(list.F)
rm(list.F)

# Test p-value vs sample size
n <- 1:1000
n <- n*10
ps <- data.table()
for (i in 1:length(n)){
  sample.size <- n[i]
  bs.family.sample <- sample(beta.sample.family$Value, sample.size, replace=TRUE)
  bs.taxon.sample <- sample(beta.sample.taxon$Value, sample.size, replace=TRUE)
  
  mwu <- wilcox.test(x=bs.family.sample, y=bs.taxon.sample)
  dt <- data.table(p.value = mwu$p.value, statistic = mwu$statistic, sample.size = sample.size)
  ps <- bind_rows(ps, dt)
  cat(sample.size, "\n")
}


# What proportion of families contain species with different significant responses?
test <- response.ks %>%
  group_by(Family, Variable) %>%
  # mutate(n.taxa = uniqueN(Taxon)) %>%
  # Filter significant species responses that are different from their family
  filter(TaxonResponse != FamilyResponse, TaxonResponse != 0) %>%
  summarise(n.diff = n()) %>%
  left_join(families, by=c("Family"))
