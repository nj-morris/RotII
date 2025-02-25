#### Rotation II Project: Avian Blood Parasites ####
#### Written by: Natalie J. Morris ####
#### in collaboration with the McNew Lab, field work led by Henrey Dease #### 
#### Last Updated: 2/24/25 ####

library(dplyr)
library(ggplot2)
library(tidyr)

# Read in current metadata
samples <- read.csv("C:/Users/natal/OneDrive - University of Arizona/Rotation_II/RotII/rotation2_data.csv")

# Sample selection and metadata compilation 
      
      # data <- read.csv("birddata23s24_njm.csv")
      # DNA <- read.csv("BirdDNA23-24_njm - original order.csv")
      # samples <- read.csv("BirdDNA23-24_njm - data_samples.csv")


      # Explore data
      table(data$species)
      table(data$recap)
      View(data)
      View(DNA)
      
      # Merge metadata and DNA/PCR results by band_cap 
      data_DNA <- merge(samples, DNA, by = "band_cap_date", all = FALSE, no.dups = TRUE)
      View(data_DNA)
      
      write.csv(data_DNA, "DNA_sex.csv")
      
      # Pick samples from larger data set:
            table(DNA$species) %>% as.data.frame() %>% filter(Freq>9)
            table(data_DNA$species.x) %>% as.data.frame() %>% filter(Freq>9)
            
            DNA %>% filter(species=="WCSP") %>% dim()
            
            DNA %>% filter(species=="WCSP") %>% select(band) %>% unique() %>% dim()
              # unique band and good concentration 
            DNA %>% filter(species=="SOSP") %>% select(band) %>% unique() %>% View()
            
            
            table(DNA$band)
            
            # resident vs migrants
            # body condition
            # pick for ticks, band, concentration 20 for each species
            
            # Filter for WCSP species and randomly select 10 unique band numbers
            wcsp_samples <- DNA %>%
              filter(species == "WCSP" & is.na(ticks)) %>%
              distinct(band, .keep_all = TRUE) %>%
              slice_sample(n = 10)
            
            # View result
            print(wcsp_samples)
            
            sosp_samples <- DNA %>%
              filter(species == "SOSP" & ticks > 0) %>%
              filter(ng.uL > 50) %>%
              distinct(band, .keep_all = TRUE) %>%
              slice_sample(n = 10)
            
            # View result
            print(sosp_samples)



#### Calculate Body Condition / SMI for each species ####

# Abert's towhee / ABTO

# Remove rows where any relevant column has NA
ABTO_data <- samples %>%
  filter(speciescode == "ABTO") %>%
  drop_na(tarsusmm, massg)  # Remove rows with NA in key variables


ggplot(ABTO_data, aes(x = tarsusmm, y= massg)) +
  geom_point() +
  geom_smooth (method="lm")

ABTO_regression <- lm(log(massg) ~ log(tarsusmm), data = ABTO_data) 
summary(ABTO_regression)

#pulls coefficent from ols_regression so that can be used in formula, coefficinet [2] gives the slope 
Bols1 <- ABTO_regression$coefficients[2]
# Calculate R coefficient, related to line produced in the graph, gives idea of distance of dots to regression line
# cor asks computer to look at how correlated mass and tarsus are to each other
pearsons_r1 <- cor(
  x = log(ABTO_data$tarsusmm),
  y = log(ABTO_data$massg),
  use = "complete.obs",
  method = "pearson"
)

pearsons_r1^2
smi_slope1 = Bols1 / pearsons_r1 # calculate smi_slope 
smi_slope1 = as.vector(smi_slope1)# removes "log(Tarsus) from the number, just stores it as exact number (ie makes it into a vector)

mean_tarsus1 <- mean(ABTO_data$tarsusmm)
ABTO_data$body_condition <- ABTO_data$massg*((mean_tarsus1/ABTO_data$tarsusmm)^smi_slope1)
print(ABTO_data$body_condition)


# Curved billed thrasher

# Remove rows where any relevant column has NA
CBTH_data <- samples %>%
  filter(speciescode == "CBTH") %>%
  drop_na(tarsusmm, massg)  # Remove rows with NA in key variables


ggplot(CBTH_data, aes(x = tarsusmm, y= massg)) +
  geom_point() +
  geom_smooth (method="lm")

CBTH_regression <- lm(log(massg) ~ log(tarsusmm), data = CBTH_data) 
summary(CBTH_regression)

#pulls coefficent from ols_regression so that can be used in formula, coefficinet [2] gives the slope 
Bols2 <- CBTH_regression$coefficients[2]
# Calculate R coefficient, related to line produced in the graph, gives idea of distance of dots to regression line
# cor asks computer to look at how correlated mass and tarsus are to each other
pearsons_r2 <- cor(
  x = log(CBTH_data$tarsusmm),
  y = log(CBTH_data$massg),
  use = "complete.obs",
  method = "pearson"
)

pearsons_r2^2
smi_slope2 = Bols2 / pearsons_r2 # calculate smi_slope 
smi_slope2 = as.vector(smi_slope2)# removes "log(Tarsus) from the number, just stores it as exact number (ie makes it into a vector)

mean_tarsus2 <- mean(CBTH_data$tarsusmm)
CBTH_data$body_condition <- CBTH_data$massg*((mean_tarsus2/CBTH_data$tarsusmm)^smi_slope2)
print(CBTH_data$body_condition)

# 

# House finch

# Remove rows where any relevant column has NA
HOFi_data <- samples %>%
  filter(speciescode == "HOFI") %>%
  drop_na(tarsusmm, massg)  # Remove rows with NA in key variables



ggplot(HOFi_data, aes(x = tarsusmm, y= massg)) +
  geom_point() +
  geom_smooth (method="lm")

HOFI_regression <- lm(log(massg) ~ log(tarsusmm), data = HOFi_data) 
summary(HOFI_regression)

#pulls coefficent from ols_regression so that can be used in formula, coefficinet [2] gives the slope 
Bols <- HOFI_regression$coefficients[2]
# Calculate R coefficient, related to line produced in the graph, gives idea of distance of dots to regression line
# cor asks computer to look at how correlated mass and tarsus are to each other
pearsons_r <- cor(
  x = log(HOFi_data$tarsusmm),
  y = log(HOFi_data$massg),
  use = "complete.obs",
  method = "pearson"
)

pearsons_r^2
smi_slope = Bols / pearsons_r # calculate smi_slope 
smi_slope = as.vector(smi_slope)# removes "log(Tarsus) from the number, just stores it as exact number (ie makes it into a vector)

mean_tarsus <- mean(HOFi_data$tarsusmm)
HOFi_data$body_condition <- HOFi_data$massg*((mean_tarsus/HOFi_data$tarsusmm)^smi_slope)
print(HOFi_data$body_condition)

# Inca dove

# Remove rows where any relevant column has NA
INDO_data <- samples %>%
  filter(speciescode == "INDO") %>%
  drop_na(tarsusmm, massg)  # Remove rows with NA in key variables



ggplot(INDO_data, aes(x = tarsusmm, y= massg)) +
  geom_point() +
  geom_smooth (method="lm")

INDO_regression <- lm(log(massg) ~ log(tarsusmm), data = INDO_data) 
summary(INDO_regression)

#pulls coefficent from ols_regression so that can be used in formula, coefficinet [2] gives the slope 
Bols <- INDO_regression$coefficients[2]
# Calculate R coefficient, related to line produced in the graph, gives idea of distance of dots to regression line
# cor asks computer to look at how correlated mass and tarsus are to each other
pearsons_r <- cor(
  x = log(INDO_data$tarsusmm),
  y = log(INDO_data$massg),
  use = "complete.obs",
  method = "pearson"
)

pearsons_r^2
smi_slope = Bols / pearsons_r # calculate smi_slope 
smi_slope = as.vector(smi_slope)# removes "log(Tarsus) from the number, just stores it as exact number (ie makes it into a vector)

mean_tarsus <- mean(INDO_data$tarsusmm)
INDO_data$body_condition <- INDO_data$massg*((mean_tarsus/INDO_data$tarsusmm)^smi_slope)
print(INDO_data$body_condition)

# Lincoln sparrow

# Remove rows where any relevant column has NA
LISP_data <- samples %>%
  filter(speciescode == "LISP") %>%
  drop_na(tarsusmm, massg)  # Remove rows with NA in key variables



ggplot(LISP_data, aes(x = tarsusmm, y= massg)) +
  geom_point() +
  geom_smooth (method="lm")

LISP_regression <- lm(log(massg) ~ log(tarsusmm), data = LISP_data) 
summary(LISP_regression)

#pulls coefficent from ols_regression so that can be used in formula, coefficinet [2] gives the slope 
Bols <- LISP_regression$coefficients[2]
# Calculate R coefficient, related to line produced in the graph, gives idea of distance of dots to regression line
# cor asks computer to look at how correlated mass and tarsus are to each other
pearsons_r <- cor(
  x = log(LISP_data$tarsusmm),
  y = log(LISP_data$massg),
  use = "complete.obs",
  method = "pearson"
)

pearsons_r^2
smi_slope = Bols / pearsons_r # calculate smi_slope 
smi_slope = as.vector(smi_slope)# removes "log(Tarsus) from the number, just stores it as exact number (ie makes it into a vector)

mean_tarsus <- mean(LISP_data$tarsusmm)
LISP_data$body_condition <- LISP_data$massg*((mean_tarsus/LISP_data$tarsusmm)^smi_slope)
print(LISP_data$body_condition)

# Pyrrhuloxia PYRR

# Remove rows where any relevant column has NA
PYRR_data <- samples %>%
  filter(speciescode == "PYRR") %>%
  drop_na(tarsusmm, massg)  # Remove rows with NA in key variables



ggplot(PYRR_data, aes(x = tarsusmm, y= massg)) +
  geom_point() +
  geom_smooth (method="lm")

PYRR_regression <- lm(log(massg) ~ log(tarsusmm), data = PYRR_data) 
summary(PYRR_regression)

#pulls coefficent from ols_regression so that can be used in formula, coefficinet [2] gives the slope 
Bols <- PYRR_regression$coefficients[2]
# Calculate R coefficient, related to line produced in the graph, gives idea of distance of dots to regression line
# cor asks computer to look at how correlated mass and tarsus are to each other
pearsons_r <- cor(
  x = log(PYRR_data$tarsusmm),
  y = log(PYRR_data$massg),
  use = "complete.obs",
  method = "pearson"
)

pearsons_r^2
smi_slope = Bols / pearsons_r # calculate smi_slope 
smi_slope = as.vector(smi_slope)# removes "log(Tarsus) from the number, just stores it as exact number (ie makes it into a vector)

mean_tarsus <- mean(PYRR_data$tarsusmm)
PYRR_data$body_condition <- PYRR_data$massg*((mean_tarsus/PYRR_data$tarsusmm)^smi_slope)
print(PYRR_data$body_condition)


# Yellow rumped warbler YRWA

# Remove rows where any relevant column has NA
YWRA_data <- samples %>%
  filter(speciescode == "YRWA") %>%
  drop_na(tarsusmm, massg)  # Remove rows with NA in key variables

ggplot(YWRA_data, aes(x = tarsusmm, y= massg)) +
  geom_point() +
  geom_smooth (method="lm")

YWRA_regression <- lm(log(massg) ~ log(tarsusmm), data = YWRA_data) 
summary(YWRA_regression)

#pulls coefficent from ols_regression so that can be used in formula, coefficinet [2] gives the slope 
Bols <- YWRA_regression$coefficients[2]
# Calculate R coefficient, related to line produced in the graph, gives idea of distance of dots to regression line
# cor asks computer to look at how correlated mass and tarsus are to each other
pearsons_r <- cor(
  x = log(YWRA_data$tarsusmm),
  y = log(YWRA_data$massg),
  use = "complete.obs",
  method = "pearson"
)

pearsons_r^2
smi_slope = Bols / pearsons_r # calculate smi_slope 
smi_slope = as.vector(smi_slope)# removes "log(Tarsus) from the number, just stores it as exact number (ie makes it into a vector)

mean_tarsus <- mean(YWRA_data$tarsusmm)
YWRA_data$body_condition <- YWRA_data$massg*((mean_tarsus/YWRA_data$tarsusmm)^smi_slope)
print(YWRA_data$body_condition)


# song sparrow SOSP

# Remove rows where any relevant column has NA
SOSP_data <- samples %>%
  filter(speciescode == "SOSP") %>%
  drop_na(tarsusmm, massg)  # Remove rows with NA in key variables



ggplot(SOSP_data, aes(x = tarsusmm, y= massg)) +
  geom_point() +
  geom_smooth (method="lm")

SOSP_regression <- lm(log(massg) ~ log(tarsusmm), data = SOSP_data) 
summary(SOSP_regression)

#pulls coefficent from ols_regression so that can be used in formula, coefficinet [2] gives the slope 
Bols <- SOSP_regression$coefficients[2]
# Calculate R coefficient, related to line produced in the graph, gives idea of distance of dots to regression line
# cor asks computer to look at how correlated mass and tarsus are to each other
pearsons_r <- cor(
  x = log(SOSP_data$tarsusmm),
  y = log(SOSP_data$massg),
  use = "complete.obs",
  method = "pearson"
)

pearsons_r^2
smi_slope = Bols / pearsons_r # calculate smi_slope 
smi_slope = as.vector(smi_slope)# removes "log(Tarsus) from the number, just stores it as exact number (ie makes it into a vector)

mean_tarsus <- mean(SOSP_data$tarsusmm)
SOSP_data$body_condition <- SOSP_data$massg*((mean_tarsus/SOSP_data$tarsusmm)^smi_slope)
print(SOSP_data$body_condition)

# white crowned sparrow WCSP

# Remove rows where any relevant column has NA
WCSP_data <- samples %>%
  filter(speciescode == "WCSP") %>%
  drop_na(tarsusmm, massg)  # Remove rows with NA in key variables



ggplot(WCSP_data, aes(x = tarsusmm, y= massg)) +
  geom_point() +
  geom_smooth (method="lm")

WCSP_regression <- lm(log(massg) ~ log(tarsusmm), data = WCSP_data) 
summary(WCSP_regression)

#pulls coefficent from ols_regression so that can be used in formula, coefficinet [2] gives the slope 
Bols <- WCSP_regression$coefficients[2]
# Calculate R coefficient, related to line produced in the graph, gives idea of distance of dots to regression line
# cor asks computer to look at how correlated mass and tarsus are to each other
pearsons_r <- cor(
  x = log(WCSP_data$tarsusmm),
  y = log(WCSP_data$massg),
  use = "complete.obs",
  method = "pearson"
)

pearsons_r^2
smi_slope = Bols / pearsons_r # calculate smi_slope 
smi_slope = as.vector(smi_slope)# removes "log(Tarsus) from the number, just stores it as exact number (ie makes it into a vector)

mean_tarsus <- mean(WCSP_data$tarsusmm)
WCSP_data$body_condition <- WCSP_data$massg*((mean_tarsus/WCSP_data$tarsusmm)^smi_slope)
print(WCSP_data$body_condition)


### Combine all results
samples <- bind_rows(ABTO_data, CBTH_data, HOFi_data, INDO_data, LISP_data, PYRR_data, SOSP_data, WCSP_data, YWRA_data)

# Plot body condition across 
ggplot(samples, aes(x = species, y = body_condition, fill = plasm.positive)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Scaled Mass Index (per species)", fill = "Plasmodium Infection Status")


ggplot(samples, aes(x = species, y = body_condition, fill = leuko.positive)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Scaled Mass Index (per species)", fill = "Leukocytozoon Infection Status")

# Create a new column indicating if an individual has ticks
samples$ticks_present <- ifelse(samples$ticks > 0, "yes", "no")

# Create a new column indicating if an individual has ticks
samples$inf_status <- ifelse(samples$hemo.positive== "yes" | samples$plasm.positive == "yes" | samples$leuko.positive == "yes", "yes", "no")

library(ggplot2)

ggplot(samples, aes(x = species, fill = ticks_present)) +
  geom_bar(position = "fill") +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Species", y = "Proportion of Individuals", fill = "Ticks Present") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate species names


ggplot(samples, aes(x = species, y = body_condition, fill = ticks_present)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Scaled Mass Index (per species)", group = "Tick Infection Status")


#### Data exploration ####

# Create stacked bar plot of all blood parasite infections across species
df_long <- samples %>%
  pivot_longer(cols = c(leuko.positive, plasm.positive, hemo.positive),
               names_to = "infection_type", values_to = "result")
ggplot(df_long, aes(x = species, fill = result)) +
  geom_bar(position = "fill") +  # Stacked bar plot with proportions
  facet_wrap(~infection_type) +  # Separate panels for each infection type
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Species", y = "Proportion", fill = "Result") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Subset individuals with more than one "yes" in infection columns
multi <- samples %>%
  rowwise() %>%  # Ensures row-wise calculations
  filter(sum(c_across(hemo.positive:leuko.positive) == "yes") > 1) %>% 
  ungroup()

# Subset to individuals with ticks and infected
tick_infected <- samples %>%
  filter(ticks > 0 & (leuko.positive == "yes" | plasm.positive == "yes" | hemo.positive == "yes"))

table(samples$plasm.positive) # 42 yes / 84 no
table(samples$leuko.positive) # 19 yes / 107 no
table(samples$ticks_present) # 33 yes / 93 no

table(data$hemo.positive, data$species)
table(data$plasm.positive, data$species) # at least four, then can test inf/un for body
# CBTH, LISP, SOSP, YRUMP
table(data$leuko.positive, data$species) # not enough power

# Look at infection across sites
ggplot(samples, aes(x = site, fill = plasm.positive)) +
  geom_bar(position = "fill") +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Site", y = "Proportion of Individuals", fill = "Plasmodium Positive") +
  theme_minimal()

ggplot(samples, aes(x = site, fill = leuko.positive)) +
  geom_bar(position = "fill") +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Site", y = "Proportion of Individuals", fill = "Leuko Positive") +
  theme_minimal()

# Explore resident/migrant
ggplot(samples, aes(x = resident_migrant, fill = plasm.positive)) +
  geom_bar(position = "fill") +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Site", y = "Proportion of Individuals", fill = "Plasmodium Positive") +
  theme_minimal()

ggplot(samples, aes(x = resident_migrant, fill = leuko.positive)) +
  geom_bar(position = "fill") +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Site", y = "Proportion of Individuals", fill = "Leuko Positive") +
  theme_minimal()

# Explore infection across age and date
ggplot(samples, aes(x = age, fill = leuko.positive)) +
  geom_bar() +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Site", y = "Individuals", fill = "Leuko +") +
  theme_minimal()

ggplot(samples, aes(x = resident_migrant, fill = leuko.positive)) +
  geom_bar(position = "fill") +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Site", y = "Proportion of Individuals", fill = "Leuko Positive") +
  theme_minimal()

ggplot(samples, aes(x = ticks_present, fill = leuko.positive)) +
  geom_bar() +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Ticks", y = "Individuals", fill = "Leuko Positive") +
  theme_classic() + facet_wrap(~species) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#### Modeling ####
library(lme4)
library(lmerTest)
library(emmeans)
# site as a random effect

# AH infection across species
# AH infection across migratory vs resident
# AH infection and body condition
# Co-occurrence of ticks and AH

write.csv(samples, "rotation2_data.csv")

# Body condition and infection

# Subset to 
# CBTH, LISP, SOSP, YRUMP
bc <- samples %>% filter(speciescode %in% c("CBTH", "LISP", "SOSP", "YRWA"))
bc

body_inf <- glmer(body_condition ~ inf_status*species + (1|site), data = bc)
summary(body_inf)

emm_bc <- emmeans(body_inf, ~ inf_status*species)
pairs(emm_bc, adjust = "tukey")

bc$predicted_bc <- predict(body_inf, type = "response")  # Includes random effects

ggplot(bc, aes(x = inf_status, y = body_condition, color = species, group = species)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.2, height = 0)) +  # Observed data with jitter
  stat_summary(fun = mean, geom = "line", size = .7) +  # Mean predicted body condition per species
  stat_summary(
    aes(y = predicted_bc, group = species, fill = species),  
    fun.data = mean_cl_normal, geom = "ribbon", alpha = 0.2, color = NA
  ) +  # Confidence interval shading
  labs(
    x = "Infection Status",
    y = "Body Condition (Predicted)",
    color = "Species",
    fill = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA)
  )


# Infection by ticks and parasites
samples$ticks_present <- as.numeric(samples$ticks_present == "yes")

inf_tick <- glmer(inf_status ~ ticks_present + (1|site) + (1|species), data = samples, family = binomial) 
summary(inf_tick)

emm_bc <- emmeans(body_inf, ~ inf_status*species)
pairs(emm_bc, adjust = "tukey")

# Infection of resident vs migratory birds: Migratory Escape
samples$inf_status <- as.numeric(samples$inf_status == "yes")
samples$plasm.positive <- as.numeric(samples$plasm.positive == "yes")
samples$leuko.positive <- as.numeric(samples$leuko.positive == "yes")

samples$resident_migrant <- as.factor(samples$resident_migrant)

inf_mig <- glmer(inf_status ~ resident_migrant + (1|species), data = samples, family = binomial)
summary(inf_mig)

inf_spec <- glmer(inf_status ~ species +(1|site), data = samples, family = binomial)
summary(inf_spec)

