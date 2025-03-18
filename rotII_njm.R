#### Rotation II Project: Avian Blood Parasites ####
#### Written by: Natalie J. Morris ####
#### in collaboration with the McNew Lab, field work led by Henrey Dease #### 
#### Last Updated: 3/18/25 ####

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(visreg)
library(stringr)

# Read in current metadata
samples <- read.csv("C:/Users/natal/OneDrive - University of Arizona/Rotation_II/RotII/rotation2_data.csv")

# Format
samples$cap_date <- as.Date(samples$cap_date, format = "%m/%d/%Y")

# Create a new column indicating if an individual has ticks
samples$ticks_present <- ifelse(samples$ticks > 0, "yes", "no")

# Create a new column indicating if an individual has one or more blood parasite
samples$inf_status <- ifelse(samples$hemo.positive== "yes" | samples$plasm.positive == "yes" | samples$leuko.positive == "yes", "yes", "no")

samples$inf_status_num <- as.numeric(samples$inf_status == "yes")
samples$plasm.positive_num <- as.numeric(samples$plasm.positive == "yes")
samples$leuko.positive_num <- as.numeric(samples$leuko.positive == "yes")
samples$resident_migrant <- as.factor(samples$resident_migrant)
samples$ticks_present_num <- as.numeric(samples$ticks_present == "yes")

samples$cap_date_numeric <- as.numeric(samples$cap_date)  # Convert to numeric

# Reorder
table(samples$species)

samples$speciescode <- factor(samples$speciescode, levels=c("INDO", "PYRR", "CBTH","ABTO", "HOFI", "YRWA", "WCSP", "SOSP", "LISP"))


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


#### Data exploration ####

# Create stacked bar plot of all blood parasite infections across species
df_long <- samples %>%
  pivot_longer(cols = c(leuko.positive, plasm.positive, hemo.positive),
               names_to = "Blood_Parasite", values_to = "result")
ggplot(df_long, aes(x = species, fill = result)) +
  geom_bar() +  # Stacked bar plot 
  facet_wrap(~Blood_Parasite) +  # Separate panels for each infection type
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Species", y = "Count", fill = "Result") +
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
table(samples$blood_parasite)
table(samples$infection_category)

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
  geom_bar() +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "blue", "no" = "grey")) +  # Custom colors
  labs(x = "Migrant/Resident", y = "Individual Count", fill = "Plasmodium Positive") +
  theme_minimal() + theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA)
  )

ggplot(samples, aes(x = resident_migrant, fill = leuko.positive)) +
  geom_bar() +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Migrant/Resident", y = "Individual Count", fill = "Leukocytozoon Positive") +
  theme_minimal() + theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA)
  )

# Explore infection across age and date
ggplot(samples, aes(x = age, fill = leuko.positive)) + # change to plasm
  geom_bar() + 
  facet_wrap(~species) +  #
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Age", y = "Individuals", fill = "Leuko +") +
  theme_minimal()

    # Proportion
    ggplot(samples, aes(x = age, fill = leuko.positive)) + # change to plasm
      geom_bar(position = "fill") + 
      facet_wrap(~species) +  #
      scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
      labs(x = "Age", y = "Individuals", fill = "Leuko +") +
      theme_minimal()

ggplot(samples, aes(x = cap_date, fill = plasm.positive)) +
  geom_bar() +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Site", y = "Proportion of Individuals", fill = "Leuko Positive") +
  theme_minimal()

# Explore ticks and infection

# Create a new column categorizing infection status based on all three parasites
samples <- samples %>%
  mutate(
    infection_category = case_when(
      plasm.positive == "no" & leuko.positive == "no" & ticks_present == "no" ~ "none",
      plasm.positive == "yes" & leuko.positive == "no" & ticks_present == "no" ~ "Plasmodium+",
      plasm.positive == "no" & leuko.positive == "yes" & ticks_present == "no" ~ "Leukocytozoon+",
      plasm.positive == "no" & leuko.positive == "no" & ticks_present == "yes" ~ "Ticks+",
      plasm.positive == "yes" & leuko.positive == "yes" & ticks_present == "no" ~ "Plasmodium+ Leukocytozoon+",
      plasm.positive == "yes" & leuko.positive == "no" & ticks_present == "yes" ~ "Plasmodium+ Ticks+",
      plasm.positive == "no" & leuko.positive == "yes" & ticks_present == "yes" ~ "Leukocytozoon+ Ticks+",
      plasm.positive == "yes" & leuko.positive == "yes" & ticks_present == "yes" ~ "Plasmodium+ Leukocytozoon+ Ticks+"
    )
  )

# Summarize data: Calculate proportions within each species
summary_df <- samples %>%
  group_by(species, infection_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(species) %>%
  mutate(proportion = count / sum(count))

# Custom color palette for each infection category
custom_colors <- c("none" = "gray70", 
                   "Plasmodium+" = "blue", 
                   "Leukocytozoon+" = "darkgreen", 
                   "Ticks+" = "purple", 
                   "Plasmodium+ Leukocytozoon+" = "cyan", 
                   "Plasmodium+ Ticks+" = "orange", 
                   "Leukocytozoon+ Ticks+" = "pink", 
                   "Plasmodium+ Leukocytozoon+ Ticks+" = "red")

# Plot the bar graph
ggplot(summary_df, aes(x = species, y = proportion, fill = infection_category)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(
    x = "Species",
    y = "Proportion of Individuals",
    fill = "Infection Status"
  ) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability



# Create a new column categorizing infection status with combined blood parasites
samples <- samples %>%
  mutate(
    blood_parasite = ifelse(plasm.positive == "yes" | leuko.positive == "yes", "yes", "no"),
    infection_category = case_when(
      blood_parasite == "no" & ticks_present == "no" ~ "none",
      blood_parasite == "yes" & ticks_present == "no" ~ "Blood Parasite(s)",
      blood_parasite == "no" & ticks_present == "yes" ~ "Ticks",
      blood_parasite == "yes" & ticks_present == "yes" ~ "Blood Parasite(s) & Ticks"
    )
  )

# Summarize data: Calculate proportions within each species
summary_df <- samples %>%
  group_by(species, infection_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(species) %>%
  mutate(proportion = count / sum(count))
summary_df$species <- factor(summary_df$species, levels=c("inca dove", "pyrrhuloxia", "curve-billed thrasher","Abert's towhee", "house finch", "yellow-rumped warbler", "white-crowned sparrow", "song sparrow", "Lincoln's sparrow"))
summary_df$infection_category <- factor(summary_df$infection_category, levels=c("none", "Blood Parasite(s)", "Blood Parasite(s) & Ticks","Ticks"))

# Custom color palette for each infection category
custom_colors <- c("none" = "gray70", 
                   "Blood Parasite(s)" = "red", 
                   "Ticks" = "brown", 
                   "Blood Parasite(s) & Ticks" = "black")

# Plot the bar graph
ggplot(summary_df, aes(x = species, y = count, fill = infection_category)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(
    x = "Avian Species",
    y = "Count",
    fill = "Infection Status"
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  # Wrap text at ~10 characters
  
  # Add brackets for Residents
  geom_segment(aes(x = 0.7, xend = 5.3, y = max(summary_df$count) * 1.4, 
                   yend = max(summary_df$count) * 1.4), size = 0.8) +
  annotate("text", x = 3, y = max(summary_df$count) * 1.5, label = "Resident", size = 5) +
  # Add brackets for Migrants
  geom_segment(aes(x = 5.7, xend = 9.3, y = max(summary_df$count) * 1.4, 
                   yend = max(summary_df$count) * 1.4), size = 0.8) +
  annotate("text", x = 7.5, y = max(summary_df$count) * 1.5, label = "Migrant", size = 5) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA))



ggplot(samples, aes(x = ticks_present, fill = leuko.positive)) +
  geom_bar() +  # Stacked proportion bars
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
  labs(x = "Ticks", y = "Individuals", fill = "Leuko Positive") +
  theme_classic() + facet_wrap(~species) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Plot body condition across species and infection
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



#### Modeling ####
library(lme4)
library(lmerTest)
library(emmeans)
library(kableExtra) 
library(gtsummary)

# site as a random effect

# AH infection across species
### Infection across species
inf_specA <- glmer(inf_status_num ~ species +(1|site), data = samples, family = binomial)
summary(inf_specA) # 

# use emmeans
inf_spec <- glmer(plasm.positive_num ~ species +(1|site), data = samples, family = binomial)
summary(inf_spec) # singualrity

emm_is <- emmeans(inf_spec, pairwise ~ species)
emm_is

contrast_df <- as.data.frame(emm_is$contrasts)
contrast_df %>%
  kable(caption = "Pairwise Comparisons of `emmeans`") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

inf_spec <- glmer(leuko.positive_num ~ species +(1|site), data = samples, family = binomial)
summary(inf_spec) # singualrity

emm_is <- emmeans(inf_spec, pairwise ~ species)
emm_is

contrast_df <- as.data.frame(emm_is$contrasts)
contrast_df %>%
  kable(caption = "Pairwise Comparisons of `emmeans`") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# AH infection across migratory vs resident

#### Infection of resident vs migratory birds: Migratory Escape
inf_mig <- glmer(inf_status_num ~ resident_migrant + (1|species) + (1|site), data = samples, family = binomial)
summary(inf_mig)

tbl_regression(inf_mig, exponentiate = FALSE, pvalue_fun = label_style_pvalue(digits = 3),
) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()

inf_mig <- glmer(plasm.positive_num ~ resident_migrant + (1|species), data = samples, family = binomial)
summary(inf_mig)

tbl_regression(inf_mig, exponentiate = FALSE, pvalue_fun = label_style_pvalue(digits = 3),
) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()

inf_mig <- glmer(leuko.positive_num ~ resident_migrant + (1|species), data = samples, family = binomial)
summary(inf_mig)



# AH infection and body condition

#### Body condition and infection

# Subset to CBTH, LISP, SOSP, YWRA
bc <- samples %>% filter(speciescode %in% c("CBTH", "LISP", "SOSP", "YRWA"))
bc

bc$speciescode <- factor(bc$speciescode, levels=c("LISP", "CBTH", "YRWA", "SOSP"))


body_inf <- lmer(body_condition ~ inf_status*speciescode + (1|site), data = bc)
summary(body_inf) # 

body_inf <- lmer(body_condition ~ plasm.positive*species + (1|site) + age, data = bc)
summary(body_inf) # 

body_inf <- lmer(body_condition ~ leuko.positive*species + (1|site) + age, data = bc)
summary(body_inf) # 

    # testing by species
    CBTH <- samples %>% filter(speciescode == "CBTH")
    hist(CBTH$body_condition)
    body_infC <- lmer(body_conditionT ~ inf_status + (1|site) + age, data = CBTH)
    summary(body_infC) # 
    
    LISP <- samples %>% filter(speciescode == "LISP")
    hist(LISP$body_condition)
    body_infC <- lmer(body_condition ~ inf_status + (1|site) + age, data = LISP)
    summary(body_infC) # 
    
    YRWA <- samples %>% filter(speciescode == "YRWA")
    hist(YRWA$body_condition)
    body_infC <- lmer(body_condition ~ inf_status + (1|site) + age, data = YRWA)
    summary(body_infC) # 
    
    SOSP <- samples %>% filter(speciescode == "SOSP")
    hist(SOSP$body_condition)
    body_infC <- lmer(body_condition ~ inf_status + (1|site) + age, data = SOSP)
    summary(body_infC) # 



emm_bc <- emmeans(body_inf, pairwise ~ inf_status*speciescode)
emm_bc

contrast_df <- as.data.frame(emm_bc$contrasts)
contrast_df %>%
  kable(caption = "Pairwise Comparisons of `emmeans`") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))



#### Infection by ticks and parasites
tick <- samples %>% filter(
  speciescode %in% c("CBTH", "LISP", "SOSP", "ABTO", "PYRR"))

inf_tick <- glmer(ticks_present_num ~ inf_status_num + resident_migrant + (1|species) + (1|cap_date_numeric) + (1|site), data = tick, family = binomial) 
summary(inf_tick)

ticky <- glmer(inf_status_num ~ ticks_present + (1|species), data = tick, family = binomial)
summary(ticky)


emm_t <- emmeans(inf_tick, pairwise ~ species)
emm_t

contrast_df <- as.data.frame(emm_t$contrasts)
contrast_df %>%
  kable(caption = "Pairwise Comparisons of `emmeans` Ticks ~ Inf*Species") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

inf_tick <- glmer(ticks_present_num ~ plasm.positive  + (1|species) + (1|cap_date_numeric) + (1|site), data = tick, family = binomial) 
summary(inf_tick) # singularity, species - variance

inf_tick <- glmer(ticks_present_num ~ leuko.positive + (1|species) + (1|cap_date_numeric),  data = tick, family = binomial) 
summary(inf_tick)

##### change cap date to date 
samples$cap_date <- as.Date(samples$cap_date, format = "%m/%d/%Y") # Ensure proper date conversion
samples$cap_date_numeric <- as.numeric(samples$cap_date)  # Convert to numeric


##### Plots for presentation


# Create a new column categorizing infection status based on all three parasites
samples <- samples %>%
  mutate(
    infection_category = case_when(
      plasm.positive == "no" & leuko.positive == "no" & hemo.positive == "no" ~ "none",
      plasm.positive == "yes" & leuko.positive == "no"  ~ "Plasmodium +",
      plasm.positive == "no" & leuko.positive == "yes"  ~ "Leukocytozoon +",
      plasm.positive == "yes" & leuko.positive == "yes" ~ "Plasmodium + & Leukocytozoon +",
      hemo.positive == "yes" ~ "Hemoproteus +"
    )
  )

# Summarize data: Calculate proportions within each species
summary_df <- samples %>%
  group_by(species, infection_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(species) %>%
  mutate(proportion = count / sum(count))

# Custom color palette for each infection category
custom_colors <- c("none" = "gray70", 
                   "Plasmodium +" = "blue", 
                   "Leukocytozoon +" = "red", 
                   "Plasmodium + & Leukocytozoon +" = "purple", 
                   "Hemoproteus +" = "pink")

summary_df$infection_category <- factor(summary_df$infection_category, levels = c("none", "Hemoproteus +", "Plasmodium +", "Leukocytozoon +", 
                                                                                  "Plasmodium + & Leukocytozoon +"))
summary_df$species <- factor(summary_df$species, levels=c("inca dove", "pyrrhuloxia", "curve-billed thrasher","Abert's towhee", "house finch", "yellow-rumped warbler", "white-crowned sparrow", "song sparrow", "Lincoln's sparrow"))

ggplot(summary_df, aes(x = species, y = count, fill = infection_category)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(
    x = "Avian Species",
    y = "Count",
    fill = "Infection Status"
  ) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA))


ggplot(summary_df, aes(x = species, y = count, fill = infection_category)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(
    x = "Avian Species",
    y = "Count",
    fill = "Infection Status"
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  # Wrap text at ~10 characters
  # Add brackets for Residents
  geom_segment(aes(x = 0.7, xend = 5.3, y = max(summary_df$count) * 1.4, 
                   yend = max(summary_df$count) * 1.4), size = 0.8) +
  annotate("text", x = 3, y = max(summary_df$count) * 1.5, label = "Resident", size = 5) +
  # Add brackets for Migrants
  geom_segment(aes(x = 5.7, xend = 9.3, y = max(summary_df$count) * 1.4, 
                   yend = max(summary_df$count) * 1.4), size = 0.8) +
  annotate("text", x = 7.5, y = max(summary_df$count) * 1.5, label = "Migrant", size = 5) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA)
  )



###### Plot species count

summary_df <- samples %>%
  group_by(speciescode) %>%
  summarise(count = n(), .groups = "drop") 

library(stringr)  # For str_wrap

table(samples$species)
summary_df$species <- factor(summary_df$species, levels=c("inca dove", "pyrrhuloxia", "curve-billed thrasher","Abert's towhee", "house finch", "yellow-rumped warbler", "white-crowned sparrow", "song sparrow", "Lincoln's sparrow"))
summary_df <- samples %>%
  group_by(resident_migrant, species) %>%  # Group by both variables
  summarise(count = n(), .groups = "drop") 


ggplot(summary_df, aes(x = species, y = count)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(
    x = "",
    y = "Count",
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  # Wrap text at ~10 characters
  geom_text(aes(label = count), vjust = -0.5, size = 4) +  # Add count labels above bars
  # Add brackets for Residents
  geom_segment(aes(x = 0.7, xend = 5.3, y = max(summary_df$count), 
                   yend = max(summary_df$count)), size = 0.8) +
  annotate("text", x = 3, y = max(summary_df$count) *1.1, label = "Resident", size = 5) +
  # Add brackets for Migrants
  geom_segment(aes(x = 5.7, xend = 9.3, y = max(summary_df$count) * 1.2, 
                   yend = max(summary_df$count) * 1.2), size = 0.8) +
  annotate("text", x = 7.5, y = max(summary_df$count) * 1.3, label = "Migrant", size = 5) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA)
  )

# Plot body condition
ggplot(bc, aes(x = inf_status, y = body_condition, fill = inf_status)) +
  geom_boxplot() +  facet_grid(~species) +
  labs(
    x = "Infection Status",
    y = "Body Condition (SMI)",
    color = "Species",
    fill = "Blood Parasite +"
  ) +
  scale_fill_manual(values = c("yes" = "red", "no" = "grey")) +  # Custom colors
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

