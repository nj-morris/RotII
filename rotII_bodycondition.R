#### Rotation II Project: Avian Blood Parasites ####
#### Written by: Natalie J. Morris ####
#### in collaboration with the McNew Lab, field work led by Henrey Dease #### 
#### Last Updated: 2/24/25 ####
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
