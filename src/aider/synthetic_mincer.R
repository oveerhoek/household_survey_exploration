set.seed(123)

n <- 15000

# Education in years (SOEP-like distribution)
education_years <- sample(8:20, n, replace = TRUE, prob = dnorm(8:20, mean = 13, sd = 2.5))

# Potential labor market experience
age <- sample(22:65, n, replace = TRUE)
experience <- pmax(age - education_years - 6, 0)

# Gender
gender <- sample(c("male", "female"), n, replace = TRUE, prob = c(0.5, 0.5))

# Migrant background
migrant_background <- sample(c("no", "yes"), n, replace = TRUE, prob = c(0.8, 0.2))

# Region (SOEP-style East/West Germany)
region <- sample(c("west", "east"), n, replace = TRUE, prob = c(0.8, 0.2))

# Marital status
marital_status <- sample(
  c("single", "married", "divorced", "widowed"),
  n,
  replace = TRUE,
  prob = c(0.4, 0.45, 0.1, 0.05)
)

# Part-time status
part_time <- sample(c("no", "yes"), n, replace = TRUE, prob = c(0.7, 0.3))

# Occupation (simplified ISCO-like groups)
occupation <- sample(
  c("manager", "professional", "technician", "clerical", "service", "craft", "operator", "elementary"),
  n,
  replace = TRUE
)

# Industry (simplified NACE-style groups)
industry <- sample(
  c("manufacturing", "construction", "trade", "transport", "finance", "public_services", "education_health", "other_services"),
  n,
  replace = TRUE
)

# Public sector
public_sector <- sample(c("no", "yes"), n, replace = TRUE, prob = c(0.75, 0.25))

# Firm size categories (common in SOEP-style datasets)
firm_size <- sample(
  c("1-9", "10-49", "50-199", "200-1999", "2000+"),
  n,
  replace = TRUE,
  prob = c(0.15, 0.25, 0.25, 0.2, 0.15)
)

# Mincer equation components
beta_educ <- 0.08
beta_exp <- 0.04
beta_exp2 <- -0.0007

# Additional effects
gender_effect <- ifelse(gender == "female", -0.12, 0)
migrant_effect <- ifelse(migrant_background == "yes", -0.08, 0)
parttime_effect <- ifelse(part_time == "yes", -0.25, 0)
public_effect <- ifelse(public_sector == "yes", 0.05, 0)
east_effect <- ifelse(region == "east", -0.1, 0)

# Random noise
epsilon <- rnorm(n, 0, 0.25)

# Log wages based on Mincer-type equation
log_wage <- 1.5 +
  beta_educ * education_years +
  beta_exp * experience +
  beta_exp2 * (experience^2) +
  gender_effect +
  migrant_effect +
  parttime_effect +
  public_effect +
  east_effect +
  epsilon

# Convert to wage level
wage <- exp(log_wage)

synthetic_data <- data.frame(
  wage = wage,
  log_wage = log_wage,
  education_years = education_years,
  experience = experience,
  gender = gender,
  migrant_background = migrant_background,
  region = region,
  marital_status = marital_status,
  part_time = part_time,
  occupation = occupation,
  industry = industry,
  public_sector = public_sector,
  firm_size = firm_size
)

# Example Mincer regression
mincer_model <- lm(log_wage ~ education_years + experience + I(experience^2), data = synthetic_data)

print(summary(mincer_model))

# Save dataset
write.csv(synthetic_data, "synthetic_mincer_data.csv", row.names = FALSE)
