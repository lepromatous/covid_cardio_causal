# ======================================================================== #
# CARDIOVASCULAR OUTCOMES OF COVID_19 USING CAUSAL MACHINE LEARNING ----
# ======================================================================== #

# ======================================================================== #
# LOAD PACKAGES ----
# ======================================================================== #
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx26g"))
gc()

library(tidyverse)
library(rJava)
library(RJDBC)
library(data.table)
library(dbplyr)
library(DBI)
library(tidycensus)
library(grf)

# ======================================================================== #
# Create Connection ----
# ======================================================================== #

con <- source("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/zz useful R code/pfe_con.R")
con <- con[[1]]  ## for some reason sourcing gives a list so need the first one for use

## list all schema and table ----
View(tab[tab$TABLE_SCHEM%like%"team_covid",])  ### look at just covid

# ======================================================================== #
# Pull Data ----
# ======================================================================== #

### Investigate names ----
con %>%
  dplyr::tbl(in_schema("team_covid", "vax_pasc_cva_ge65")) %>%
  colnames(.) 

### Look at snippet ---
### State abbreviation is in patient_state
con %>%
  dplyr::tbl(in_schema("team_covid", "vax_pasc_cva_ge65")) %>%
  dplyr::collect() -> df

## Pull in census data to look for instrument ----
tidycensus::get_acs(geography="state",
                    variables = c("B17010_001E","B17010_002E")) %>%
  select(-c(moe)) %>%
  pivot_wider(., id_cols = c("GEOID", "NAME"), names_from = "variable", values_from = "estimate") %>%
  mutate(
    state.abb[match(NAME,state.name)]
  ) %>%
  rename(
    patient_state = 5
  ) %>%
  left_join(
    df, ., by="patient_state"
    ) %>%
  mutate(
    prop.poverty = B17010_002 / B17010_001 *100
    ) -> df

df$any.cardio <- ifelse(rowSums(df[,103:109])>=1,1,0)


# Check base instrument ----
out <- glm(any.cardio ~ prop.poverty, data = df, family="binomial")
summary(out)
out2 <- glm(any.cardio ~ covid_type, data = df, family="binomial")
summary(out2)

# Make covariates set ----
df$gender <- ifelse(df$gender=="M",1,0)
X = df[,c(8, 10:17, 20:31, 43:81, 85, 89:109)]

df2 <- df[complete.cases(df[,c(names(X), "any.cardio", "icu", "prop.poverty")]), c(names(X), "any.cardio", "icu", "prop.poverty")]
X2 <- df2[,names(X)]
# ==========================================================================#
# Causal Forest
# ==========================================================================#
set.seed(123467)
model <- instrumental_forest(
  Y=df2$any.cardio,
  X=X2,
  W=df2$icu,
  Z=df2$prop.poverty,
  num.trees = 50000
)
