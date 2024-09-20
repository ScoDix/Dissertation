##--- preparation ====
library(dplyr)
library(tidyr)

data_full <- read.csv("data.csv")

LSOAs <- read.csv("CNTW LSOAs.csv")
# LSOAs_full <- read.csv("LSOAs.csv")
# LSOAs_2022pop <- read.csv("mid2022pop.csv")[,3:5]
LSOAs_2019pop <- read.csv("mid2019pop.csv")[,c(1,2,7,8)]
matching_rows <- semi_join(LSOAs, LSOAs_2019pop, by = c("LSOA.Code"))

##--- verifying that the official LSOA data from the ONS is the same as the data on the spreadsheet ====
A <- LSOAs_full %>% select(LSOA.code..2011.,
                           LSOA.name..2011.,
                           Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.,
                           Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.)
B <- LSOAs

colnames(B) <- c("code","name","rank","decile")
colnames(A) <- c("code","name","rank","decile")

A <- A %>% select(-name)
B <- B %>% select(-name)

# Check if all rows in B are present in A
matching_rows <- semi_join(B, A, by = c("code", "rank", "decile"))
all_rows_match <- nrow(matching_rows) == nrow(B)

##--- getting the populations of each IMD decile in CNTW ====
LSOAs_2019pop <- semi_join(LSOAs_2019pop, LSOAs, by = "LSOA.Code")
LSOAs <- inner_join(LSOAs, LSOAs_2019pop, by = "LSOA.Code")

IMD_summary_A <- LSOAs %>% 
  group_by(Index.of.Multiple.Deprivation.Decile) %>% 
  summarise(population = sum(All.Ages))

IMD_summary_A_adult <- LSOAs %>%
  group_by(Index.of.Multiple.Deprivation.Decile) %>% 
  summarise(population = sum(Adult))

##--- remove irrelevant columns ====

data <- data_full %>% select(IMDr, IMDdec, sex, age = Age, ethnicity, accepted = accepted.,
                             accepted_code = for.coding, referral_added_to_RiO = Referral.Added.to.RiO,
                             allocated_on_RiO = Allocated.to.CaPE.HCP.on.RiO, first_contact_on_RiO = First.contact..appointment.recorded.on.RiO.,
                             appnt_1_date = Appnt.1.date, attended = Attended., research = Research, research_code = for.coding.1,
                             randomised_date = date.randomised, research2 = Research.Study2)

data <- data %>% mutate(IMDr = as.integer(IMDr), IMDdec = as.integer(IMDdec))

##--- Chi squared test of IMD for all referrals ====
# assume that populations in each decile are equal (a quick check of the entire country using 2019 IMD and 2015 population data says this is reasonably accurate)

# remove all observations with NA in IMD column
data_full_IMD <- data %>% 
  filter(!is.na(IMDdec))
# we are left with n = 308. This means expected values of 30.8 per decile, which satisfies the "large counts" condition of a chi-squared test

IMD_summary_B <- data_full_IMD %>%
  group_by(IMDdec) %>%
  summarize(decile_popn = n()) %>%
  mutate(expected_decile_popn_adult = IMD_summary_A_adult$population) %>% 
  mutate(expected_decile_popn = IMD_summary_A$population)

IMD_summary_C <- data_full_IMD %>% filter(accepted_code == "yes") %>% group_by(IMDdec) %>% summarize(decile_popn_accepted = n())
IMD_summary_C <- inner_join(IMD_summary_C, IMD_summary_B, by="IMDdec")

IMD_summary_D <- data_full_IMD %>% filter(attended == "Yes" | attended == "yes" | attended == "YES") %>% group_by(IMDdec) %>% summarize(decile_popn_attended = n())
IMD_summary_D <- inner_join(IMD_summary_D, IMD_summary_C, by="IMDdec")

IMD_summary_E <- data_full_IMD %>%
  filter(research_code == "randomised/eligible" | research == "SCiD randomised" | research2 == "SCiD randomised") %>%
  group_by(IMDdec) %>%
  summarize(decile_popn_research = n()) %>% 
  complete(IMDdec = 1:10, fill = list(decile_popn_research = 0))
IMD_summary_E <- inner_join(IMD_summary_E, IMD_summary_D, by = "IMDdec")

IMD_summary <- IMD_summary_E

##--- CHI-SQUARED TESTS FOR IMD ====
# even dist to B
chisq_test <- chisq.test(x = IMD_summary$decile_popn, p = rep(0.1, 10))
print(chisq_test)
# A+ to B
chisq_test <- chisq.test(x = IMD_summary$decile_popn, p = IMD_summary$expected_decile_popn / sum(IMD_summary$expected_decile_popn))
print(chisq_test)
# A- to B
chisq_test <- chisq.test(x = IMD_summary$decile_popn, p = IMD_summary$expected_decile_popn_adult / sum(IMD_summary$expected_decile_popn_adult))
print(chisq_test)
# A+ to C
chisq_test <- chisq.test(x = IMD_summary$decile_popn_accepted, p = IMD_summary$expected_decile_popn / sum(IMD_summary$expected_decile_popn))
print(chisq_test)
# A- to C
chisq_test <- chisq.test(x = IMD_summary$decile_popn_accepted, p = IMD_summary$expected_decile_popn_adult / sum(IMD_summary$expected_decile_popn_adult))
print(chisq_test)
# A+ to D
chisq_test <- chisq.test(x = IMD_summary$decile_popn_attended, p = IMD_summary$expected_decile_popn / sum(IMD_summary$expected_decile_popn))
print(chisq_test)
# A- to D
chisq_test <- chisq.test(x = IMD_summary$decile_popn_attended, p = IMD_summary$expected_decile_popn_adult / sum(IMD_summary$expected_decile_popn_adult))
print(chisq_test)
# B to C
chisq_test <- chisq.test(x = IMD_summary$decile_popn_accepted, p = IMD_summary$decile_popn / sum(IMD_summary$decile_popn))
print(chisq_test)
# B to D
chisq_test <- chisq.test(x = IMD_summary$decile_popn_attended, p = IMD_summary$decile_popn / sum(IMD_summary$decile_popn))
print(chisq_test)
# C to D
chisq_test <- chisq.test(x = IMD_summary$decile_popn_attended, p = IMD_summary$decile_popn_accepted / sum(IMD_summary$decile_popn_accepted))
print(chisq_test)
# Fisher tests for X to E
# scaling the table to the size of the research population
IMD_summary_scaled <- cbind(IMD_summary[,1:2],29*IMD_summary[,3]/sum(IMD_summary[,3]),29*IMD_summary[,4]/sum(IMD_summary[,4]),29*IMD_summary[,5]/sum(IMD_summary[,5]),29*IMD_summary[,6]/sum(IMD_summary[,6]),29*IMD_summary[,7]/sum(IMD_summary[,7]))
# the tests
# A- to E
fisher_test <- fisher.test(IMD_summary_scaled[,c(2,6)], workspace = 2e8)
print(fisher_test)
# B to E
fisher_test <- fisher.test(IMD_summary_scaled[,c(2,5)], workspace = 2e8)
print(fisher_test)
# C to E
fisher_test <- fisher.test(IMD_summary_scaled[,c(2,4)], workspace = 2e8)
print(fisher_test)
# D to E
fisher_test <- fisher.test(IMD_summary_scaled[,c(2,3)], workspace = 2e8)
print(fisher_test)


##--- Ethnicity Chi squareds ====

data_full_eth <- data %>% filter(ethnicity != "1" & ethnicity != "RNA'd" & ethnicity != "UTC" & ethnicity != "NYA" & ethnicity != "Not known/not stated")
data_full_eth <- data_full_eth %>%
  mutate(ethnicity_group = case_when(
    ethnicity %in% c("White - English/Welsh/Scottish/Northern Irish/British Irish",
                     "White - Any other white background",
                     "White") ~ "White",
    ethnicity %in% c("Mixed - any other mixed background",
                "Asian or Asian British -Any other Asian background") ~ "Non-white",
    TRUE ~ "N/A"
  )) %>% 
  relocate(ethnicity_group, .before = ethnicity)

eth_summary_B <- data_full_eth %>% group_by(ethnicity_group) %>% summarize(B = n())
eth_summary_B <- eth_summary_B %>%
  mutate(A = c(9.150780716,90.84921928)) %>% # these values come from an excel spreadsheet (ethnicity population.xlsx)
  relocate(A, .before = B)

eth_summary_C <- data_full_eth %>% filter(accepted_code == "yes") %>% group_by(ethnicity_group) %>% summarize(C = n())
eth_summary_C <- inner_join(eth_summary_B, eth_summary_C, by="ethnicity_group")

eth_summary_D <- data_full_eth %>% filter(attended == "Yes" | attended == "yes" | attended == "YES") %>% group_by(ethnicity_group) %>% summarize(D = n())
eth_summary_D <- inner_join(eth_summary_C, eth_summary_D, by = "ethnicity_group")

eth_summary_E <- data_full_eth %>% filter(research_code == "randomised/eligible" | research == "SCiD randomised" | research2 == "SCiD randomised") %>% group_by(ethnicity_group) %>% summarize(E = n())
eth_summary_E <- inner_join(eth_summary_D, eth_summary_E, by = "ethnicity_group")

eth_summary <- eth_summary_E

# scaling before fisher test
eth_summary_scaledB <- eth_summary %>% mutate(A = A*sum(B)/sum(A))
eth_summary_scaledC <- eth_summary %>% mutate(A = A*sum(C)/sum(A),
                                              B = B*sum(C)/sum(B))
eth_summary_scaledD <- eth_summary %>% mutate(A = A*sum(D)/sum(A),
                                              B = B*sum(D)/sum(B),
                                              C = C*sum(D)/sum(C))
eth_summary_scaledE <- eth_summary %>% mutate(A = A*sum(E)/sum(A),
                                              B = B*sum(E)/sum(B),
                                              C = C*sum(E)/sum(C),
                                              D = D*sum(E)/sum(D))

#AB
fisher_test <- fisher.test(eth_summary_scaledB[,c(2,3)], workspace = 2e8)
print(fisher_test)
#AC
fisher_test <- fisher.test(eth_summary_scaledC[,c(2,4)], workspace = 2e8)
print(fisher_test)
#BC
fisher_test <- fisher.test(eth_summary_scaledC[,c(3,4)], workspace = 2e8)
print(fisher_test)
#AD
fisher_test <- fisher.test(eth_summary_scaledD[,c(2,5)], workspace = 2e8)
print(fisher_test)
#BD
fisher_test <- fisher.test(eth_summary_scaledD[,c(3,5)], workspace = 2e8)
print(fisher_test)
#CD
fisher_test <- fisher.test(eth_summary_scaledD[,c(4,5)], workspace = 2e8)
print(fisher_test)
#AE
fisher_test <- fisher.test(eth_summary_scaledE[,c(2,6)], workspace = 2e8)
print(fisher_test)
#BE
fisher_test <- fisher.test(eth_summary_scaledE[,c(3,6)], workspace = 2e8)
print(fisher_test)
#CE
fisher_test <- fisher.test(eth_summary_scaledE[,c(4,6)], workspace = 2e8)
print(fisher_test)
#DE
fisher_test <- fisher.test(eth_summary_scaledE[,c(5,6)], workspace = 2e8)
print(fisher_test)

##--- Chi squared test of sex for all referrals ====
# assume 51% female and 49% male in total population

# remove all observations with NA in sex
data_full_sex <- data %>% 
  filter(sex != "")
# n = 339, so we have large enough expected values for the test to be valid

sex_summary <- data_full_sex %>%
  group_by(sex) %>%
  summarize(sex_popn = n()) %>%
  mutate(expected_sex_prop = case_when(
    sex == "M" ~ 0.49,
    sex == "F" ~ 0.51
  ))

# the test
chisq_test <- chisq.test(x = sex_summary$sex_popn, p = sex_summary$expected_sex_prop)
print(chisq_test)

##--- Chi squared test of age for all referrals ====
# take the population curve from

##--- exploring ====
library(ggplot2)

age_plot <- ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 4, fill = "grey", color = "black", na.rm = TRUE) +
  labs(title = "Age Distribution of Patients", x = "Age", y = "Frequency") +
  theme_minimal(base_size = 14) + # Use a base size for academic readability
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),  # Center title, serif font
    axis.title.x = element_text(margin = margin(t = 10), family = "serif"),  # X-axis label, serif font
    axis.title.y = element_text(margin = margin(r = 10), family = "serif"),  # Y-axis label, serif font
    text = element_text(family = "serif")  # Apply serif font globally
  )
ggsave("age_plot.png", plot = age_plot, width = 170, height = 85, units = "mm")

IMDdec_plot <- ggplot(data %>% drop_na(IMDdec), aes(x = as.factor(IMDdec))) +
  geom_bar(fill = "grey", color = "black", na.rm = TRUE) +
  labs(title = "IMD Deciles of Patients", x = "IMD decile, where 1 is the most deprived", y = "Number of patients") +
  theme_minimal(base_size = 14) + # Use a base size for academic readability
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),  # Center title, serif font
    axis.title.x = element_text(margin = margin(t = 10), family = "serif"),  # X-axis label, serif font
    axis.title.y = element_text(margin = margin(r = 10), family = "serif"),  # Y-axis label, serif font
    text = element_text(family = "serif")  # Apply serif font globally
  )
ggsave("IMDdec_plot.png", plot = IMDdec_plot, width = 170, height = 85, units = "mm")

eth_plot <- ggplot(data = data.frame(ethnicity = factor(c("White British", "Not known/not stated", "White - Other", "Mixed", "Asian"), levels = c("White British", "Not known/not stated", "White - Other", "Mixed", "Asian")),
  count = c(132, 21, 13, 3, 1)), aes(x = ethnicity, y = count)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, family = "serif") +
  labs(title = "Distribution of patient ethnicity", x = "Ethnicity", y = "Number of patients") +
  theme_minimal(base_size = 14) + # Use a base size for academic readability
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),  # Center title, serif font
    axis.title.x = element_text(margin = margin(t = 10), family = "serif"),  # X-axis label, serif font
    axis.title.y = element_text(margin = margin(r = 10), family = "serif"),  # Y-axis label, serif font
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, family = "serif"),  # Angle x-axis labels
    text = element_text(family = "serif")  # Apply serif font globally
  )
eth_plot
ggsave("eth_plot.png", plot = eth_plot, width = 170, height = 115, units = "mm")

NE_IMD_plot <- ggplot(IMD_summary_A_adult, aes(x = as.factor(Index.of.Multiple.Deprivation.Decile), y = population)) +
  geom_bar(stat = "identity", fill = "grey", color = "black", na.rm = TRUE) +
  labs(title = "IMD Deciles of the adult population of Northumberland, Cumbria and Tyne and Wear", x = "IMD decile, where 1 is the most deprived", y = "Number of people") +
  theme_minimal(base_size = 14) + # Use a base size for academic readability
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),  # Center title, serif font
    axis.title.x = element_text(margin = margin(t = 10), family = "serif"),  # X-axis label, serif font
    axis.title.y = element_text(margin = margin(r = 10), family = "serif"),  # Y-axis label, serif font
    text = element_text(family = "serif")  # Apply serif font globally
  )
NE_IMD_plot

##--- LOGISTIC REGRESSION!!! ====
#install.packages(c("naniar", "mice", "VIM", "car"))
library(naniar)
library(mice)
library(VIM)
library(car)

data_clean <- read.csv("data clean.csv")
data_clean <- data_clean %>% select(-PAX.ASCEnD.,-X.1,-X.2,-X.3)
data_clean <- data_clean %>% mutate(across(-c(date.randomised), ~ na_if(as.character(.x), "")))

#vis_miss(data_clean)
#md.pattern(data_clean)
#aggr_plot <- aggr(data_clean, col=c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, 
 #                 labels=names(data_clean), cex.axis=.7, gap=3, 
 #                 ylab=c("Missing data","Pattern"))

#mcar_test_result <- mcar_test(data_clean)
#print(mcar_test_result)

data_clean <- data_clean %>% mutate(CaPE.Patient.Number = as.integer(CaPE.Patient.Number),
                        IMDr = as.integer(IMDr),
                        IMDdec = as.integer(IMDdec),
                        Age = as.integer(Age),
                        X = as.logical(X),
                        referral_date = as.Date(Referral.Date.Received, format = "%d/%m/%Y"),
                        Referral.Date.Received = as.numeric(as.Date(Referral.Date.Received, format = "%d/%m/%Y") - as.Date("04/02/2022", format = "%d/%m/%Y")),
                        allocated.to.medic.code..1yes.2no. = as.integer(allocated.to.medic.code..1yes.2no.))

data_clean <- data_clean %>%
  mutate(across(where(is.character), ~ replace_na(., "missing")))

data_clean <- data_clean %>% mutate(research_participant = ifelse(data_clean$for.coding.1 == "randomised/eligible" |
                                                                    data_clean$Research == "SCiD randomised"|
                                                                    data_clean$Research.Study2 == "SCiD randomised", 1, 0))

data_log <- data_clean %>% select(IMDr,
                                  return_visit = Return.Visit,
                                  sex,
                                  age = Age,
                                  referral_source = Source.of.Referral,
                                  PCN = PCN,
                                  CTT = Relevant.CTT,
                                  date_referral = Referral.Date.Received,
                                  ethnicity,
                                  accepted_code = for.coding,
                                  referral_added_to_RiO = Referral.Added.to.RiO,
                                  referral_reason = Referral.Reason..main.motivation,
                                  allocated_to_medic = Allocated.to.Medic,
                                  QIDS_SR = X.first..QIDS.SR,
                                  questionnaires_sent = Appointment.letter.questionnaires.sent,
                                  first_contact_on_RiO = First.contact..appointment.recorded.on.RiO.,
                                  questionnaires_received = Completed.Q.aires.received,
                                  appnt_location = Location.details,
                                  RiO_consent = RiO..Consent.Status.recorded,
                                  RiO_risk = RiO..Narrative.Risk.Completed,
                                  RiO_core = RiO..Core.Docs.Completed,
                                  RiO_progress = RiO...Progress.Notes.Completed,
                                  RiO_full = RiO.full.set.,
                                  letter_to_Pam = Letter.sent.to.Pam,
                                  treatment = Psychological.treatment.recommended,
                                  neurodevelop = Neurodevelop.,
                                  attended = Attended.,
                                  research = Research,
                                  research_code = for.coding.1,
                                  research2 = Research.Study2,
                                  assessment_letter_to_GP = Assessment.Letter.sent.to.GP.Pt.,
                                  discharge = Discharged.to.,
                                  research_participant)

data_log <- data_log %>%
  mutate(ethnicity = ifelse(ethnicity %in% c("White", "White - Any other white background", "White - English/Welsh/Scottish/Northern Irish/British Irish"), "White", ethnicity)) %>% 
  mutate(ethnicity = ifelse(ethnicity %in% c("missing", "Not known/not stated"), "Not_known", ethnicity)) %>% 
  mutate(ethnicity = ifelse(ethnicity == "Asian or Asian British -Any other Asian background", "Asian", ethnicity)) %>% 
  mutate(ethnicity = ifelse(ethnicity == "Mixed - any other mixed background", "Mixed", ethnicity)) %>% 
  mutate(referral_source_agg = case_when(
    referral_source == "Primary" ~ "Primary",
    referral_source %in% c("Secondary", "Tertiary") ~ "Secondary/Tertiary",
    referral_source %in% c("Interface", "Lindus", "PIC") ~ "External",
    referral_source == "R+Me" ~ "Research",
    referral_source == "Self" ~ "Self"
  ))

data_log <- data_log %>% 
  mutate(from_primary = ifelse(referral_source_agg == "Primary", 1, 0)) %>% 
  mutate(from_secondary = ifelse(referral_source == "Secondary", 1, 0)) %>% 
  mutate(from_sec_or_tert = ifelse(referral_source_agg == "Secondary/Tertiary", 1, 0))

model <- glm(data = data_log, research_participant ~
              #IMDr + 
              #return_visit +
              #sex +
               age +
              #referral_source+
              #referral_source_agg+
               from_primary,
              #from_secondary+
               #from_sec_or_tert+
              #PCN +
              #date_referral,
              #ethnicity+
               #accepted_code +
              #referral_reason,
               #allocated_to_medic +
               #QIDS_SR +
               #questionnaires_received +
               #appnt_location+
               #RiO_full +
               #letter_to_Pam,
               #treatment +
               #attended +
               #discharge,
               family = binomial)
summary(model)
prob_pred <- predict(model, newdata = data_log, type = "response")
pred <- ifelse(prob_pred > 0.5, 1, 0)
#library(caret)
confusionMatrix(factor(pred), factor(data_log$research_participant))










print(AIC(model))
print(BIC(model_date))

vif(model_date)

summary(model_date)

##--- more graphs ====
library(lubridate)

data_clean$year_month <- floor_date(data_clean$referral_date, "month")

monthly_data <- data_clean %>%
  group_by(year_month) %>%
  summarize(participants = sum(research_participant))

month_plot <- ggplot(monthly_data, aes(x = year_month, y = participants)) +
  geom_col(fill = "grey", color = "black") +
  labs(title = "Research Participants by Month",
       x = "",
       y = "Number of Research Participants") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme_minimal(base_size = 14) + # Use a base size for academic readability
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),  # Center title, serif font
    axis.title.x = element_text(margin = margin(t = 10), family = "serif"),  # X-axis label, serif font
    axis.title.y = element_text(margin = margin(r = 10), family = "serif"),  # Y-axis label, serif font
    text = element_text(family = "serif"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = -45, hjust = 0)
  )
ggsave("month_plot.png", plot = month_plot, width = 170, height = 115, units = "mm")
