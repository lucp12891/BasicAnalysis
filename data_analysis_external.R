library(dplyr)

# Changing 'M' to 'Male' and 'F' to 'Female'
# - relabel 
levels(Demographix$Sex)[c('M', 'F')] <- c('Male', 'Female')

# One variable
Demographix <- Demographix %>%
  mutate(Sex = ifelse(Sex == "F", "Female", Sex))

# Multiple variable renaming
Demographix <- Demographix %>%
  mutate(Sex = recode(Sex, "F" = "Female", 
                      "M" = "Male"))


# * * * * * * * * * * * * * * * * * Summarizing data * * * * * * * * * * * * * * * * #

# Install the package (if not already installed - Remove # to install)
# install.packages("summarytools")  # 

# Alternative

# Load the package
library(summarytools)

# Create a summary table for a data frame (replace 'your_data' with your actual data)
df_summary <- dfSummary(Demographix, plain.ascii = FALSE)

# Print the summary table
df_summary
dfSummary(Demographix, 
          plain.ascii  = FALSE,
          style        = 'grid',
          graph.magnif = 0.85,
          varnumbers = FALSE,
          valid.col    = FALSE,
          tmp.img.dir  = "/tmp")

# Load the package
library(gtsummary)
library(tidyverse)


# With Customization
Demographix_tbl <-
  tbl_summary(
    Demographix,
    include = c(Sex, Age, `Duration on PrEP (Months)`, `Category (Months)`, `Relationship_Type`),
    by = Sex, # split table by group
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p(list(all_continuous() ~ "t.test", all_categorical() ~ "fisher.test")) %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()
Demographix_tbl

## Second Part

# read the worksheet from the XLSX file
Indicator2 <- read_excel("indicator_data.xlsx", sheet = "indicator")
Indicator2$Category = as.factor(Indicator2$Category)

#Plot

#Grouped
ggplot(Indicator2, aes(fill=Category, y=Value, x=Indicator2)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Indicator2") + ylab("Value") +
  theme_bw(base_size=10)

# Stacked
ggplot(Indicator2, aes(fill=Category, y=Value, x=Indicator2)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Indicator2") + ylab("Value") +
  theme_bw(base_size=10)

Indicator2$Categoryx = stringr::str_wrap(Indicator2$Category, width = 15)

#Ordered Stacked
# read the worksheet from the XLSX file
Indicator2 <- read_excel("indicator_data.xlsx", sheet = "indicator")
Indicator2$Category = as.factor(Indicator2$Category)
Indicator2$Category <- factor(Indicator2$Category,
                                     levels = c("Unreached","Reached"),
                                     labels = c("Unreached","Reached"))

Indicator2$Indicator2 <- factor(Indicator2$Indicator2,
                              levels = c("CLHIV","Diagnosed", "ART", "Viral_suppression"),
                              labels = c("CLHIV","Diagnosed", "ART", "Viral Suppression"))

#then I do my plot, using my factor variable as the fill value.
ggplot(Indicator2, aes(x=Indicator2, y=Value, fill=Category)) +
  geom_bar(stat="identity") + 
  xlab(" ") + ylab("") +
  scale_fill_manual(values=c("gold", "coral1"))+
  theme_minimal()

paed_cascade$Category <- factor(paed_cascade$Category,
                              levels = c("Unreached","Reached"),
                              labels = c("Unreached","Reached"))

paed_cascade$Indicator2 <- factor(paed_cascade$Indicator2,
                                levels = c("CLHIV","Diagnosed", "ART", "Viral_suppression"),
                                labels = c("CLHIV","Diagnosed", "ART", "Viral Suppression"))

#then I do my plot, using my factor variable as the fill value.
ggplot(paed_cascade, aes(x=Indicator2, y=Value, fill=Category)) +
  geom_bar(stat="identity") + 
  xlab(" ") + ylab("") +
  scale_fill_manual(values=c("gold", "coral1"))+
  scale_x_discrete(labels = function(Categoryx) str_wrap(Categoryx, width = 15)) +
  theme_minimal()
