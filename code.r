# Load necessary libraries
library(pcr)
library(dplyr)
library(ggplot2)
library(readxl)
	 
# Import data from Excel file – format: genes in columns, Ct values listed in rows with HepG2 values listed first followed by CAV1+ values
data <- read_excel("C:/Users/ganob/OneDrive - Coventry University/MSc Molecular Biology/7004BMS Research Techniques in Molecular Biology/Assessment/qpcr_data_hepg2_cav1.xlsx")
	 
# Define groups and number of repeats
groups <- rep(c("HepG2", "CAV1+"), each = 2)

# Analyze data using default method (delta_delta_ct)
analyzed_data <- pcr_analyze(data, 
                                 group_var = groups,
                                 reference_gene = "GAPDH",
                                 reference_group = "HepG2") 

# Perform statistical testing using student's t-test
t_test <- pcr_test(data, 
                       group_var = groups, 
                       reference_gene = "GAPDH", 
                       reference_group = "HepG2", 
                       test = "t.test")

# Generate plot for CAV1 expression changes
cav1_plot <- analyzed_data %>% filter(gene == "CAV1") %>% 
  ggplot(aes(x = group, y = relative_expression)) + 
  geom_bar(stat = "identity", color = "black", fill = "green") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                    position = "dodge") + 
  xlab("") + 
  ylab("Relative expression") + 
  theme(axis.text = element_text(face = "bold", color = "black"),
              axis.title = element_text(face = "bold", color = "grey50"),
              legend.position = "none") +
  annotate("text", x = "CAV1+", y = 80, label = "p = 0.01670")
 
# Generate plots for E-cadherin and vimentin changes
ecad_vim_plot <- analyzed_data %>% 
  filter(gene == "ECad" | gene == "VIM" | gene == "GAPDH") %>%
  ggplot(aes(x = group, y = relative_expression, fill = gene)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                    position = "dodge")+
  facet_wrap(.~gene, scales = "free") +
  xlab("") +
  ylab("Relative expression") +
  theme(axis.text = element_text(face = "bold", color = "black"),
              axis.title = element_text(face = "bold", color = "grey50"),
              legend.position = "none")
