################################################################################
#
# This syntax belongs to:
# Every Primary School Needs a Special Educator
#
# Further contact: Dr. Nikola Ebenbeck (she/her) - nikola.ebenbeck@lmu.de
#
################################################################################

# 0. Setting up ----------------------------------------------------------------

# Packages
packages <- c("tidyverse", "patchwork", "geosphere", "sf", "ggspatial", "khroma")
lapply(packages, library, character.only = T)

# Environment
setwd("H:/Daten/Bayern Daten/Neue Tabellen/Modelle")

# 1. Loading and Reshaping Data ------------------------------------------------

# Function for loading and reshaping
function1 <- function(path) {read.csv(path) %>%
    # Select necessary variables
    select(Schulnummer,RS_Kreis,Schulart,Schultyp,Strasse,PLZ,Ort,lat,lon,
           RS_Gemeinde,KTYP4,SN_KTYP4,n_students_school,n_students_SEN_primary,
           n_students_PS_add,prop_students_PS_add,n_students_PS_new, 
           prop_support_rate_new) %>%
    # Rename columns
    rename(n_students_old = n_students_school,
           n_students_SEN_old = n_students_SEN_primary,
           n_students_SEN_add = n_students_PS_add,
           n_students_new = n_students_PS_new) %>%
    # Generate new column: School size
    mutate(n_students_SEN_new = n_students_SEN_old + n_students_SEN_add)}

# Current Model
data <- read.csv("model_0.csv") 

# Model 1 without Special Schools
data_noSS <- function1("model_1a.csv")

# Model 2 with some Special Schools
data_someSS <- function1("model_2a.csv")

# Geodata of districts
district <- st_read("Z:/2. Forschung/Daten und Codes/Schulstatistiken/Bayerndaten 2010 - 2020 alle Schulen/Geodaten/Verwaltungseinheiten Deutschland/vg250_ebenen_0101/VG250_KRS.shp") %>%
  mutate(RS_Kreis = as.numeric(ARS)) %>% select(RS_Kreis, GEN, geometry)

# Geodata of Bavaria
bavaria <- st_read("Z:/2. Forschung/Daten und Codes/Schulstatistiken/Bayerndaten 2010 - 2020 alle Schulen/Geodaten/Verwaltungseinheiten Deutschland/Bundesländer_Ohne_Wasser.shp")

# 2. Calculation of Special Education Teacher Hours ----------------------------

# Percentage of subject teachers + religion teachers on FT and HT
9485 # all teachers
5594 # all FT teachers
3793 # all HT teachers
390  # subject teachers in FT
278  # subject teachers in HT
262  # religion teachers in HT

390/5574 # 7 % of FT teachers are subject teachers
(278+262)/3783 # 14.3 % of HT teachers are subject or religion teachers

# Full time hours + half time hours + short time hours + "Referendariat"
110994 +  # Lesson hours FT
  26331 + # Advisory hours FT
  10969 + # Mobile hours FT
59103 +   # Lesson hours HT
  10994 + # Advisory hours HT
  6962 +  # Mobile hours HT
11326 +   # Lesson hours ST
  3537 +  # Advisory hours ST
  2289 +  # Mobile hours ST
8461      # Referendariat hours
# 148294 FT teacher hours, 77059 HT teacher hours, 17152 ST teacher hours   
# --> 250966 teacher hours in total

# Subtracting subject and religion teacher hours
250966 - (148294 * 0.07) - (77059 * 0.143) 
# --> 229566 resulting teacher hours
# Substracting 5 % of teacher hours for hospital schools or other reasons
229566 * 0.95
# --> 218087.7 resulting teacher hours

table(data_original$Schultyp)
  (30 +          # Behavior Disorder
    10 +         # Learning
    164 +        # SFZ
    7) / 350     # Speaking
table(data_original$Schulart) # --> 350 Special Schools in Current System
## --> 40% of Teachers work in a special school which stays in Model 2

# 3. Data Description ----------------------------------------------------------

# Number of districts in urban / rural areas
data %>% select(RS_Kreis,SN_KTYP4) %>% unique() %>% pull(SN_KTYP4) %>% table()

# Number of School types in urban / rural areas
unique(select(data,Schulnummer,Schulart,SN_KTYP4)) %>% count(Schulart,SN_KTYP4)

# Number of total students
data %>% select(n_students_school, Schulart,SN_KTYP4) %>% unique() %>% 
  summarise(total_students = sum(n_students_school, na.rm = T))

# 4. Simulating special educator distribution ----------------------------------

# Calculation of proportion factors
## Model 1 (50% teacher hours / all students with SEN)
propfac_1 <- (218087.7/2) / sum(data_noSS$n_students_SEN_new, na.rm=T)
## Model 2 (60% of model 1 teacher hours / all students with SEN)
propfac_2 <- (218087.7/2)*0.6 / sum(data_someSS$n_students_SEN_new, na.rm=T)

# Function for Distribution of special educator hours
function2 <- function(d, propfac, sim_number) {
  result <- d %>%
  mutate(teacher_hours = n_students_SEN_new*propfac) %>%
  mutate(n_speced_teachers = teacher_hours/26) %>%
  group_by(RS_Kreis) %>%
  mutate(teacher_hours_Kreis = sum(teacher_hours, na.rm=T)) %>%
  mutate(teachers_Kreis = teacher_hours_Kreis/26) %>%
  mutate(SN_KTYP4 = as.factor(SN_KTYP4)) %>%
  mutate(SN_KTYP4_ENG = case_when(
    SN_KTYP4==1 ~ "large cities", SN_KTYP4==2 ~ "urban districts",
    SN_KTYP4==3 ~ "rural districts", SN_KTYP4==4 ~ "sparsley\npopulated")) %>%
  mutate(SN_KTYP4_ENG = factor(SN_KTYP4_ENG,
    levels=c("large cities", "urban districts", 
             "rural districts", "sparsley\npopulated"))) %>%
  mutate(sim = sim_number) %>% ungroup()
  return(result)}

# Distribution Model 1
model1 <- function2(data_noSS, propfac_1, 1)

# Distribution Model 1
model2 <- function2(data_someSS, propfac_2, 2)

# 5. Matching Schools ----------------------------------------------------------

# Greedy Algorithm per District [With some coding help from ChatGPT 4.0]
function3 <- function(df, model = NULL) {
  # New empty list to save school pairs
  pairs <- list()
  # Loop: Do it for every district!
  for (kreis in unique(df$RS_Kreis)) {
    # Filter the data
    d_kreis <- df %>% filter(RS_Kreis == kreis)
    # Create the coordinates matrix for distance calculation
    coords <- cbind(d_kreis$lon, d_kreis$lat)
    # Compute the distance matrix
    dist_matrix <- distm(coords)
    # Function to calculate the teacher hours difference
    teacher_diff <- function(i, j) {
      abs(d_kreis$teacher_hours[i] - d_kreis$teacher_hours[j])
    }
    # Number of schools, which need matching
    remaining_schools <- 1:nrow(d_kreis)
    # Do this, as long as more than 1 unmatched school remains
    while (length(remaining_schools) > 1) {
      # Create empty variables to save information
      best_pair <- NULL # Pair of school numbers
      best_distance <- Inf # Minimal distance
      best_teacher_diff <- -Inf # Largest teacher hour diff
      best_pair_index <- c() # Indices of best school pair
      # Select the first school to match
      for (i in 1:(length(remaining_schools) - 1)) {
        # Select second school, which matches the first school
        for (j in (i + 1):length(remaining_schools)) {
          # List the two schools to compare
          school_i <- remaining_schools[i]
          school_j <- remaining_schools[j]
          # Calculate the distance between the schools
          distance <- dist_matrix[school_i, school_j]
          # Calculate the teacher hour diff between the schools
          teacher_diff_value <- teacher_diff(school_i, school_j)
          # If distance and teacher diff are better then other options,
          # choose this pair as matching pair
          if (distance < best_distance || (
            distance==best_distance&&teacher_diff_value>best_teacher_diff)) {
            # Save variables of matching pair
            best_pair <- c(school_i, school_j)
            best_distance <- distance
            best_teacher_diff <- teacher_diff_value
            best_pair_index <- c(i, j)
          }
        }
      }
      # Add the best pair to the list
      pairs <- c(pairs, list(c(d_kreis$Schulnummer[best_pair[1]], 
                               d_kreis$Schulnummer[best_pair[2]])))
      # Remove the selected pair from the remaining schools
      remaining_schools <- remaining_schools[-c(best_pair_index)]
    }
  }
  # Create a DataFrame from the list of pairs for a better overview
  paired_schools <- do.call(rbind, pairs) %>%
    # List to dataframe
    as.data.frame() %>%
    # Rename columns
    rename(Schule_A = V1, Schule_B = V2) %>%
    # New column with pair-ID
    mutate(Group = paste0(pmin(Schule_A, Schule_B), "_", 
                          pmax(Schule_A, Schule_B))) %>%
    # Restructure df: Every row is a school
    pivot_longer(c(Schule_A, Schule_B), values_to = "Schulnummer") %>%
    # Remove old variable
    select(-name)
  
  # Join the pairs and match left schools to nearest pair
  if (!is.null(model)) {
    model_matched <- model %>% 
      # Join pairs and remove duplicates
      right_join(paired_schools) %>% unique() %>%
      # Sort distance between schools in a district
      arrange(RS_Kreis, lon, lat) %>% group_by(RS_Kreis) %>%
      # Manually match left schools to nearest district
      fill(Group, .direction = "up") %>% fill(Group, .direction = "down") %>%
      ungroup() %>% group_by(Group) %>%
      # Create new columns
      mutate(dist_km = mean(
        distm(matrix(c(lon, lat), ncol = 2)) / 1000, na.rm = T)) %>%
      mutate(dist_km = na_if(dist_km, 0)) %>%
      mutate(sum_hours = sum(teacher_hours)) %>%
      mutate(sum_teachers = sum_hours / 26)
    return(model_matched)
  }
  return(paired_schools)
}

# Match schools without full-time special educators per district
x1 <- model1 %>% filter(teacher_hours < 26) %>% function3(model=model1)
x2 <- model2 %>% filter(teacher_hours < 26) %>% function3(model=model2)

# Combine all information into one df and calculate new variables
models <- rbind(x1, x2) %>% ungroup() %>% 
  full_join(model1) %>% full_join(model2) %>%
  group_by(RS_Kreis, sim) %>%
  # Proportion of matched schools / all schools in district
  mutate(prop_matched = (sum(is.na(Group)))/n()*100) %>%
  # Fot plots
  mutate(system = case_when(sim==1 ~ "Model 1", sim==2 ~ "Model 2")) %>%
  mutate(sum_hours = case_when(sum_hours > 5000 ~ 5000, TRUE ~ sum_hours)) %>%
  ungroup()

rm(x1, x2) # Remove unused dfs

# 6. Plots ---------------------------------------------------------------------

# Figure 1
## Plot - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
plot1 <- rbind(data.frame(
  Schulnummer = model1$Schulnummer, sim = 0,
  n_students_SEN_new = model1$n_students_SEN_old,
  SN_KTYP4_ENG = model1$SN_KTYP4_ENG), 
  model1 %>% select(Schulnummer,sim,n_students_SEN_new,SN_KTYP4_ENG), 
  model2 %>% select(Schulnummer,sim,n_students_SEN_new,SN_KTYP4_ENG)) %>%
  rename(n_students_SEN = n_students_SEN_new) %>% na.omit() %>%
  mutate(system = case_when(
    sim==0 ~ "current system", sim==1 ~ "model 1", sim==2 ~ "model 2")) %>%
  # base boxplot
  ggplot(aes(y=n_students_SEN, x=SN_KTYP4_ENG, fill=system)) +
  geom_boxplot()+ theme_bw() + 
  # legend and margins
  theme(legend.position = "top", legend.justification = "left",
        legend.margin = margin(5,0,0,0),
        legend.box.margin = margin(-10,-10,-10,0),
        axis.title.x = element_text(margin=margin(t=-5))) +
  # labels and text
  ylab("n students with SEN") +
  xlab("district categories") + 
  labs(subtitle=bold("Model comparison:")~
         "Number of students with SEN at primary schools") +
  scale_y_continuous("n students with SEN", breaks=seq(0, 200, 20)) +
  # colors
  scale_fill_manual(values=c("white", "grey75", "grey45"))
plot1

## Saving figure - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ggsave("H:/Daten/Bayern Daten/ZFE Artikel/Plot1.png", 
       plot=plot1, width=20, heigh=10, units=c("cm"), dpi=1200)

# Figure 2

## Figure 2a - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- models %>% left_join(district) %>% st_as_sf()
y <- models %>% filter(Schulart == "Foerderzentren" & Schultyp != "Kranke") %>%
  select(system, lat, lon) %>% st_as_sf(coords = c("lon", "lat"), crs=4326)

plot2a <- ggplot() + 
  # Districts filled with teacher hours
  geom_sf(data=x, aes(geometry=geometry, fill=teacher_hours_Kreis)) +
  # Points of Schools
  geom_sf(data=y, aes(geometry=geometry, color="")) +
  # legend, margins and style
  labs(fill = "special educator hours", color="special school locations",
       subtitle=bold("A:")~
         "Spatial districution of special educator hours and special schools") + 
  annotation_scale(location="br") +
  theme(legend.position = "bottom", legend.justification = "left",
        legend.key.width=unit(1, "cm")) +
  facet_grid(cols = vars(system)) +
  theme_bw() +
  # labels and text
  guides(
    fill = guide_colorbar(title.position = "top", order=1, title.hjust = 0.5),
    color = guide_legend(title.position = "top", order=2, title.hjust = 0.5)) +
  # colors
  scale_color_manual(values="black") +
  scale_fill_distiller(
    type="seq",direction=1,palette="Greys",limits=c(100,5000),
    breaks=c(1000, 2000, 3000, 4000, 5000),
    labels=c("1000", "2000", "3000", "4000", "> 5000"))

## Figure 2b - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
plot2b <- models %>%
  ggplot(aes(y=teacher_hours, x=SN_KTYP4_ENG, fill=system)) +
  geom_boxplot()+ theme_bw() + 
  # legend, margins and style
  theme(legend.position = "bottom", legend.justification = "left",
        legend.margin = margin(5,0,0,0),
        legend.box.margin = margin(-10,-10,-10,0),
        axis.title.x = element_text(margin=margin(t=-5)),
        plot.title = element_text(size=12, face="bold")) +
  # labels and text
  ylab("n special educator hours") + xlab("district categories") + 
  labs(subtitle=bold("B:")~
         "Special educator hours and full positions at primary schools") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  # second y-axis
  scale_y_continuous("N special educator hours", breaks = seq(0, 350, 50),
                     sec.axis=sec_axis(~./26, name="N full special educators",
                                       breaks = seq(0, 15, 1))) +
  # colors & lines
  scale_fill_manual(values=c("grey80", "grey45")) +
  geom_hline(yintercept = 26, linetype = "dashed", color = "black")

## Combining and saving figures - - - - - - - - - - - - - - - - - - - - - - - - 
plot2 <- plot2a/guide_area()/plot2b + 
  plot_layout(heights=c(1, 0.2, 1), guides="collect") &
  theme(legend.box="horizontal")

ggsave("H:/Daten/Bayern Daten/ZFE Artikel/Plot2.png", 
       plot=plot2, width=20, heigh=24, units=c("cm"), dpi=1200)

# Figure 3

## Preparing variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
models_sf <- models %>% # Spatial Points
  st_as_sf(coords = c("lon", "lat"), crs=4326) 
models_lines <- models_sf %>% group_by(Group) %>% # Spatial Lines
  mutate(geometry = st_union(geometry) %>% st_cast("LINESTRING"))

## Munich: Model 1 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
plot3a <- ggplot() +
  # Map of Munich City
  geom_sf(data = district %>% filter(RS_Kreis=="9162") %>% select(geometry), 
          fill = "grey95", color = "black") +
  # Points of Schools
  geom_sf(data=models_sf %>% filter(RS_Kreis == "9162") %>% filter(sim==1) %>%
            select(teacher_hours, geometry), aes(size=teacher_hours), alpha=0.7) +
  # Lines between Partner Schools
  geom_sf(data = models_lines %>% filter(RS_Kreis == "9162") %>% filter(sim==1) %>%
            filter(!is.na(Group)), aes(fill=Group)) +
  # Labs
  labs(subtitle=bold("Model 1:")~"Large city") +
  # Customized Theme
  guides(fill = "none") + theme_bw() + annotation_scale() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks=seq(11, 12, by=0.1)) +
  scale_size_continuous(name = "special educator hours", guide = "legend",
                        breaks = c(10, 25, 50, 75, 100), limits=c(0, 200))

## Tirschenreuth: Model 1 - - - - - - - - - - - - - - - - - - - - - - - - - - -     
plot3b <- ggplot() +
  # Map of Munich City
  geom_sf(data = district %>% filter(RS_Kreis=="9377") %>% select(geometry), 
          fill = "grey95", color = "black") +
  # Points of Schools
  geom_sf(data=models_sf %>% filter(RS_Kreis == "9377") %>% filter(sim==1) %>%
            select(teacher_hours, geometry), aes(size=teacher_hours), alpha=0.7) +
  # Lines between Partner Schools
  geom_sf(data = models_lines %>% filter(RS_Kreis == "9377") %>% filter(sim==1) %>%
            filter(!is.na(Group)), aes(fill=Group)) +
  # Labs
  labs(subtitle=bold("Model 1:")~"Sparsely populated district") +
  # Customized Theme
  guides(fill = "none") + theme_bw() + annotation_scale() +
  theme(legend.position = "bottom") +
  scale_size_continuous(name = "special educator hours", guide = "legend",
                        breaks = c(10, 25, 50, 75, 100), limits=c(0, 200))

## Munich: Model 2 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
plot3c <- ggplot() +
  # Map of Munich City
  geom_sf(data = district %>% filter(RS_Kreis=="9162") %>% select(geometry), 
          fill = "grey95", color = "black") +
  # Points of Schools
  geom_sf(data=models_sf %>% filter(RS_Kreis == "9162") %>% filter(sim==2) %>%
            select(teacher_hours, geometry), aes(size=teacher_hours), alpha=0.7) +
  # Lines between Partner Schools
  geom_sf(data = models_lines %>% filter(RS_Kreis == "9162") %>% filter(sim==2) %>%
            filter(!is.na(Group)), aes(fill=Group)) +
  # Labs
  labs(subtitle=bold("Model 2:")~"Large city") +
  # Customized Theme
  guides(fill = "none") + theme_bw() + annotation_scale() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks=seq(11, 12, by=0.1)) +
  scale_size_continuous(name = "special educator hours", guide = "legend",
                        breaks = c(10, 25, 50, 75, 100), limits=c(0, 200))

## Tirschenreuth: Model 2 - - - - - - - - - - - - - - - - - - - - - - - - - - -     
plot3d <- ggplot() +
  # Map of Munich City
  geom_sf(data = district %>% filter(RS_Kreis=="9377") %>% select(geometry), 
          fill = "grey95", color = "black") +
  # Points of Schools
  geom_sf(data=models_sf %>% filter(RS_Kreis == "9377") %>% filter(sim==2) %>%
            select(teacher_hours, geometry), aes(size=teacher_hours), alpha=0.7) +
  # Lines between Partner Schools
  geom_sf(data = models_lines %>% filter(RS_Kreis == "9377") %>% filter(sim==2) %>%
            filter(!is.na(Group)), aes(fill=Group)) +
  # Labs
  labs(subtitle=bold("Model 2:")~"Sparsely populated district") +
  # Customized Theme
  guides(fill = "none") + theme_bw() + annotation_scale() +
  theme(legend.position = "bottom") +
  scale_size_continuous(name = "special educator hours", guide = "legend",
                        breaks = c(10, 25, 50, 75, 100), limits=c(0, 200))

plot3 <- plot3a+plot3b+plot3c+plot3d+plot_layout(guides = "collect", nrow=2) & 
  theme(legend.position='bottom')

ggsave("H:/Daten/Bayern Daten/ZFE Artikel/Plot3.png", 
       plot=plot3, width=25, heigh=18, units=c("cm"), dpi=1200)

# Figure 4

## Figure 4a - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
plot4a <- models %>% 
  ggplot(aes(fill=SN_KTYP4_ENG, y = sum_hours, x=as.factor(sim))) + 
  geom_boxplot(color="black", alpha=0.8) +
  theme_bw() + theme(legend.position = "bottom") +
  scale_fill_manual(values=c("grey90", "grey70", "grey50", "grey30")) +
  labs(x="model", y="SEH per school pair", fill="district categories")

## Figure 4b - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
plot4b <- models %>% 
  ggplot(aes(fill=SN_KTYP4_ENG, y = dist_km, x=as.factor(sim))) + 
  geom_boxplot(alpha=0.8) +
  theme_bw() + theme(legend.position = "none") +
  scale_fill_manual(values=c("grey90", "grey70", "grey50", "grey30")) +
  labs(x="model", y="distance between school pairs (km)", 
       fill="district categories") +
  scale_y_continuous(position="right", breaks=seq(0, 30, 5),
                     limits = c(0, 20))

## Combining and saving figures - - - - - - - - - - - - - - - - - - - - - - - - 
plot4 <- (plot4a + plot4b)/guide_area() + 
  plot_layout(guides="collect", heights=c(6, 1)) +
  plot_annotation(subtitle=bold("Model comparison:")~
                    "Mobile special educator hour demand and school pair distances")
plot4

ggsave("H:/Daten/Bayern Daten/ZFE Artikel/Plot4.png", 
       plot=plot4, width=20, heigh=12, units=c("cm"), dpi=1200)

## 7. Analysis -----------------------------------------------------------------

names(models)

# Percentage of primary schools with > 1 special educator
nrow(models %>% filter(sim == 1) %>% filter(n_speced_teachers >= 1)) / 
  nrow(models %>% filter(sim == 1))*100
nrow(models %>% filter(sim == 2) %>% filter(n_speced_teachers >= 1)) / 
  nrow(models %>% filter(sim == 2))*100

# Percentage of primary schools with < 1 special educator
100-(nrow(models %>% filter(sim == 1) %>% filter(n_speced_teachers >= 1)) / 
  nrow(models %>% filter(sim == 1))*100)
100-(nrow(models %>% filter(sim == 2) %>% filter(n_speced_teachers >= 1)) / 
  nrow(models %>% filter(sim == 2))*100)

# Number of teacher hours and teachers per primary school
summary(models %>% filter(sim == 1) %>% pull(teacher_hours))
summary(models %>% filter(sim == 1) %>% pull(n_speced_teachers))
summary(models %>% filter(sim == 2) %>% pull(teacher_hours))
summary(models %>% filter(sim == 2) %>% pull(n_speced_teachers))

# Average demand of spec ed hours per district type
x <- models %>% group_by(RS_Kreis, sim) %>% filter(sim==1) %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T), 
         mean_teachers = mean(teachers, na.rm=T))

mean(x %>% filter(SN_KTYP4_ENG=="large cities") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="urban districts") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="rural districts") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="sparsley\npopulated") %>% pull(mean_hours))

summary(aov(data=x, mean_hours~SN_KTYP4_ENG))
pairwise.t.test(x$mean_hours, x$SN_KTYP4_ENG)

x <- models %>% group_by(RS_Kreis, sim) %>% filter(sim==2) %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T), 
         mean_teachers = mean(teachers, na.rm=T))

mean(x %>% filter(SN_KTYP4_ENG=="large cities") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="urban districts") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="rural districts") %>% pull(mean_hours))
mean(x %>% filter(SN_KTYP4_ENG=="sparsley\npopulated") %>% pull(mean_hours))

summary(aov(data=x, mean_hours~SN_KTYP4_ENG))
pairwise.t.test(x$mean_hours, x$SN_KTYP4_ENG)

# Difference in hours demand between sim 1 and 2
x <- models %>% group_by(RS_Kreis, sim) %>%filter(SN_KTYP4_ENG=="large cities") %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T))
t.test(data=x, mean_hours~sim)
x <- models %>% group_by(RS_Kreis, sim) %>%filter(SN_KTYP4_ENG=="urban districts") %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T))
t.test(data=x, mean_hours~sim)
x <- models %>% group_by(RS_Kreis, sim) %>%filter(SN_KTYP4_ENG=="rural districts") %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T))
t.test(data=x, mean_hours~sim)
x <- models %>% group_by(RS_Kreis, sim) %>%filter(SN_KTYP4_ENG=="sparsley\npopulated") %>%
  mutate(mean_hours = mean(teacher_hours, na.rm=T))
t.test(data=x, mean_hours~sim)

x <- filter(models, Ort == "Muenchen", sim=="2")

# Mobile special educator hours demand of district categories
models %>% 
  select(SN_KTYP4_ENG, sim, sum_hours) %>% na.omit() %>%
  count(sim, SN_KTYP4_ENG)

# Hours demand of mobile schools
summary(models %>% filter(sim == 1) %>% pull(sum_hours))
summary(models %>% filter(sim == 2) %>% pull(sum_hours))

# Distance between school pairs (km)
summary(models %>% filter(sim == 1) %>% pull(dist_km))
summary(models %>% filter(sim == 1 & SN_KTYP4_ENG == "urban districts") %>% pull(dist_km))
summary(models %>% filter(sim == 1 & SN_KTYP4_ENG == "rural districts") %>% pull(dist_km))
summary(models %>% filter(sim == 1 & SN_KTYP4_ENG == "sparsley\npopulated") %>% pull(dist_km))

summary(models %>% filter(sim == 2) %>% pull(dist_km))
summary(models %>% filter(sim == 2 & SN_KTYP4_ENG == "large cities") %>% pull(dist_km))
summary(models %>% filter(sim == 2 & SN_KTYP4_ENG == "urban districts") %>% pull(dist_km))
summary(models %>% filter(sim == 2 & SN_KTYP4_ENG == "rural districts") %>% pull(dist_km))
summary(models %>% filter(sim == 2 & SN_KTYP4_ENG == "sparsley\npopulated") %>% pull(dist_km))

# Tirschenreuth Example

x <- models %>% filter(RS_Kreis=="9377") %>% filter(system=="Model 1") 

# 6. Maps ----------------------------------------------------------------------

## Preparing variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
x <- models %>% left_join(select(district, RS_Kreis, GEN)) %>%
  select(RS_Kreis, GEN, KTYP4) %>% unique()

models_plot <- models %>% 
  left_join(select(district, RS_Kreis, GEN)) %>%
  mutate(studentsSEN_plot =case_when(Schulart=="Grundschulen" ~ n_students_SEN_new,
                                     Schulart=="Foerderzentren" ~ n_students_old))
models_sf <- models_plot %>% # Spatial Points
  mutate(Schultyp = case_when(
    Schultyp == "Grund- u. Mittelschule" ~ "Primary School",
    Schultyp == "GE" ~ "Special School - Intellectual Disability",
    Schultyp == "Sehen" ~ "Special School - Blindness",
    Schultyp == "Hoeren" ~ "Special School - Hearing",
    Schultyp == "KME" ~ "Special School - Physical Disability",
    Schultyp == "Kranke" ~ "Hospital School")) %>%
  st_as_sf(coords = c("lon", "lat"), crs=4326) 
models_lines <- models_sf %>% group_by(Group) %>% # Spatial Lines
  mutate(geometry = st_union(geometry) %>% st_cast("LINESTRING"))
  
## List of all districts
districts_unique <- unique(models_sf$RS_Kreis)

## Colors for mapping
farben <- colour("vibrant")(7)
school_colors <- c(
  "Primary School" = "black",
  "Special School - Intellectual Disability"     = farben[1], # orange
  "Special School - Blindness"  = farben[2], # dunkelblau
  "Special School - Hearing" = farben[3], # hellblau
  "Special School - Physical Disability"    = farben[5], # rot
  "Hospital School" = farben[6]) # dunkelgrün

## Plots of Model 1 for every district
for (kreis in districts_unique) {
  # Name des Kreises
  kreisname <- models_plot %>%
  filter(RS_Kreis == kreis) %>%
    distinct(GEN) %>%
    pull(GEN)
  # Filtere die Daten für den aktuellen Kreis
  kreis_geom <- district %>% filter(RS_Kreis == kreis) %>% select(geometry)
  schools <- models_sf %>% filter(RS_Kreis == kreis, sim == 1) %>% 
    select(studentsSEN_plot, Schultyp, geometry)
  # Plot
  p <- ggplot() +
    geom_sf(data = kreis_geom, fill = "grey95", color = "black") +
    geom_sf(data = schools, aes(size = studentsSEN_plot, color = Schultyp), alpha = 0.9) +
    scale_color_manual(values = school_colors) +
    labs(subtitle = paste0(kreisname, ": Model 1"),
         size="N students with SEN", color="School type") +
    guides(fill = "none") + 
    theme_bw() + annotation_scale() + theme(legend.position = "right")
  # Save plot
  ggsave(filename = paste0("Z:/2. Forschung/Projekte/Räumlich-Strukturelle Einflüsse auf Inklusion/Lehrerstunden Bayern (ZfE)/Karten alle Landkreise/Modell 1/", kreisname, "-", kreis, ".png"),
         plot = p, width = 20, height = 15, units=c("cm"), dpi = 500)
}

## Plots of Model 2 for every district
for (kreis in districts_unique) {
  # Name des Kreises
  kreisname <- models_plot %>%
    filter(RS_Kreis == kreis) %>%
    distinct(GEN) %>%
    pull(GEN)
  # Filtere die Daten für den aktuellen Kreis
  kreis_geom <- district %>% filter(RS_Kreis == kreis) %>% select(geometry)
  schools <- models_sf %>% filter(RS_Kreis == kreis, sim == 2) %>% 
    select(studentsSEN_plot, Schultyp, geometry)
  # Plot
  p <- ggplot() +
    geom_sf(data = kreis_geom, fill = "grey95", color = "black") +
    geom_sf(data = schools, aes(size = studentsSEN_plot, color = Schultyp), alpha = 0.9) +
    scale_color_manual(values = school_colors) +
    labs(subtitle = paste0(kreisname, ": Model 2"),
         size="N students with SEN", color="School type") +
    guides(fill = "none") + 
    theme_bw() + annotation_scale() + theme(legend.position = "right") +
  # Save plot
  ggsave(filename = paste0("Z:/2. Forschung/Projekte/Räumlich-Strukturelle Einflüsse auf Inklusion/Lehrerstunden Bayern (ZfE)/Karten alle Landkreise/Modell 2/", kreisname, "-", kreis, ".png"),
         plot = p, width = 20, height = 15, units=c("cm"), dpi = 500)
}






















































plot3 <- c %>% na.omit() %>%
  ggplot(aes(x = as.factor(sim), fill = jobs)) + 
  geom_bar(position="fill", color="black", alpha=0.7) +
  facet_wrap(vars(SN_KTYP4_ENG), nrow=1) +
  # legend, margins and style
  theme_bw() +
  theme(legend.position = "top", legend.justification = "left",
        legend.margin = margin(5,0,0,0),
        legend.box.margin = margin(-10,-10,-5,0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # labels and text
  ylab("% of work arrangement ") +
  xlab("simulation model") + 
  labs(subtitle=bold("Model comparison:")~
         "SEN teacher work arrangements in district types",
       fill = "work arrangements") +
  # colors
  scale_fill_manual(values=c("grey5", "grey50", "grey80"))

ggsave("H:/Daten/Bayern Daten/ZFE Artikel/Plot3.png", 
       plot=plot3, width=20, heigh=10, units=c("cm"), dpi=1200)

c1 <- filter(c, sim==1)
c2 <- filter(c, sim==2)
table(c1$jobs)
table(c2$jobs)





















