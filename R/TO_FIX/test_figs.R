# testing and creating visualisation functions

data_to_run <- read_rds("/Users/vfe032/Dropbox/03_GITHUB/HOPE/Data_for_testing/data_to_run_2022-12-09__429e3f1ff6b24b23b9de0986dad16293__ (1).rds")

test <-
  data_to_run$data_for_ord %>%
  rlang::set_names(
    nm = data_to_run$dataset_id
  ) %>%
  purrr::map_dfr(
    .id = "dataset_id",
    .f = ~ get_varhp(
      data_source = .x,
      reponse_vars = c(
        "n0", "n1", "n2",
        "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0",
        "roc",
        "dcca_axis_1"
      ),
      predictor_vars = list(
        human = c("spd"),
        climate = c(
          "temp_cold",
          "prec_summer",
          "prec_win",
          "gdm"
        ),
        time = c("age")
      ),
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = 99
    ) %>%
      purrr::pluck("summary_table")
  )

data_for_vis <- 
  test %>% 
  group_by(dataset_id) %>%
  mutate(AdjR2_total = sum(Individual)) %>%
  ungroup() %>%
  rename(I_perc = `I.perc(%)`) %>%
  inner_join(data_meta %>% 
               dplyr::select(dataset_id, lat, long, region, ecozone_koppen_5))



# get full output
output_varpart <- 
data_to_run$data_for_ord %>%
  rlang::set_names(
    nm = data_to_run$dataset_id
  ) %>%
  purrr::map(
    .id = "dataset_id",
    .f = ~ get_varhp(
      data_source = .x,
      reponse_vars = c(
        "n0", "n1", "n2",
        "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0",
        "roc",
        "dcca_axis_1"
      ),
      predictor_vars = list(
        human = c("spd"),
        climate = c(
          "temp_cold",
          "prec_summer",
          "prec_win",
          "gdm"
        ),
        time = c("age")
      ),
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = 99
    ) 
  )



input <- data_for_vis %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(
    mean_unique_adj_r2 = mean(Unique),
    mean_ind_adj_r2 = mean(Individual),
    mean_shared_adj_r2 = mean(Average.share),
    sd_unique_adj_r2 = sd(Unique),
    sd_ind_adj_r2 = sd(Individual),
    sd_shared_adj_r2 = sd(Average.share),
    lat = mean(lat),
    long = mean(long)
  ) %>% 
  ungroup()



# TEST DIFFERENT FIGS

#quick barplots of individual predictors between regions and ecozones
plot_individual_predictor_bars(data_for_vis)

# quick boxplot of total expl var among regions and ecozones
boxplot_expvar_by_region(data_for_vis)



# VENN DIAGRAMS
library(ggplot2)
library(ggforce)
library(ggVennDiagram)



# example 1 using ggvenn
test <- data_for_vis %>%
  nest(data = -dataset_id)

# new tibble: one vector value, logical for the predictor variables
testvenn2 <- tibble(
  Fraction = c(0.0140, 0.0466, 0.3090),
  climate = c(TRUE, FALSE, TRUE),
  time =  c(FALSE, TRUE, TRUE)
)

ggvenn::ggvenn(testvenn2, 
               c("climate", "time"), 
               show_elements = "Fraction",
               fill_color = c("blue", "red"),
               stroke_alpha = .3)

# list example
output_varpart$`3927`$varhp_output$Var.part
fract <- 
  list(human = c(0.0081, 0.0083, 0.0051, 0.0151 ),
       climate = c(0.0257, 0.0083, 0.0368, 0.0151),
       time = c(0.0021, 0.0051, 0.0368, 0.0151))

ggvenn::ggvenn(fract,show_element = TRUE)


# CREATE NEW VENN PLOTS USING GGPLOT AND GGFORCE

# extract var.part fractions from output
fraction.df <- output_varpart$`3927`$varhp_output$Var.part %>% as.data.frame()


# create dataframe for venn diagram (or circles)
df.venn <- data.frame(x = c(0, 0.866, -0.866),
                      y = c(1, -0.5, -0.5),
                      labels = c('Human', 'Climate', 'Time'))

# create dataframe for the fractions and the xy coordinates of the circle
df.vdc <- fraction.df[-8,1] %>% # remove total
  as_tibble %>%
  mutate(x = c(0, 1.2, -1.2, 0.8,  -0.8, 0, 0),
         y = c(1.2, -0.6, -0.6, 0.5,  0.5, -1, 0))

tot.expl.var <- fraction.df[8,1]
  
ggplot(df.venn, aes(x0 = x, y0 = y, r = 1.5, fill = labels)) +
  geom_circle(alpha = .3, size = 1, colour = 'grey') +
  coord_fixed() +
  theme_void() +
  #theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('cornflowerblue', 'firebrick',  'gold')) +
  scale_colour_manual(values = c('cornflowerblue', 'firebrick', 'gold'), guide = FALSE) +
  labs(fill = NULL) +
  annotate("text", x = df.vdc$x, y = df.vdc$y, label = df.vdc$value, size = 5) + 
  labs(caption = paste0("Total explained var = ", tot.expl.var)) +
  theme(
    plot.caption.position = "plot",
    plot.caption = element_text(size = 12, hjust = 0.1)
    )



# CICULAR BARPLOTS
# EXAMPLE 1
input %>%
ggplot(aes(x = mean_ind_adj_r2, fill = predictor)) +
  geom_histogram(binwidth = .05, colour = "black", size = .25) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_polar() +
  scale_x_continuous(limits = c(0,1),
                     breaks = seq(0, 1, by = 0.1),
                     minor_breaks = seq(0, 1, by = 0.2)) +
  scale_fill_brewer() +
  facet_wrap(~ecozone_koppen_5 + region)


# EXAMPLE 2

theme_opt <-  theme(
  # Remove axis ticks and text
  axis.title = element_blank(),
  #axis.ticks = element_blank(),
  #axis.text.y = element_blank(),
  # Use gray text for the region names
  axis.text.x = element_text(color = "gray12", size = 10),
  # Move the legend to the bottom
  legend.position = "bottom"
)

y_var = "mean_ind_adj_r2"  

input %>%
  ggplot()  +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0, 0.3)), # scale relative to max adjr2
    color = "lightgrey") + 
  # Add bars to represent the mean adj r2
  # str_wrap() wraps the text 
  geom_col(
    aes(
      x = reorder(str_wrap(interaction(region,ecozone_koppen_5), 5), get(y_var)),
      y = get(y_var),
      fill = predictor
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
    ) +
  # Add dots to represent the mean adjr2
  geom_point(
    aes(
      x = reorder(str_wrap(interaction(region, ecozone_koppen_5), 5), get(y_var)),
      y = get(y_var),
      col = predictor
      ),
    size = 2
    ) +
  # Lollipop shaft 
  geom_segment(
    aes(
      x = reorder(str_wrap(interaction(region, ecozone_koppen_5), 5), get(y_var)),
      y = 0,
      xend = reorder(str_wrap(interaction(region, ecozone_koppen_5), 5), get(y_var)),
      yend = 0.3
        ),
    linetype = "dashed",
    color = "gray12"
    ) +
  coord_polar() +
  theme_opt
 
# EXAMPLE SPIDER PLOT WITH ERROR 

input

input %>%
  mutate(predictor = as.factor(predictor)) %>%
  ggplot(aes(x = predictor, 
             y = mean_unique_adj_r2, 
             group = ecozone_koppen_5,
             col = ecozone_koppen_5,
             fill = ecozone_koppen_5)) +
  geom_polygon(aes(y = mean_unique_adj_r2 + (1.96 * sd_unique_adj_r2)), 
    #fill = "grey50", 
    alpha = 0.5) +
  geom_polygon(aes(y = mean_unique_adj_r2 - (1.96 * sd_unique_adj_r2)), 
               fill = "grey99", 
               alpha = 0.7) +
  geom_polygon(fill = NA) +
  theme_light() +
  theme(panel.grid.minor = element_blank()) + 
  coord_polar() +
  labs(x = "", y = "") +
  facet_wrap(~region)

# other spider/rader chart example

library(fmsb)
?raderchart

# HOW TO GET FIGS ON A WORLDMAP

# GET MAP 
worldmap <- map_data("world")

mapplot1 <- worldmap %>%
  ggplot(aes(x = long, y = lat, group = group), col = "white", fill = "gray50")  +
  borders(colour = "gray50", fill = "gray50") +
  coord_fixed()

mapplot1 


# some code example from stack
# library(ggsubplot)
# library(ggplot2)
# library(maps)
# library(plyr)
# 
# #Get world map info
# world_map <- map_data("world")
# 
# #Create a base plot
# p <- ggplot()  + geom_polygon(data=world_map,aes(x=long, y=lat,group=group))
# 
# # Calculate the mean longitude and latitude per region, these will be the coördinates where the plots will be placed, so you can tweak them where needed.
# # Create simulation data of the age distribution per region and merge the two.
# 
# centres <- ddply(world_map,.(region),summarize,long=mean(long),lat=mean(lat))
# mycat <- cut(runif(1000), c(0, 0.1, 0.3, 0.6, 1), labels=FALSE) 
# mycat <- as.factor(mycat)
# age <- factor(mycat,labels=c("<15","15-30","20-60",">60"))
# simdat <- merge(centres ,age)
# colnames(simdat) <- c( "region","long","lat","Age" )
# 
# # Select the countries where you want a subplot for and plot
# simdat2 <- subset(simdat, region %in% c("USA","China","USSR","Brazil", "Australia"))
# (testplot <- p+geom_subplot2d(aes(long, lat, subplot = geom_bar(aes(Age, ..count.., fill = Age))), bins = c(15,12), ref = NULL, width = rel(0.8), data = simdat2))
