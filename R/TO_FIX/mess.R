# figures for visualisation, input or source is varhp_table


#barplot total and constrained variation
varhp_table %>%
  ggplot(aes(x = dataset_id, y = log(Constrained_eig/Unconstrained_eig), fill = ecozone)) +
  geom_bar(stat= "identity")

# barplot variables mean unique and individual adj r2
varhp_table  %>%
  group_by(region, ecozone, Vars) %>%
  summarise(
    mean_unique = mean(Unique),
    mean.adj.r2 = mean(Individual),
    n = n()
  ) %>% 
  ungroup() %>% 
  ggplot(aes(y= mean.adj.r2, x = reorder(Vars, - mean.adj.r2), fill = Vars)) +
  geom_bar(stat = "identity", alpha = 0.5) +
  facet_wrap(~region + ecozone)

# variation adjR2 in ecozones within regions
varhp_table  %>%
  ggplot(aes(y = AdjR2_total, x = ecozone, fill = ecozone)) +
  geom_boxplot() +
  facet_wrap(~region)

# total eigenvalues in ecozones within regions
varhp_table  %>%
  ggplot(aes(y = Total_eig, x = ecozone, fill = ecozone)) +
  geom_boxplot() +
  facet_wrap(~region)

# unexplained variation per ecozone within regions
varhp_table  %>%
  ggplot(aes(y = Unconstrained_eig, x = ecozone, fill = ecozone)) +
  geom_boxplot() +
  facet_wrap(~region)


# function to get circular plots
plot.df <- . %>%
  filter(region == "Europe") %>%
  group_by(ecozone, Vars) %>%
  summarise(
    sum_unique = sum(Unique),
    mean.adj.r2 = mean(Individual),
    n = n()
  )





plt <- ggplot(plot.df) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0, 0.5, 1)),
    color = "lightgrey"
  ) + 
  # Add bars to represent the cumulative track lengths
  # str_wrap(region, 5) wraps the text so each line has at most 5 characters
  # (but it doesn't break long words!)
  geom_col(
    aes(
      x = reorder(str_wrap(Vars, 5), sum_unique),
      y = sum_unique,
      fill = n
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  
  # Add dots to represent the mean gain
  geom_point(
    aes(
      x = reorder(str_wrap(Vars, 5),sum_unique),
      y = mean.adj.r2
    ),
    size = 3,
    color = "gray12"
  ) +
  
  # Lollipop shaft for mean gain per region
  geom_segment(
    aes(
      x = reorder(str_wrap(Vars, 5), sum_unique),
      y = 0,
      xend = reorder(str_wrap(Vars, 5), sum_unique),
      yend = 1
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  
  # Make it circular!
  coord_polar()

plt

plt <- plt +
  # Annotate the bars and the lollipops so the reader understands the scaling
  annotate(
    x = 11, 
    y = 1300,
    label = "Mean Elevation Gain\n[FASL]",
    geom = "text",
    angle = -67.5,
    color = "gray12",
    size = 2.5,
    family = "Bell MT"
  ) +
  annotate(
    x = 11, 
    y = 3150,
    label = "Cummulative Length [FT]",
    geom = "text",
    angle = 23,
    color = "gray12",
    size = 2.5,
    family = "Bell MT"
  ) +
  # Annotate custom scale inside plot
  annotate(
    x = 11.7, 
    y = 1100, 
    label = "1000", 
    geom = "text", 
    color = "gray12", 
    family = "Bell MT"
  ) +
  annotate(
    x = 11.7, 
    y = 2100, 
    label = "2000", 
    geom = "text", 
    color = "gray12", 
    family = "Bell MT"
  ) +
  annotate(
    x = 11.7, 
    y =3100, 
    label = "3000", 
    geom = "text", 
    color = "gray12", 
    family = "Bell MT"
  ) +
  # Scale y axis so bars don't start in the center
  scale_y_continuous(
    limits = c(-1500, 3500),
    expand = c(0, 0),
    breaks = c(0, 1000, 2000, 3000)
  ) + 
  # New fill and legend title for number of tracks per region
  scale_fill_gradientn(
    "Amount of Tracks",
    colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195")
  ) +
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12),
    # Move the legend to the bottom
    legend.position = "bottom",
  )

plt