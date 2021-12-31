p <- ggplot(mpg, aes(displ, cty)) + geom_point()
p
# Use vars() to supply variables from the dataset:
p + facet_grid(rows = vars(drv))

VarNames <- c("Deaths","Confirmed","People_partially_vaccinated_Perc")

p<- master_data %>%
  filter(is.na(Province_State), Country_Region %in% c("France","Germany","Peru")) %>%
  #select(Country_Region,Date,Deaths,Confirmed)%>%
  pivot_longer(
    cols = VarNames,
    names_to = "VarName",
    values_to = "Value"
    ) %>%
  ggplot(aes(Date,Value,color = Country_Region)) +
  geom_line() +
  facet_wrap(~VarName, ncol=1,strip.position = "bottom",scales = "free_y") +
  theme(axis.title.y=element_blank())


ggplotly(p)
