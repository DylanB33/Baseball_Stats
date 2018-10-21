#
 #  library(dplyr)
 #  library(ggplot2)
 #  library(ggformula)
 #  library(readr)
 #  library(ggpmisc)

Pitching_2018 <- read_csv("~/Desktop/Coding/StatCast_Data/PITCHING/2018/FanGraph_Pitching_2018.csv")
WHIP_2018 <- read_csv("~/Desktop/Coding/StatCast_Data/PITCHING/2018/FanGraph_WHIP_2018.csv")
Velocity_2018 <-read_csv("~/Desktop/Coding/StatCast_Data/PITCHING/2018/FanGraph_Velocity_2018.csv")

Pitching_2018 <-inner_join(Pitching_2018, WHIP_2018, by=c())
Pitching_2018 <-inner_join(Pitching_2018, Velocity_2018, by = c())


# Adding "Pitcher_Type" to Pitching_2018 df
FlyBall <- Pitching_2018 %>%
            filter(FlyBall_per > .45)%>%
            mutate(Pitcher_Type = "FlyBall")
GroundBall <- Pitching_2018 %>%
            filter(GB_per > .45)%>%
            mutate(Pitcher_Type = "GroundBall")
Neither  <- Pitching_2018 %>%
            filter(GB_per < .45 & FlyBall_per < .45)%>%
            mutate(Pitcher_Type = "Mixed")

FBvGB <- full_join(FlyBall, GroundBall, by = c())
FBvGB <- full_join(FBvGB, Neither, by=c())
FBvGB <- inner_join(FBvGB, WHIP_2018, by = c())


###################
FBvGB <- FBvGB %>%
        filter(IP > 60)
########################################################
########################################################

#Grouped Plot (by Pitcher Type)
Grouped_Plot <-gf_point( Response ~ Independant,
                         data = FBvGB,
                         color = ~ Pitcher_Type) %>%
  gf_lm(Response ~ Independant,
        data = FBvGB,
        color = ~Pitcher_Type ) +
  stat_poly_eq(data = FBvGB,
               formula = y~x,
               eq.with.lhs = "italic(y) ~`=`~",
               eq.x.rhs = "~italic(x)",
               aes(label= paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "right",
               label.y.npc = "top",
               parse = TRUE) +
  labs(x = title.x,
       y = title.y,
       title = title)


#Ungrouped Plot

unGrouped_Plot <-gf_point( Response ~ Independant,
                         data = FBvGB) %>%
  gf_lm(Response ~ Independant,
        data = FBvGB
        ) +
  stat_poly_eq(data = FBvGB,
               formula = y~x,
               eq.with.lhs = "italic(y) ~`=`~",
               eq.x.rhs = "~italic(x)",
               aes(label= paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "right",
               label.y.npc = "top",
               parse = TRUE) +
  labs(x = title.x,
       y = title.y,
       title = title)



model <- lm(Response ~ Independant, data = FBvGB)
ANOVA <- anova(model)

########################################################################
########################################################################

# Plot of Swinging Strike Percentage v. xFIP (unGrouped)

Independant <-FBvGB$SwStrike_per
Response <-FBvGB$xFIP
title.x <-"Swinging Strike Percentage"
title.y <-"xFIP"
title <- "xFIP ~ Swing Strike Percentage"

unGrouped_Plot


# Plot of Swinging Strike Percentage v. xFIP (Grouped)

Independant <-FBvGB$SwStrike_per
Response <-FBvGB$xFIP
title.x <-"Swinging Strike Percentage"
title.y <-"xFIP"
title <- "xFIP ~ Swing Strike Percentage"

Grouped_Plot

########################

# Swinging Strike Percentage ~ Fastball Velocity (UnGrouped)
Independant <-FBvGB$vFA
Response <-FBvGB$SwStrike_per
title.x <-"Average Fasball Velocity"
title.y <-"Swinging Strike Percentage %"
title <- "Swing Strike Percentage ~ Fastball Velocity"

unGrouped_Plot

# Swinging Strike Percentage ~ Fastball Velocity (Grouped)
Independant <-FBvGB$vFA
Response <-FBvGB$SwStrike_per
title.x <-"Average Fasball Velocity"
title.y <-"Swinging Strike Percentage %"
title <- "Swing Strike Percentage ~ Fastball Velocity"

Grouped_Plot


