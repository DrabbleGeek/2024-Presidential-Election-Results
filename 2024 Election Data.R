# File: 2024 US Election Stuffs
# Working Directory
setwd("~/Data")

# PACKAGES ####
library(sf)
library(tidyverse)
library(maps)
library(ggspatial)
library(cowplot)

# DATA ####
## SF data ####
states <- read_sf("files/US_State_Boundaries.shx") |>
  select("NAME", "STATE_ABBR", geometry) |>
  rename("State.or.District" = "NAME")

## CSv Data Manipulation ####
state_elec <- read.csv("2024 Election Data.csv",
                       skip = 2) |>
  select("State.or.District":"Democratic.EV", "Total.Votes") |>
  mutate(Republican.Votes, Republican.Votes = str_remove_all(Republican.Votes,
                                                                 ",")) |>
  mutate(Republican.Votes, Republican.Votes = as.numeric(Republican.Votes)) |>
  mutate(Republican.., Republican.. = str_remove_all(Republican..,
                                                         "%")) |>
  mutate(Republican.., Republican.. = as.numeric(Republican..)) |>
  mutate(Republican.EV, Republican.EV = as.numeric(Republican.EV,
                                                       na_if(Republican.EV,
                                                             "-"))) |>
  mutate(Democratic.Votes, Democratic.Votes = str_remove_all(Democratic.Votes,
                                                             ",")) |>
  mutate(Democratic.Votes, Democratic.Votes = as.numeric(Democratic.Votes)) |>
  mutate(Democratic.., Democratic.. = str_remove_all(Democratic..,
                                                     "%")) |>
  mutate(Democratic.., Democratic.. = as.numeric(Democratic..)) |>
  mutate(Democratic.EV, Democratic.EV = as.numeric(Democratic.EV,
                                                   na_if(Democratic.EV,
                                                         "-"))) |>
  mutate(Total.Votes, Total.Votes = str_remove_all(Total.Votes,
                                                   ",")) |>
  mutate(Total.Votes, Total.Votes = as.numeric(Total.Votes)) |>
  filter(State.or.District != "Total")

## Combine Maine Data ####
maineInfo <- state_elec |>
  filter(State.or.District == "Maine †" | 
           State.or.District == "Maine's 1st congressional district" |
           State.or.District == "Maine's 2nd congressional district") |>
  #Info from filtering
  rbind(c("Maine",
          377837 + 164616 + 213221,
          ((377837 + 164616 + 213221) / (830989 + 429577 + 401412)) * 100,
          1,
          435351 + 255427 + 179924,
          ((435351 + 255427 + 179924)/(830989 + 429577 + 401412)) * 100,
          2 + 1,
          830989 + 429577 + 401412)) |>
  filter(State.or.District == "Maine")

state_elec <- state_elec |>
  rbind(maineInfo) |>
  filter(State.or.District != "Maine †" & 
           State.or.District != "Maine's 1st congressional district" &
           State.or.District != "Maine's 2nd congressional district")


## Combine Nebraska Data ####
nebraskaInfo <- state_elec |>
  filter(row_number() %in% 27:30) |>
  # based on filter
  rbind(c("Nebraska",
        177666 + 148905 + 238245 + 564816,
        round(((177666 + 148905 + 238245 + 564816) / (320194 + 318646 + 313342 + 952182) * 100),
              1),
        1 + 1 + 2,
        136153 + 163541 + 70301 + 369995,
        round(((136153 + 163541 + 70301 + 369995) / (320194 + 318646 + 313342 + 952182) * 100),
              1),
        1,
        320194 + 318646 + 313342 + 952182)) |>
  filter(State.or.District == "Nebraska")
  
state_elec <- state_elec |>
  filter(row_number() != 27 &
           row_number() != 28 &
           row_number() != 29 &
           row_number() != 30) |>
  rbind(nebraskaInfo)

rm(maineInfo)
rm(nebraskaInfo)
 
## State EV Color ####
state_elec <- state_elec |>
  mutate(evColor = ifelse(is.na(Republican.EV),
                         "D",
                         "R")) |>
  mutate(evColor = ifelse(State.or.District == "Maine",
                          "D",
                          evColor)) |>
  mutate(ev.. = ifelse(evColor == "R",
                       1,
                       -1)) |>
  mutate(ev.. = ifelse(State.or.District == "Maine",
                   -0.75,
                   ev..)) |>
  mutate(ev.., ev.. = ifelse(State.or.District == "Nebraska",
                           0.8,
                           ev..)) |>
  mutate(Republican.. = as.numeric(Republican..)) |>
  mutate(Democratic.. = as.numeric(Democratic..)) |>
  mutate(perChange = (Republican.. - Democratic..)/100) |>
  mutate(perChange = as.numeric(perChange))

## Combine data sets ####
state_elec <- left_join(state_elec, 
          states,
          "State.or.District") |>
  filter(State.or.District != "Total")

## Limit SF Data ####
cont <- state_elec |>
  filter(State.or.District != "Alaska") |>
  filter(State.or.District != "Hawaii") 

alaska <- state_elec |>
  filter(State.or.District == "Alaska")

hawaii <- state_elec |>
  filter(State.or.District == "Hawaii")

# Creation of Country Maps ####
## 270 Electoral votes ####
# Cont
Fig1a <- ggplot() +
  geom_sf(data = cont$geometry,
          color = "white",
          aes(fill = "red")) +
  scale_fill_manual(values = alpha("red", 1)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
  
# Alaska
Fig1b <- ggplot() +
  geom_sf(data = alaska$geometry,
          color = "white",
          aes(fill = "red")) +
  scale_fill_manual(values = alpha("red", 1)) +
  coord_sf(xlim = c(-180, -120)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Hawaii
Fig1c <- ggplot() +
  geom_sf(data = hawaii$geometry,
          color = "white",
          aes(fill = "red")) +
  scale_fill_manual(values = alpha("red", 1)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

## Percentage of EC ####
# Cont
Fig2a <- ggplot() +
  geom_sf(data = cont$geometry,
          color = "white",
          aes(fill = "red")) +
  scale_fill_manual(values = alpha("red", 0.16)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Alaska
Fig2b <- ggplot() +
  geom_sf(data = alaska$geometry,
          color = "white",
          aes(fill = "red")) +
  scale_fill_manual(values = alpha("red", 0.16)) +
  coord_sf(xlim = c(-180, -120)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Hawaii
Fig2c <- ggplot() +
  geom_sf(data = hawaii$geometry,
          color = "white",
          aes(fill = "red")) +
  scale_fill_manual(values = alpha("red", 0.16)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

## Percentage of PC ####
# Cont
Fig3a <- ggplot() +
  geom_sf(data = cont$geometry,
          color = "white",
          aes(fill = "red")) + 
  scale_fill_manual(values = alpha("red", 0.01)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Alaska
Fig3b <- ggplot() +
  geom_sf(data = alaska$geometry,
          color = "white",
          aes(fill = "red")) + 
  scale_fill_manual(values = alpha("red", 0.01)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(xlim = c(-180, -120))

#Hawaii
Fig3c <- ggplot() +
  geom_sf(data = hawaii$geometry,
          color = "white",
          aes(fill = "red")) +
  scale_fill_manual(values = alpha("red", 0.01)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Creation of State Maps ####
## % Electoral Votes ####
# Cont
Fig4a <- ggplot() +
  geom_sf(data = cont$geometry,
          color = "white",
          aes(fill = cont$ev..)) +
  scale_color_gradient2(low = "blue",
                        mid = "white",
                        high = "red",
                        midpoint = 0,
                        aesthetics = "fill",
                        name = "Percent EC",
                        guide = "colourbar") +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Alaska
Fig4b <- ggplot() +
  geom_sf(data = alaska$geometry,
          color = "white",
          aes(fill = alaska$ev..)) +
  scale_color_gradient2(low = "blue",
                        mid = "white",
                        high = "red",
                        midpoint = 0,
                        aesthetics = "fill",
                        limits = c(-1, 1)) +
  coord_sf(xlim = c(-180, -120)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Hawaii
Fig4c <- ggplot() +
  geom_sf(data = hawaii$geometry,
          color = "white",
          aes(fill = hawaii$ev..)) +
  scale_color_gradient2(low = "blue",
                        mid = "white",
                        high = "red",
                        midpoint = 0,
                        aesthetics = "fill",
                        limits = c(-1, 1)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

## Percent Vote ####
# Cont
Fig5a <- ggplot() +
  geom_sf(data = cont$geometry,
          color = "white",
          aes(fill = cont$perChange)) +
  scale_color_gradient2(low = "blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0,
                       aesthetics = "fill",
                       guide = "colourbar",
                       limits = c(-1,1),
                       name = "Percent Votes") +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Alaska
Fig5b <- ggplot() +
  geom_sf(data = alaska$geometry,
          color = "white",
          aes(fill = alaska$perChange)) +
  scale_color_gradient2(low = "blue",
                        mid = "white",
                        high = "red",
                        midpoint = 0,
                        aesthetics = "fill",
                        guide = "colourbar",
                        limits = c(-1, 1)) +
  coord_sf(xlim = c(-180, -120)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Hawaii
Fig5c <- ggplot() +
  geom_sf(data = hawaii$geometry,
          color = "white",
          aes(fill = hawaii$perChange)) +
  scale_color_gradient2(low = "blue",
                        mid = "white",
                        high = "red",
                        midpoint = 0,
                        aesthetics = "fill",
                        guide = "colourbar",
                        limits = c(-1, 1)) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Final Figures ####
# Figure 1. 2024 Election Results - 270 Electoral College Vote Victor
plot_grid(plot_grid(Fig1b,
                    Fig1c,
                    ncol = 1),
          Fig1a) +
  labs(title = "2024 Presidental Election
       Based on 270 Electoral Votes Needed for Win") +
  theme(plot.title = element_text(size = 16,
                                  face = "bold"))

# Figure 2. 2024 Election Results - Percent Change in Electoral College Votes
plot_grid(plot_grid(Fig2b,
                    Fig2c,
                    ncol = 1),
          Fig2a) +
  labs(title = "2024 Presidental Election 
       Based on Percent Change in Electoral College Votes") +
  theme(plot.title = element_text(size = 16,
                                  face = "bold"))

# Figure 3. 2024 Election Results - Percent Change in Popular Vote
plot_grid(plot_grid(Fig3b,
                    Fig3c,
                    ncol = 1),
          Fig3a) +
  labs(title = "2024 Presidental Election
       Based on Percent Change in Total Votes") +
  theme(plot.title = element_text(size = 16,
                                  face = "bold"))

# Figure 4. 2024 Election Results Based on State Electoral College Data
plot_grid(plot_grid(Fig4b,
                    Fig4c,
                    ncol = 1),
          Fig4a + theme(legend.position = "none"),
          get_legend(Fig4a),
          ncol = 3,
          rel_widths = c(1, 1, 0.25)) +
  labs(title = "2024 Presidental Election
       Based on State Electoral College Votes") +
  theme(plot.title = element_text(size = 16,
                                  face = "bold"))

# Figure 50 2024 Election Results Based on State Popular Vote
plot_grid(plot_grid(Fig5b,
                    Fig5c,
                    ncol = 1),
          Fig5a + theme(legend.position = "none"),
          get_legend(Fig5a),
          ncol = 3,
          rel_widths = c(1, 1, 0.25)) +
  labs(title = "2024 Presidental Election Based on State Popular Vote") +
  theme(plot.title = element_text(size = 16,
                                  face = "bold"))

# CLEAN UP ####
#Clear R
# Ctrl+Shift+F10 or Session > Restart R
#Clear console
cat("\014") #Mimics ctrl+L
