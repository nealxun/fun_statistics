# Monty Hall problem simulation

# load necessary packages
library(tidyverse)

# single game function
monty_hall_game <- function(door_num = 3, switch = FALSE) {
        # given the number of doors and whehter the player is going to swith the selection
        # return whether the play win the price
        doors <- seq(from = 1, to = door_num)
        
        door_price <- base::sample(doors, 1)
        initial_selection <- base::sample(doors, 1)
        
        if (switch) {
                # cross out doors
                door_not_selected <- doors[!doors %in% c(initial_selection)]
                
                if (door_price == initial_selection) {
                        door_cross_out <- base::sample(doors[!doors %in% c(door_price, initial_selection)], door_num - 2)
                } else {
                        door_cross_out <- doors[!doors %in% c(door_price, initial_selection)]
                }
                
                second_selection <- door_not_selected[!door_not_selected %in% door_cross_out]
                result <- ifelse(second_selection == door_price, "WIN", "LOSE")
        } else {
                result <- ifelse(initial_selection == door_price, "WIN", "LOSE")
        }
        
        return(result)
}

# simulation function
monty_hall_sim <- function(door_num = 3, game = 100, switch = FALSE) {
        # given number of doors, how many games are going to be played, the strategy the play chooses
        # return a data frame contains game index and game result
        
        ls_result <- vector("character", length(game))
        ls_strategy <- rep(switch, game)
        
        for (i in 1:game) {
                ls_result[i] <- monty_hall_game(door_num, switch)
        }
        
        df_final <- tibble(game_index = seq(1, game), switch = ls_strategy, result = ls_result)
        
        return(df_final)
}

# run simulation
DOOR_NUM <- 3
GAME <- 10000
df_sim_not_switch <- monty_hall_sim(DOOR_NUM, game = GAME, switch = FALSE)
df_sim_switch <- monty_hall_sim(DOOR_NUM, game = GAME, switch = TRUE)

# visual check
df <- bind_rows(df_sim_not_switch, df_sim_switch) %>%
        group_by(switch, result) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        mutate(switch = ifelse(switch == TRUE, "Switch", "Not Switch")) |> 
        mutate(result = fct(result, levels = c("WIN", "LOSE")))

df |> 
        ggplot(aes(x = switch, y = count, fill = result)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.25) +
        labs(x = "Strategy", y = "Game Count", title = "Month Hall Problem Simulation Result", 
             subtitle = paste0("Suppose there are ", DOOR_NUM, " doors in total and ", 
                               GAME, " games are played"))

df |> 
        ggplot(aes(x = result, y = count, fill = result)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.25) +
        geom_label(aes(label = result), vjust = 2.5, color = "white", size = 5) +
        facet_wrap(vars(switch), ncol = 1) +
        scale_fill_manual(values=c("#28A745", 
                                   "#DC3545"
                                   )) +
        labs(x = "", y = "Game Count", title = "If you switch - you get 2/3 chance to win the game", 
             subtitle = paste0("Suppose there are ", DOOR_NUM, " doors in total and ", 
                               GAME, " games are played")) +
        theme(legend.position = "none")

