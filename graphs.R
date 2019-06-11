# this script will create graphs from 
# the espn pre-season fantasy rankings 
# and fantasy pros end of season rankings


# libraries 
 library(stringr)
 library(dplyr)
 library(ggplot2)
 library(tidyr)


# reading files 
 
 espn_rankings <- read.csv(
   'data/espn rankings_16_18.csv', 
   stringsAsFactors = F
 )
 
 fp_finishes <- read.csv(
   'data/fp finishes_16_18.csv', 
   stringsAsFactors = F
 )

 
 weekly_rankings <- read.csv(
   'data/fp weekly finishes_16_18.csv', 
   stringsAsFactors = F
 )





# joining rankings to finishes ======================================================================

# going to join based on year, first, and last name
# would do team, but fantasypros only has current team
# not the team they were on back during the season
# going to left join to espn rankings because not every player
# was ranked


rank_finish <- left_join(
  espn_rankings, fp_finishes, 
  by = c('first', 'last', 'year', 'Pos' = 'Position')
)

rank_finish %>% group_by(first, last, year) %>% 
  summarize(count = n()) %>% 
  filter(count > 1)


# removing defense from rankings 

espn_rankings <- espn_rankings %>% filter(
  Pos != 'DST' & Pos !='D/ST' & Pos != 'K'
)


rank_finish <- left_join(
  espn_rankings, fp_finishes, 
  by = c('first', 'last', 'year', 'Pos' = 'Position')
)

# removing missing players 
# after research it appears most are instances where 
# the player didn't play during the season
# or switched positions (ty monty)

rank_finish <- rank_finish %>% filter(
  !is.na(finish)
)


# going to filter out players with less than 10 games
# going to make that a separate frame so that i can 
# easily come back if needed


full_szn_rf <- rank_finish %>% filter(
  Games > 9
)


# variance graph ===========================================================================================

# creating variable that is just 
# difference between initial rank and final finish
# going to graph that over time to see if 
# higher or lower rankings are more volatile

# also filtering out kickers, because who cares


full_szn_rf <- full_szn_rf %>% mutate(
  variance = init_rank - finish
)



full_szn_rf %>% ggplot(aes(init_rank, variance))+
  geom_point(aes(color = as.character(year)))+
  geom_hline(aes(yintercept = 0))+
  facet_wrap(. ~ Pos, scales = 'free_x')+
  scale_color_discrete(name = 'year')+
  theme_bw()

full_szn_rf %>% ggplot(aes(init_rank, variance))+
  geom_point(aes(color = Pos))+
  geom_hline(aes(yintercept = 0))+
  geom_abline(aes(intercept = -30, slope = 1), linetype = 'dashed')+
  facet_grid(year ~ . , scales = 'free_x')+
  scale_color_discrete(name = 'Pos')+
  theme_bw()



# probably not much value of looking at 
# players ranked over 60
# if 10 teams, 6 of any positions is the likely 
# max, so anything past that is maybe the last pick of
# the draft

full_szn_top <- full_szn_rf %>% filter(
  init_rank < 61
)






# graph of rb and WR initial ranks v final finishes 
# dashed line indicates top-30 or better (above)
# or worse than top-30 finish (below)
# top-30 because in a ten-team 2 wr & rb and 1 flex
# league, top-30 would be startable


full_szn_top %>% filter(
  Pos %in% c('WR', 'RB')
) %>% ggplot(aes(init_rank, variance))+
  geom_point(aes(color = Pos))+
  geom_hline(aes(yintercept = 0))+
  geom_abline(aes(intercept = -30, slope = 1), linetype = 'dashed')+
  facet_grid(year ~ . , scales = 'free_x')+
  scale_color_discrete(name = 'Position')+
  ylab('Initial minus Finish')+
  xlab('Initial ESPN Ranking')+
  theme_bw()+
  ggsave('graphs/espn rank variance years.png', width = 13, height = 6)





# graph of rb and WR initial ranks v final finishes 
# dashed line indicates top-30 or better (above)
# or worse than top-30 finish (below)
# top-30 because in a ten-team 2 wr & rb and 1 flex
# league, top-30 would be startable

full_szn_top %>%filter(
  Pos %in% c('WR', 'RB')
) %>%  ggplot(aes(init_rank, variance))+
  geom_point(aes(color = as.character(year)))+
  geom_hline(aes(yintercept = 0))+
  geom_abline(aes(intercept = -30, slope = 1), linetype = 'dashed')+
  facet_grid(Pos ~ . , scales = 'free_x')+
  scale_color_discrete(name = 'Year')+
  ylab('Initial minus Finish')+
  xlab('Initial ESPN Ranking')+
  theme_bw()+
  ggsave('graphs/espn rank variance pos.png', width = 13, height = 6)




# graph of what ten (10, 20 , 30, etc)
# position and number of top 30 from that ten
# also average variance of that ten 
# so you can see expected finish from each ten 
# compare across years and positions

full_szn_top <- full_szn_top %>% mutate(
  ten = 1 + floor((init_rank-1) / 10)
)

full_szn_top$top_rank <- if_else(
  full_szn_top$finish <31, 1, 0
)

full_szn_top$top_ten <- if_else(
  full_szn_top$finish <11, 1, 0
)


ten_ranks <- full_szn_top %>% group_by(
  year, ten, Pos
) %>% summarize(
  avg_var = mean(variance), 
  avg_finish = mean(finish), 
  top_players = sum(top_rank), 
  net_var = sum(variance), 
  top_ten = sum(top_ten)
)







# count of top ten finishes for rbs and wrs
# within the same 10 ranks (e.g. 1-10, 10-20)

ten_ranks %>% filter(
  Pos %in% c('WR', 'RB')
) %>% ggplot(aes(ten, top_players))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ . )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'RB' = 'dodgerblue4', 
      'WR' = 'tomato3'
    )
    )+
  scale_y_continuous(
    name = 'Count of Top 30 Finishes', 
    breaks = c(0:10)
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:6), 
    labels = paste('Top ', seq(10, 60, 10), sep = '')
  )+
  theme_bw()+
  ggsave(
    'graphs/ESPN top finish counts.png', width = 13, height = 6
  )




# count of top ten finishes for qbs and tes
# within the same 10 ranks (e.g. 1-10, 10-20)

ten_ranks %>% filter(
  Pos %in% c('TE', 'QB')
) %>% ggplot(aes(ten, top_ten))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ . )+
  scale_fill_manual(
    name = 'Position',
    values = c('TE' = 'dodgerblue3', 'QB' = 'forestgreen')
    )+
  scale_y_continuous(
    name = 'Count of Top 10 Finishes', 
    breaks = c(0:10)
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:6), 
    labels = paste('Top ', seq(10, 60, 10), sep = '')
  )+
  theme_bw()+
  ggsave(
    'graphs/ESPN top finish counts_te.png', width = 13, height = 6
  )






# showing the average difference between initial ESPN rank
# and final fantasy finish for players 
# within the same 10 ranks (e.g. 1-10, 10-20)

ten_ranks %>% filter(
  Pos %in% c('WR', 'RB')
) %>% ggplot(aes(ten, avg_var))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ . )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'RB' = 'dodgerblue4', 
      'WR' = 'tomato3'
    )
  )+
  scale_y_continuous(
    name = 'Average Variance between Rank and Finish'
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:6), 
    labels = paste('Top ', seq(10, 60, 10), sep = '')
  )+
  theme_bw()+
  ggsave(
    'graphs/ESPN ranks average variance.png', 
    height = 6, width = 13
  )







# showig the average finish for players
# within the same 10 ranks (e.g. 1-10, 10-20)


ten_ranks %>% filter(
  Pos %in% c('WR', 'RB')
) %>% ggplot(aes(ten, avg_finish))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  geom_hline(aes(yintercept = 30), linetype = 'dashed')+
  facet_grid(year ~ . )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'RB' = 'dodgerblue4', 
      'WR' = 'tomato3'
    )
  )+
  scale_y_continuous(
    name = 'Average Finish'
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:6), 
    labels = paste('Top ', seq(10, 60, 10), sep = '')
  )+
  theme_bw()+ggsave(
    'graphs/ESPN rank average finish.png', 
    width = 13, height = 6
  )








# showing the net difference between initial rank 
# and final finish for players ranked within the same ten

ten_ranks %>% filter(
  Pos %in% c('WR', 'RB')
) %>% ggplot(aes(ten, avg_var))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ . )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'RB' = 'dodgerblue4', 
      'WR' = 'tomato3'
    )
  )+
  scale_y_continuous(
    name = 'Average Variance between Rank and Finish'
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:6), 
    labels = paste('Top ', seq(10, 60, 10), sep = '')
  )+
  theme_bw()+
  ggsave(
    'graphs/ESPN ranks net variance.png', 
    height = 6, width = 13
  )







# count of top ten finishes for rbs and wrs
# within the same 10 ranks (e.g. 1-10, 10-20)

ten_ranks %>% filter(
  Pos %in% c('WR', 'RB')
) %>% ggplot(aes(ten, top_ten))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ . )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'RB' = 'dodgerblue4', 
      'WR' = 'tomato3'
    )
  )+
  scale_y_continuous(
    name = 'Count of Top 10 Finishes', 
    breaks = c(0:10)
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:6), 
    labels = paste('Top ', seq(10, 60, 10), sep = '')
  )+
  theme_bw()+
  ggsave(
    'graphs/ESPN top ten finish counts.png', width = 13, height = 6
  )










# fantasy finish graphs ==========================================================================================


top_40_finishes <- fp_finishes %>% 
  filter(finish < 41)


# density graph of point finishes by year by position
# shows that rb has a very wide distribution, but 
# that the top scoring rb's always score more than the top scoring 
# wide receivers 
# tight ends are almost all worthless 
# quarterbacks have a very flat distribution

top_40_finishes %>% filter(
  Position %in% c('RB', 'WR', 'TE', 'QB')
) %>%  ggplot(aes(Points))+
  geom_density(aes(fill = Position), alpha = .2) +
  facet_grid(year ~ .)+
  theme_bw()+
  ggsave(
    'graphs/point distribution of positions.png', 
    width = 13, height = 6
  )





# graph shows that top finishing rb's are 
# more valuable than top finishing WR's
# but after the top they are equal

top_40_finishes %>% filter(
  Position %in% c('RB', 'WR', 'TE', 'QB')
) %>% ggplot(aes(finish, Points))+
  geom_line(aes(color = Position))+
  geom_vline(aes(xintercept = 10))+
  geom_vline(aes(xintercept = 30))+
  facet_grid(year ~ .)+
  xlab('End of Season Finish')+
  theme_bw()+
  ggsave(
    'graphs/points by finish and position.png', 
    width = 13, height = 6
  )





# joining espn_ranks and weekly finishes ====================================================================



# top ten finishes 
# a top ten finish is really good for WR and RB
# and startable for QB and TE

top_10_finishes <- weekly_rankings %>% filter(
  finish < 11
) %>% group_by(
  first, last, Position, year
) %>% summarize(
  count = n()
)



rank_top_10 <- left_join(
  espn_rankings, top_10_finishes, 
  by = c('first', 'last', 'year', 'Pos' = 'Position')
)


rank_top_10 <- rank_top_10 %>% mutate(
  ten = 1 + floor((init_rank-1) / 10)
) %>% filter(
  init_rank < 61 & !is.na(count)
) %>% group_by(year, ten, Pos) %>% summarize(
  count = sum(count)
) %>% ungroup() %>% complete(
  year, ten, Pos, fill = list(count = 0)
)





# going to do top-5 finishes 
# for qb and te since you can only start one
# top 5 would be a really good week 

top_5_finishes <- weekly_rankings %>% filter(
  finish < 6
) %>% group_by(
  first, last, Position, year
) %>% summarize(
  count = n()
)



rank_top_5 <- left_join(
  espn_rankings, top_5_finishes, 
  by = c('first', 'last', 'year', 'Pos' = 'Position')
)


rank_top_5 <- rank_top_5 %>% mutate(
  ten = 1 + floor((init_rank-1) / 10)
) %>% filter(
  init_rank < 61 & !is.na(count)
) %>% group_by(year, ten, Pos) %>% summarize(
  count = sum(count)
) %>% ungroup() %>% complete(
  year, ten, Pos, fill = list(count = 0)
)






# going to do top 30 for WR and RB
# since that would be startable


top_30_finishes <- weekly_rankings %>% mutate(
  ten_finish = paste('Top', 10*(1 + floor((finish-1) / 10)))
) %>% filter(
  finish < 31
) %>% group_by(
  first, last, Position, year, ten_finish
) %>% summarize(
  count = n()
)



top_30_finishes <- left_join(
  espn_rankings, top_30_finishes, 
  by = c('first', 'last', 'year', 'Pos' = 'Position')
)


top_30_finishes <- top_30_finishes %>% mutate(
  ten = 1 + floor((init_rank-1) / 10)
) %>% filter(
  init_rank < 61 & !is.na(count)
) %>% group_by(year, ten, Pos, ten_finish) %>% summarize(
  count = sum(count)
) %>% ungroup() %>% complete(
  year, ten, Pos, fill = list(count = 0)
)











# graphs of weekly finishes ==================================================================================







# graphs the number of top ten weekly finishes 
# for players ranked in a ten range 
# rbs and wrs

rank_top_10 %>% filter(Pos %in% c('WR', 'RB')) %>% 
  ggplot(aes(ten, count))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ .)+
  theme_bw()+
  scale_y_continuous(
    name = 'Count of Top Ten Weekly Finishes'
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:6), 
    labels = paste('Top ', seq(10, 60, 10), sep = '')
  )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'RB' = 'dodgerblue4', 
      'WR' = 'tomato3'
    )
  )+
  ggsave('graphs/count of top ten weeks.png', width = 13, height = 6)






# graphs the number of top ten weekly finishes 
# for players ranked in a ten range 
# rbs and wrs

rank_top_10 %>% filter(
  Pos %in% c('QB', 'TE') & ten < 4
  ) %>% 
  ggplot(aes(ten, count))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ .)+
  theme_bw()+
  scale_y_continuous(
    name = 'Count of Top Ten Weekly Finishes'
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:3), 
    labels = paste('Top ', seq(10, 30, 10), sep = '')
  )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'TE' = 'dodgerblue3', 'QB' = 'forestgreen'
    )
  )+
  ggsave('graphs/count of top ten weeks_qb_te.png', width = 13, height = 6)










# number of top five weekly finishes 
# for players in a ten range
# tes and qbs


rank_top_5%>% filter(
  Pos %in% c('TE', 'QB') & ten < 4
  ) %>% 
  ggplot(aes(ten, count))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ .)+
  theme_bw()+
  scale_y_continuous(
    name = 'Count of Top Five Weekly Finishes'
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:6), 
    labels = paste('Top ', seq(10, 60, 10), sep = '')
  )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'TE' = 'dodgerblue3', 'QB' = 'forestgreen'
    )
  )+
  ggsave(
    'graphs/count of top weeks_qbte.png', width = 13, height = 6
  )






# count of top 30 weekly finishes
# for WR colored by finish level

top_30_finishes %>% filter(Pos == 'WR') %>% 
  ggplot(aes(ten, count))+
  geom_col(aes(fill = ten_finish))+
  facet_grid(year ~ .)+
  scale_y_continuous(
    name = 'Count of Top 30 Weekly Finishes'
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:6), 
    labels = paste('Top ', seq(10, 60, 10), sep = '')
  )+ scale_fill_manual(
    name = 'Finish', 
    values = c(
      'Top 10' = 'darksalmon', 'Top 20' = 'tomato2', 'Top 30' = 'firebrick'
    )
  )+
  theme_bw()+
  ggtitle(
    'Top 30 Finishes for WR by Pre-season ESPN Rank'
  )+
  ggsave(
    'graphs/wr top 30 finishes.png', width = 13, height = 6
  )







# count of top 30 weekly finishes
# for RB colored by finish level

top_30_finishes %>% filter(Pos == 'RB') %>% 
  ggplot(aes(ten, count))+
  geom_col(aes(fill = ten_finish))+
  facet_grid(year ~ .)+
  scale_y_continuous(
    name = 'Count of Top 30 Weekly Finishes'
  )+
  scale_x_continuous(
    name = 'ESPN Ranks', 
    breaks = c(1:6), 
    labels = paste('Top ', seq(10, 60, 10), sep = '')
  )+ scale_fill_manual(
    name = 'Finish', 
    values = c(
      'Top 10' = 'cyan', 'Top 20' = 'dodgerblue', 'Top 30' = 'navy'
    )
  )+
  theme_bw()+
  ggtitle(
    'Top 30 Finishes for RB by Pre-season ESPN Rank'
  )+
  ggsave(
    'graphs/rb top 30 finishes.png', width = 13, height = 6
  )









# creating weekly finish data =============================================================================

# data for weekly finish rankings
# to see if pre-season ranks are consistent over time


rank_weeks <- left_join(
  espn_rankings, weekly_rankings, 
  by = c('first', 'last', 'year', 'Pos' = 'Position')
)


rank_weeks <- rank_weeks %>% filter(
  init_rank < 61 & !is.na(finish) & finish < 31
) %>% mutate(
  ten = paste('Top', 10*(1 + floor((init_rank-1) / 10))), 
  ten_finish = paste('Top', 10*(1 + floor((finish-1) / 10)))
) %>% group_by(year, week, ten, Pos, ten_finish) %>% summarize(
  count = n()
) %>% ungroup() %>% complete(
  year, ten, Pos, ten_finish, week, fill = list(count = 0)
)







# weekly finish graph ==============================================================================

# count of top ten running back finishes
# by week, by year, by pre-season rank

rank_weeks %>% filter(
  ten_finish == 'Top 10' & Pos == 'RB'
) %>% ggplot(aes(week, count))+
  geom_line(aes(group = ten, color = ten))+
  facet_grid(year ~ . )+
  scale_y_continuous(
    name = 'Count of Top Ten Finishes'
  )+
  scale_x_continuous(
    name = 'Week', breaks = c(1:16)
  )+
  scale_color_brewer(name = 'ESPN Rank', palette = 'Paired')+
  theme_bw()+
  ggtitle('Count of Running Back Top Ten Finishes by Week')+
  ggsave(
    'graphs/rb top ten weekly count.png', width = 13, height = 6
  )


# count of top ten wide receiver finishes
# by week, by year, by pre-season rank

rank_weeks %>% filter(
  ten_finish == 'Top 10' & Pos == 'WR'
) %>% ggplot(aes(week, count))+
  geom_line(aes(group = ten, color = ten))+
  facet_grid(year ~ . )+
  scale_y_continuous(
    name = 'Count of Top Ten Finishes'
  )+
  scale_x_continuous(
    name = 'Week', breaks = c(1:16)
  )+
  scale_color_brewer(name = 'ESPN Rank', palette = 'Paired')+
  theme_bw()+
  ggtitle('Count of Wide Receiver Top Ten Finishes by Week')+
  ggsave(
    'graphs/wr top ten weekly count.png', width = 13, height = 6
  )



# rb weekly count of 11-20 finishes 

rank_weeks %>% filter(
  ten_finish == 'Top 20' & Pos == 'RB'
) %>% ggplot(aes(week, count))+
  geom_line(aes(group = ten, color = ten))+
  facet_grid(year ~ . )+
  scale_y_continuous(
    name = 'Count of Top 11-20 Finishes'
  )+
  scale_x_continuous(
    name = 'Week', breaks = c(1:16)
  )+
  scale_color_brewer(name = 'ESPN Rank', palette = 'Paired')+
  theme_bw()+
  ggtitle('Count of Running Back Top 11-20 Finishes by Week')+
  ggsave(
    'graphs/rb top 20 weekly count.png', width = 13, height = 6
  )


# wr top 20 finishes by week

rank_weeks %>% filter(
  ten_finish == 'Top 20' & Pos == 'WR'
) %>% ggplot(aes(week, count))+
  geom_line(aes(group = ten, color = ten))+
  facet_grid(year ~ . )+
  scale_y_continuous(
    name = 'Count of Top 11-20 Finishes'
  )+
  scale_x_continuous(
    name = 'Week', breaks = c(1:16)
  )+
  scale_color_brewer(name = 'ESPN Rank', palette = 'Paired')+
  theme_bw()+
  ggtitle('Count of Wide Receiver Top 11-20 Finishes by Week')+
  ggsave(
    'graphs/wr top 20 weekly count.png', width = 13, height = 6
  )






# rb weekly count of 21-30 finishes 

rank_weeks %>% filter(
  ten_finish == 'Top 30' & Pos == 'RB'
) %>% ggplot(aes(week, count))+
  geom_line(aes(group = ten, color = ten))+
  facet_grid(year ~ . )+
  scale_y_continuous(
    name = 'Count of Top 21-30 Finishes'
  )+
  scale_x_continuous(
    name = 'Week', breaks = c(1:16)
  )+
  scale_color_brewer(name = 'ESPN Rank', palette = 'Paired')+
  theme_bw()+
  ggtitle('Count of Running Back Top 11-20 Finishes by Week')+
  ggsave(
    'graphs/rb top 30 weekly count.png', width = 13, height = 6
  )


# wr top 20 finishes by week

rank_weeks %>% filter(
  ten_finish == 'Top 30' & Pos == 'WR'
) %>% ggplot(aes(week, count))+
  geom_line(aes(group = ten, color = ten))+
  facet_grid(year ~ . )+
  scale_y_continuous(
    name = 'Count of Top 21-30 Finishes'
  )+
  scale_x_continuous(
    name = 'Week', breaks = c(1:16)
  )+
  scale_color_brewer(name = 'ESPN Rank', palette = 'Paired')+
  theme_bw()+
  ggtitle('Count of Wide Receiver Top 21-30 Finishes by Week')+
  ggsave(
    'graphs/wr top 30 weekly count.png', width = 13, height = 6
  )









# creating consistency data =================================================================================


# counts of players with a certain number
# of usable games to see if certain ranks
# have more or less consistent players


week_consistency <- left_join(
  espn_rankings, weekly_rankings, 
  by = c('first', 'last', 'year', 'Pos' = 'Position')
)


week_consistency <- week_consistency %>% filter(
  init_rank < 61 & !is.na(finish) & finish < 31
) %>% mutate(
  ten = paste('Top', 10*(1 + floor((init_rank-1) / 10))), 
  ten_finish = paste('Top', 10*(1 + floor((finish-1) / 10)))
) %>% group_by(year, first, last, Pos, ten, ten_finish) %>% summarize(
  count = n()
)

top_10_consistency <- week_consistency %>% 
  filter(
    ten_finish == 'Top 10' & count > 7
  ) %>% group_by(year, Pos, ten) %>% summarize(
    count = n()
  ) %>% ungroup() %>% complete(
    year, Pos, ten, fill = list(count = 0)
  )


top_20_consistency <- week_consistency %>% filter(
  ten_finish %in% c('Top 10', 'Top 20')
) %>% group_by(year, first, last, Pos, ten) %>% summarize(
  finishes = sum(count) 
) %>% filter(finishes > 7) %>% group_by(
  year, Pos, ten
) %>% summarize(
  count = n()
)%>% ungroup() %>% complete(
  year, Pos, ten, fill = list(count = 0)
)



top_30_consistency <- week_consistency %>% filter(
  ten_finish %in% c('Top 10', 'Top 20', 'Top 30')
) %>% group_by(year, first, last, Pos, ten) %>% summarize(
  finishes = sum(count) 
) %>% filter(finishes > 7) %>% group_by(
  year, Pos, ten
) %>% summarize(
  count = n()
)%>% ungroup() %>% complete(
  year, Pos, ten, fill = list(count = 0)
)







# consistency graphs ========================================================================================




top_10_consistency %>% filter(
  Pos %in% c('RB', 'WR')
) %>% ggplot(aes(ten, count))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ .)+
  theme_bw()+
  scale_y_continuous(
    name = 'Count of Players'
  )+
  scale_x_discrete(
    name = 'ESPN Ranks'
  )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'RB' = 'dodgerblue4', 
      'WR' = 'tomato3'
    )
  )+ggtitle(
    'Count of Players with 8+ Top Ten Games'
  )+ggsave(
    'graphs/top ten consistency_rb_wr.png', width = 13, height = 6
  )









top_20_consistency %>% filter(
  Pos %in% c('RB', 'WR')
) %>% ggplot(aes(ten, count))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ .)+
  theme_bw()+
  scale_y_continuous(
    name = 'Count of Players'
  )+
  scale_x_discrete(
    name = 'ESPN Ranks'
  )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'RB' = 'dodgerblue4', 
      'WR' = 'tomato3'
    )
  )+ggtitle(
    'Count of Players with 8+ Top 20 Games'
  )+ggsave(
    'graphs/top 20 consistency_rb_wr.png', width = 13, height = 6
  )









top_30_consistency %>% filter(
  Pos %in% c('RB', 'WR')
) %>% ggplot(aes(ten, count))+
  geom_col(aes(fill = Pos), position = 'dodge')+
  facet_grid(year ~ .)+
  theme_bw()+
  scale_y_continuous(
    name = 'Count of Players'
  )+
  scale_x_discrete(
    name = 'ESPN Ranks'
  )+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'RB' = 'dodgerblue4', 
      'WR' = 'tomato3'
    )
  )+ggtitle(
    'Count of Players with 8+ Top 30 Games'
  )+ggsave(
    'graphs/top 30 consistency_rb_wr.png', width = 13, height = 6
  )








# point distribution of weekly top finishes ================================================================


weekly_rankings %>% filter(
  Position %in% c('WR', 'RB') & finish < 31
) %>% ggplot(aes(Points))+
  geom_density(aes(fill = Position), alpha = .2)+
  facet_grid(year ~ .)+
  theme_bw()+
  scale_fill_manual(
    name = 'Position', 
    values = c(
      'RB' = 'dodgerblue4', 
      'WR' = 'tomato3'
    )
  )+
  ggtitle('Distribution of Points Scored')+
  ggsave(
    'graphs/weekly point distribution_rb_wr.png', width = 13, height = 6
  )




