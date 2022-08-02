key_votes<- read.csv("key_votes.csv")

leaderboard<- read.csv("leaderboard.csv")

Hall_all_members<- read.csv("HSall_members.csv")

HSall_parties<- read.csv("HSall_parties.csv")

regions<-read.csv("region.csv")


test<- leaderboard %>% 
  filter(first_cong > 93) %>% 
  filter(first_cong < 99) %>% 
  arrange(desc(oppo_per)) %>% 
  select(bioname, party_code, wing, oppo_per, gen, nominate_dim1, nominate_dim2) %>% 
  distinct()

test_gop<- leaderboard %>% 
  filter(party_code == "Republican") %>% 
  filter(first_cong > 93) %>% 
  filter(first_cong < 99) %>% 
  arrange(desc(oppo_per)) %>% 
  select(bioname, wing, oppo_per, gen, chamber, state_abbrev) %>% 
  distinct()

test_gop %>% 
  group_by(gen, wing) %>% 
  summarize(mean(oppo_per),
            number = n())

write.csv(test_gop, "gop_paul.csv")

pal<- c("blue", "red")

test %>% 
  filter(oppo_per > .5) %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, color = party_code)) + geom_point(aes(size = oppo_per)) +
  scale_color_manual(values = pal,
                     labels = c("Democrat", "Ron Paul")) +
  xlim(-1,1) + ylim(-1, 1) +
  geom_hline(yintercept = 0, linetype_1="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype_1="dashed", color = "grey", size = 1) +
  labs(title = "Congressional Foreign Policy Opposition, by Party and Ideology",
       subtitle = "Members Who Started Their Careers from 1975-1985 and Who Voted in Opposition More than 50%",
       color = "",
       size = "Opposition",
       x = "← Liberal      Economic/Redistributive      Conservative →",
       y = "← Liberal      Social Policy      Conservative →",
       caption = "Original Analysis and visualization by: Brandan P Buck, 
       Data source: Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin, and Luke Sonnet (2021). Voteview: Congressional Roll-Call Votes Database. https://voteview.com/") +
  scale_size_continuous(breaks = c (0.55, 0.60, 0.65, 0.70),
                        labels = c("55%", "60%", "65%", "70%+")) +
  theme_bw(base_size = 15) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=15, color = "Black", face="bold")) +
  guides(color = guide_legend(override.aes = list(size=7)),
         shape = guide_legend(override.aes = list(size=7)))

#mil

total_iso<- key_votes %>%  #Total number of votes in opposition to rollcalls
  filter(type_1 == "mil") %>% 
  filter(wing != "UNK") %>% 
  filter(cast_code == "iso") %>% 
  group_by(bioname) %>% 
  summarize(iso_num = n()) %>% 
  select(bioname, iso_num)

total_vote<- key_votes %>%  #Total number of votes to rollcalls, excluding abstentions, votes of present, etc. 
  filter(type_1 == "mil") %>% 
  filter(wing != "UNK") %>% 
  filter(cast_code != "nv") %>% 
  group_by(bioname) %>% 
  summarize(total_vote = n()) %>% 
  left_join(total_iso, by =c("bioname"))

total_vote[is.na(total_vote)] <- "0"

congress_num<- Hall_all_members %>% 
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  select(icpsr) %>% 
  group_by(icpsr) %>% 
  summarize(congress_num = n())

total_vote$total_vote <- as.numeric(as.character(total_vote$total_vote))
total_vote$iso_num <- as.numeric(as.character(total_vote$iso_num))

last_cong<- Hall_all_members %>%  #The last congress served by each individual
  group_by(icpsr) %>% 
  do(tail(., 1)) %>% 
  filter(congress > 73) %>% 
  select(icpsr, congress)

first_cong<- Hall_all_members %>% #The first congress served by each individual
  group_by(icpsr) %>% 
  do(head(., 1)) %>% 
  filter(congress < 103) %>% 
  select(icpsr, congress)

names(first_cong)[2]<-"first_cong"

names(last_cong)[2]<-"last_cong"

total_iso_session<- key_votes %>% 
  filter(type_1 == "mil") %>% 
  filter(cast_code == "iso") %>% 
  group_by(icpsr, party_code, chamber, congress) %>% 
  summarize(iso_num = n()) %>% 
  select(icpsr, party_code, chamber, congress, iso_num)

total_vote_session<- key_votes %>% 
  filter(type_1 == "mil") %>% 
  filter(cast_code != "nv") %>% 
  group_by(icpsr, party_code, chamber, congress) %>% 
  summarize(total_vote = n()) %>% 
  select(icpsr, party_code,chamber, total_vote, congress) %>% 
  left_join(total_iso_session, by = c("icpsr", "congress", "chamber", "party_code"))

total_vote_session[is.na(total_vote_session)] <- "0"

total_vote_session$total_vote <- as.numeric(as.character(total_vote_session$total_vote))
total_vote_session$iso_num <- as.numeric(as.character(total_vote_session$iso_num))


total_vote_session_1<- total_vote_session %>% 
  mutate(opp_sess = iso_num / total_vote) %>% 
  select(icpsr, congress, party_code, chamber, opp_sess) %>% 
  spread(congress, opp_sess)

total_vote_session_1[is.na(total_vote_session_1)] <- "X"

mil_leaderboard<- Hall_all_members %>%
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  filter(chamber != "President") %>% 
  left_join(regions, by = "state_abbrev") %>% 
  distinct(icpsr, bioname, chamber, party_code, wing, nominate_dim1, nominate_dim2, state_abbrev, born, died, gen, region) %>% 
  left_join(total_vote, by = c("bioname")) %>% 
  mutate(mil_oppo_per = iso_num / total_vote) %>% 
  arrange(desc(mil_oppo_per)) %>% 
  left_join(first_cong, by = "icpsr") %>% 
  left_join(last_cong, by = "icpsr") %>% 
  left_join(congress_num, by = "icpsr") %>% 
  drop_na(mil_oppo_per) %>% 
  left_join(total_vote_session_1, by = c("icpsr", "party_code", "chamber"))

write.csv(leaderboard, "clean_data/mil_leaderboard.csv")


mil<- mil_leaderboard %>% 
  filter(party_code ==  "Republican") %>% 
  filter(wing == "Right") %>% 
  filter(mil_oppo_per > .5) %>% 
  arrange(desc(first_cong)) %>% 
  select(bioname, party_code, wing, mil_oppo_per, gen, first_cong, nominate_dim1, nominate_dim2) %>% 
  distinct()

write.csv(mil, "paul_mil.csv")

mil %>% 
  ggplot(aes(x= first_cong, y= mil_oppo_per, color = wing)) + geom_point(size = 3)

test<- mil_leaderboard %>% 
  filter(congress_num > 1)

mil_leaderboard %>% 
  filter(party_code == "Republican") %>%
  ggplot(aes(x= first_cong, y= mil_oppo_per, color = wing, shape=chamber, 
             size = congress_num, label=bioname)) +
  geom_point(alpha = .75) +
  theme_bw(base_size = 15) +
  geom_hline(yintercept = 0.5, linetype="dashed", color = "black", size = .25) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=10, color = "Black", face="bold")) +
  guides(color = guide_legend(nrow = 1),(override.aes = list(size=10))) +
  scale_y_continuous(breaks=c(0, .25, .50, .75, 1),                                                                                 
                     labels=c("0%", "25%", "50%", "75%", "100%")) +
  annotate("rect", xmin = 77, xmax = 79, ymin = .0, ymax = 1,
           alpha = .4, fill = "grey") +
  annotate("rect", xmin = 80.75, xmax = 81.25, ymin = .0, ymax = 1,
           alpha = .4, fill = "grey") +
  annotate("rect", xmin = 96.75, xmax = 97.25, ymin = .0, ymax = 1,
           alpha = .4, fill = "grey") +
  annotate("rect", xmin = 100.5, xmax = 101.5, ymin = .0, ymax = 1,
           alpha = .4, fill = "grey") +
  annotate("rect", xmin = 88, xmax = 90, ymin = 0, ymax = 1,
           alpha = .4, fill = "grey") +
  annotate("text", label = "Reagan Elected", x = 96.5, y = .725, 
           color = "black", angle = 90, fontface="bold", size = 4) +
  annotate("text", label = "Start of the Korean War", x = 80.5, y = .69, 
           color = "black", angle = 90, fontface="bold", size = 4) +
  annotate("text", label = "Escalation in Vietnam", x = 87.75, y = .7, 
           color = "black", angle = 90, fontface="bold", size = 4) +
  annotate("text", label = "Fall of the Berlin Wall", x = 100.25, y = .7, 
           color = "black", angle = 90, fontface="bold", size = 4) +
  annotate("text", label = "US involvement in WWII", x = 76.75, y = .69, 
           color = "black", angle = 90, fontface="bold", size = 4) +
  annotate("text", label = "Ron Paul", x = 94, y = .87, 
           color = "black", angle = 0, fontface="bold", size = 5) +
  annotate("segment", x = 94, xend = 94, y = .85, yend = .72,
           colour = "black", size = 2, arrow = arrow()) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=10, color = "Black", face="bold")) +
  labs(title = "Republican Voting Patterns on Military Policy, 1935 - 1992",
       subtitle = "By Wing, Chamber, and First Congress Served",
       color = "Wing",
       shape = "Chamber",
       size = "Congresses Served", 
       x = "First Congress Served",
       y = "Career Opposition Percentage on Miliary Policy",
       caption = "Original Analysis and visualization by: Brandan P Buck, 
       Data source: Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin, and Luke Sonnet (2021). Voteview: Congressional Roll-Call Votes Database. https://voteview.com/") 


test<- econ_leaderboard %>% 
  filter(first_cong > 93) %>% 
  filter(first_cong < 99) %>% 
  arrange(desc(econ_oppo_per)) %>% 
  select(bioname, party_code, wing, econ_oppo_per, gen, nominate_dim1, nominate_dim2) %>% 
  distinct()

test<- gov_leaderboard %>% 
  filter(first_cong > 93) %>% 
  filter(first_cong < 99) %>% 
  filter(congress_num > 1) %>% 
  arrange(desc(gov_oppo_per)) %>% 
  select(bioname, party_code, wing, gov_oppo_per, gen, nominate_dim1, nominate_dim2) %>% 
  distinct()
