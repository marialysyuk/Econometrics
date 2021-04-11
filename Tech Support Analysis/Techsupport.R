library("dplyr")
library("lubridate")
library("data.table")
library("tidyverse")
library("ggpubr")
library("lmtest")
library("stargazer")
library("erer")

# Чтение данных -----------------------------------------------------------

train_df <- fread('train.csv', header = TRUE)
test_df <- fread('test.csv', header = TRUE)
sample <- fread('sample_submission', header = TRUE) 

#test <- satisfaction %>% filter(request_id != "") %>%
#  group_by(request_id, result_mentioned_by_user) %>% summarise(n = n()) %>% filter(n > 1) 


# Преобразование начальных данных -----------------------------------------

satisfaction_fin <- satisfaction %>% filter(request_id != "") %>% unique()
support_base <- support_tickets %>% left_join(satisfaction_fin)
part2 <- support_base %>% select(-current_state, -request_id) %>% arrange(user_id, activity_start_dt, fact_reaction_dt)
part1 <- requests %>% select(user_id, item_id, item_starttime)


# Добавление difference + числовая шкала для satisfaction -----------------


part2_feedback <- part2 %>% filter(result_mentioned_by_user != 'NA') %>%
  mutate(difference = round(difftime(fact_reaction_dt, activity_start_dt, units = "hours"),2)) %>%
  mutate(result_mentioned_by_user = recode(result_mentioned_by_user, 
                                         'Отлично' = 5,
                                         'Хорошо' = 4,
                                         'Нейтрально' = 3,
                                         'Удовлетворительно' = 2,
                                         'Не удовлетворительно' = 1))


# Analysis by months ---------------------------------------------------------------

#part2_feedback_sep <- part2_feedback %>% dplyr::filter(fact_reaction_dt > "2015-08-30",
#                                                      fact_reaction_dt < "2015-10-01") 

#part2_feedback_sep <- part2_feedback %>% dplyr::filter(fact_reaction_dt > "2015-09-30",
#                                                      fact_reaction_dt < "2015-11-01") 


#part2_feedback_sep <- part2_feedback %>% dplyr::filter(fact_reaction_dt > "2015-10-31",
#                                                       fact_reaction_dt < "2015-12-01") 

part2_feedback_sep <- part2_feedback %>% dplyr::filter(fact_reaction_dt > "2015-11-30",
                                                       fact_reaction_dt < "2016-01-01") 

part2_feedback_sep <- part2_feedback_sep %>% mutate(dummy_neg= ifelse(result_mentioned_by_user < 4, 1, 0),
                                                    dummy_pos = ifelse(result_mentioned_by_user > 3, 1, 0))   

part2_feedback_sep <- part2_feedback_sep %>% group_by(user_id, ticket_category, fact_reaction_dt) %>%
  summarise(feedback_avg= mean(result_mentioned_by_user), neg_sum = sum(result_mentioned_by_user < 4),
            neg_dumm = max(dummy_neg), pos_dumm=max(dummy_pos), time= mean(difference))

part2_feedback_sep_lastrow <- part2_feedback_sep %>% group_by(user_id) %>%
  filter(row_number() == n()) 

#difference=aug-nov
   
part1_sep <- part1 %>% filter(user_id %in% part2_feedback_sep_lastrow$user_id)

#part1_sep <- part1_sep %>% filter(item_starttime > "2015-07-31", item_starttime < "2015-11-01")
#part1_sep <- part1_sep %>% filter(item_starttime > "2015-08-31", item_starttime < "2015-12-01")
#part1_sep <- part1_sep %>% filter(item_starttime > "2015-09-30", item_starttime < "2016-01-01")
part1_sep <- part1_sep %>% filter(item_starttime > "2015-10-31", item_starttime < "2016-02-01")

joined_sep <- part1_sep %>% inner_join(part2_feedback_sep_lastrow)
joined_sep <- joined_sep %>% mutate(dummy_before = ifelse(item_starttime < fact_reaction_dt, 1, 0))
joined_frq <- joined_sep %>% group_by(user_id) %>% summarise(posts = n())
joined_before <- joined_sep %>% group_by(user_id) %>% summarise(before = sum(dummy_before))

joined_sep<- part2_feedback_sep_lastrow %>% inner_join(joined_frq)
joined_sep<- joined_sep %>% inner_join(joined_before)

joined_sep <- joined_sep %>% mutate(after=posts-before) %>% mutate(diff = before-after)
joined_sep$ticket_category<- factor(joined_sep$ticket_category)

joined_sep <- joined_sep %>% filter(before != 0)

joined_sep_left<- joined_sep%>%filter(after==0)

mean(joined_sep$before)
mean(joined_sep$after)
t.test(joined_sep$before, joined_sep$after, alternative = 'greater', paired= TRUE)

#regressions
model0 <- lm(data=joined_sep, diff~ticket_category+ feedback_avg+time)
model1 <- lm(data=joined_sep, diff~ticket_category+ neg_dumm+neg_sum+time)
model2 <- lm(data=joined_sep, diff~ticket_category+ neg_dumm+time)
model20 <- rlm(data=joined_sep, diff~ticket_category+ neg_dumm+time)
model3 <- lm(data=joined_sep, diff~ticket_category+ neg_sum+time)
model30<- rlm(data=joined_sep, diff~ticket_category+ neg_sum+time)

model2_sep <- lm(data=joined_sep, diff~ticket_category+ neg_dumm+time)
model3_sep <- lm(data=joined_sep, diff~ticket_category+ neg_sum+time)
model2_oct <- lm(data=joined_sep, diff~ticket_category+ neg_dumm+time)
model3_oct <- lm(data=joined_sep, diff~ticket_category+ neg_sum+time)
model2_nov <- lm(data=joined_sep, diff~ticket_category+ neg_dumm+time)
model3_nov <- lm(data=joined_sep, diff~ticket_category+ neg_sum+time)
model2_dec <- lm(data=joined_sep, diff~ticket_category+ neg_dumm+time)
model3_dec <- lm(data=joined_sep, diff~ticket_category+ neg_sum+time)

#october prediction
part1_oct <- part1 %>% dplyr::filter(item_starttime > "2015-09-30",item_starttime < "2015-11-01") %>%
  group_by(user_id) %>% summarise(n=n())
joined_sep <- part2_feedback_sep_lastrow %>% left_join(part1_oct)  
joined_sep[is.na(joined_sep)] <- 0
joined_sep$ticket_category.f <- factor(joined_sep$ticket_category)

model0 <- lm(data=joined_sep, n~ticket_category.f+ feedback_avg+time)
model1 <- lm(data=joined_sep, n~ticket_category.f+ neg_dumm+neg_sum+neg_dumm+time)
model2 <- lm(data=joined_sep, n~ticket_category.f+ neg_dumm+time)
model3 <- lm(data=joined_sep, n~ticket_category.f+ neg_sum+time)


#out of posting

#part2_feedback_sep <- part2_feedback %>% dplyr::filter(fact_reaction_dt > "2015-08-30",
#                                                      fact_reaction_dt < "2015-10-01") 

#part2_feedback_sep <- part2_feedback %>% dplyr::filter(fact_reaction_dt > "2015-09-30",
#                                                      fact_reaction_dt < "2015-11-01") 


#part2_feedback_sep <- part2_feedback %>% dplyr::filter(fact_reaction_dt > "2015-10-31",
#                                                       fact_reaction_dt < "2015-12-01") 

part2_feedback_sep <- part2_feedback %>% dplyr::filter(fact_reaction_dt > "2015-11-30",
                                                       fact_reaction_dt < "2016-01-01") 

part2_feedback_sep <- part2_feedback_sep %>% mutate(dummy_neg= ifelse(result_mentioned_by_user < 4, 1, 0),
                                                    dummy_pos = ifelse(result_mentioned_by_user > 3, 1, 0))   

part2_feedback_sep <- part2_feedback_sep %>% group_by(user_id, ticket_category, fact_reaction_dt) %>%
  summarise(feedback_avg= mean(result_mentioned_by_user), neg_sum = sum(result_mentioned_by_user < 4),
            neg_dumm = max(dummy_neg), pos_dumm=max(dummy_pos), time= mean(difference))

part2_feedback_sep_lastrow <- part2_feedback_sep %>% group_by(user_id) %>%
  filter(row_number() == n()) 



part1_sep <- part1 %>% filter(user_id %in% part2_feedback_sep_lastrow$user_id)

#part1_sep <- part1_sep %>% filter(item_starttime > "2015-07-31", item_starttime < "2015-11-01")
#part1_sep <- part1_sep %>% filter(item_starttime > "2015-08-31", item_starttime < "2015-12-01")
#part1_sep <- part1_sep %>% filter(item_starttime > "2015-09-30", item_starttime < "2016-01-01")
part1_sep <- part1_sep %>% filter(item_starttime > "2015-10-31", item_starttime < "2016-02-01")

joined_sep <- part1_sep %>% inner_join(part2_feedback_sep_lastrow)
joined_sep <- joined_sep %>% mutate(dummy_before = ifelse(item_starttime < fact_reaction_dt, 1, 0))
joined_frq <- joined_sep %>% group_by(user_id) %>% summarise(posts = n())
joined_before <- joined_sep %>% group_by(user_id) %>% summarise(before = sum(dummy_before))

joined_sep<- part2_feedback_sep_lastrow %>% inner_join(joined_frq)
joined_sep<- joined_sep %>% inner_join(joined_before)

joined_sep <- joined_sep %>% mutate(after=posts-before) %>% mutate(diff = before-after)
joined_sep$ticket_category.f <- factor(joined_sep$ticket_category)

joined_sep <- joined_sep %>% filter(before != 0)

joined_sep <- joined_sep %>% mutate(dummy_out= ifelse(after == 0, 1, 0))

#regressions

logit0_sep <- glm(data=joined_sep, dummy_out~ticket_category.f+ neg_dumm+time, 
              family=binomial(link="logit"), x=TRUE)
logit1_sep <- glm(data=joined_sep, dummy_out~ticket_category.f+ neg_sum+time, 
              family=binomial(link="logit"), x=TRUE)
logit0_oct <- glm(data=joined_sep, dummy_out~ticket_category.f+ neg_dumm+time, 
                  family=binomial(link="logit"), x=TRUE)
logit1_oct <- glm(data=joined_sep, dummy_out~ticket_category.f+ neg_sum+time, 
                  family=binomial(link="logit"), x=TRUE)
logit0_nov <- glm(data=joined_sep, dummy_out~ticket_category.f+ neg_dumm+time, 
                  family=binomial(link="logit"), x=TRUE)
logit1_nov <- glm(data=joined_sep, dummy_out~ticket_category.f+ neg_sum+time, 
                  family=binomial(link="logit"), x=TRUE)
logit0_dec <- glm(data=joined_sep, dummy_out~ticket_category.f+ neg_dumm+time, 
                  family=binomial(link="logit"), x=TRUE)
logit1_dec <- glm(data=joined_sep, dummy_out~ticket_category.f+ neg_sum+time, 
                  family=binomial(link="logit"), x=TRUE)

#предельные эффекты
mab_oct <- maBina(logit0_oct, x.mean=FALSE)

# Latex -------------------------------------------------------------------

stargazer(mab_sep, mab_oct, mab_nov, mab_dec, align=TRUE, title="Results")
