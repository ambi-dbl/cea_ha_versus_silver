

#######################################################################
# in the following, all statistical used for the paper 
# Blunck, D. and Schöffski, O. (2022). Cost-effectiveness of 
#      hyaluronic acid dressings versus silver dressings in chronic 
#      wound patients in a German setting
# is provided. Please contact dominik.blunck@fau.de if you have
# any questions.
#######################################################################

# load packages ####
library(ggplot2) # data visualization
library(rdecision) # decision tree 

library(dplyr) # data management
library(tidyr) # data management



# decision tree ####

## nodes ####
d1 <- DecisionNode$new("d1")

c1 <- ChanceNode$new("c1")
c2 <- ChanceNode$new("c2")

t1 <- LeafNode$new("t1")
t2 <- LeafNode$new("t2")
t3 <- LeafNode$new("t3")
t4 <- LeafNode$new("t4")


## probabilities ####
p.heal_ha <- 0.6068
p.not_heal_ha <- 1 - p.heal_ha
p.heal_silver <- 0.5962
p.not_heal_silver <- 1 - p.heal_silver


## cost ####
weeks <- 12
price_ha <- 19.0750
price_ha_var <- (price_ha * 0.05)^2
price_silver <- 22.7890
price_silver_var <- (price_silver * 0.05)^2
price_change <- 12.1667
price_change_var <- 6.404133333
units_ha <- 2
units_silver <- 2

cost_ha <- weeks * units_ha * (price_ha + price_change)
cost_silver <- weeks * units_silver * (price_silver + price_change)


## edges ####
E <- list(
  Action$new(d1,c1, "HA", cost = cost_ha),
  Reaction$new(c1,t1, p = p.heal_ha, benefit = 1, label = "healed after 12w"), # , benefit = 1
  Reaction$new(c1,t2, p = p.not_heal_ha, label = "not healed after 12w"),
  Action$new(d1,c2, "Silver", cost = cost_silver),
  Reaction$new(c2, t3, p = p.heal_silver, benefit = 1, label = "healed after 12w"), #, benefit = 1
  Reaction$new(c2, t4, p = p.not_heal_silver, label = "not healed after 12w")
)


## tree ####
V <- list(d1, c1,c2, t1,t2,t3,t4)
DT<-DecisionTree$new(V,E)


#pdf("decisiontree.pdf", width = 7, height = 4.5)
DT$draw()
#dev.off()


## find optimal strategies ####
RES <- DT$evaluate()
RES$Payoff <- RES$Benefit-RES$Cost

RES

DT$evaluate(by = "path")


## print results ####
incr_cost <- cost_ha-cost_silver 
incr_efct <- p.heal_ha-p.heal_silver
icer <- incr_cost/incr_efct

print(list(cost_ha = cost_ha, healingrate_ha = p.heal_ha,
           cost_silver = cost_silver, healingrate_silver = p.heal_silver,
           incremental_cost = incr_cost, incremental_healing = incr_efct,
           ICER = icer))



# one-way sensitivity analysis ####

df1 <- read.table(header = TRUE, text = '
Parameter	Lower_Bound	Upper_Bound	UL_Difference
Price_ha_unit	-10568.49057	-6249.622642	4318.867925
Price_silver_unit	-5829.169811	-10988.9434	5159.773585
Units_Ha	-8409.056604	26958.90566	35367.96226
Units_silver	-8409.056604	-127126.5283	118717.4717
healing_HA	250.2414374	-245.0137438	495.2551812
healing_silver	-166.0506706	2684.819277	2850.869948
price_change	-8409.056604	-8409.056604	0
')


## tornado plot ####
# adapted from Kikoralston (2019). https://stackoverflow.com/questions/55751978/tornado-both-sided-horizontal-bar-plot-in-r-with-chart-axes-crosses-at-a-given/55755671


# original value of output
base.value <- -8409.056604

# get order of parameters according to size of intervals
order.parameters <- df1 %>% arrange(UL_Difference) %>%
  mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
  select(Parameter) %>% unlist() %>% levels()

# width of columns in plot (value between 0 and 1)
width <- 0.95

# get data frame in shape for ggplot and geom_rect
df1.2 <- df1 %>% 
  # gather columns Lower_Bound and Upper_Bound into a single column using gather
  gather(key='type', value='output.value', Lower_Bound:Upper_Bound) %>%
  # just reordering columns
  select(Parameter, type, output.value, UL_Difference) %>%
  # create the columns for geom_rect
  mutate(Parameter=factor(Parameter, levels=order.parameters),
         ymin=pmin(output.value, base.value),
         ymax=pmax(output.value, base.value),
         xmin=as.numeric(Parameter)-width/2,
         xmax=as.numeric(Parameter)+width/2)


#pdf("owsa.pdf1", width = 7, height = 4.5)
ggplot() + 
  geom_rect(data = df1.2, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
  theme_bw() + 
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = base.value) +
  scale_x_continuous(breaks = c(1:length(order.parameters)), 
                     labels = c("Price dressing change","Healing rate HA","Healing rate silver","Price HA","Price silver","Units per week HA","Units per week silver")) +
  scale_fill_discrete(labels = c("Low values", "High values")) +
  labs(x = "Inremental cost-effectiveness ratio", title = "Tornado diagram", subtitle = "One-way sensitivity analysis", y = "") +
  coord_flip() +
  annotate("text", y = -30000, x = 1.0, label = "Baseline:\n-8409.06", size = 3, color = "grey20")
#dev.off()





# probabilistic sensitivity analysis ####

WTP = 20
n = 10000


data = data.frame(c(seq(1,n)))

set.seed(1)

## incremental effectiveness ####
data$effect_ha <- rbeta(n=n, shape1 = 2.03, shape2 = 1.295979)
data$effect_sv <- rbeta(n=n, shape1 = 1.82, shape2 = 1.432298)

data$incr_effectiveness = data$effect_ha - data$effect_sv


## costs ####
data$units_ha = rbinom(1000, size = 8, prob = 0.18) + 1
data$units_sv = rbinom(1000, size = 8, prob = 0.23) + 1

data$price_ha = rgamma(n=n, shape = price_ha/(price_ha_var/price_ha), scale = price_ha_var/price_ha) 
data$price_sv = rgamma(n=n, shape = price_silver/(price_silver_var/price_silver), scale = price_silver_var/price_silver) 

data$price_dressing_change = rgamma(n=n, shape = price_change/(price_change_var/price_change), scale = price_change_var/price_change) 

data$costs_ha = data$units_ha * (data$price_ha + data$price_dressing_change) 
data$costs_sv = data$units_sv * (data$price_sv + data$price_dressing_change)

data$total_cost_ha = data$costs_ha * 12
data$total_cost_sv = data$costs_sv * 12

## incremental cost ####
data$incr_cost = data$total_cost_ha - data$total_cost_sv


## ICER ####
data$icer = data$incr_cost/data$incr_effectiveness


data$model = WTP * data$incr_effectiveness
data$model_true = data$model - data$incr_cost

data$CE = ifelse(test = data$model_true > 0, yes = 1, no = 0)


## PSA plot ####

#pdf("psa.pdf", width = 7, height = 4.5)

ggplot(data, aes(x = incr_effectiveness, y = incr_cost)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(size = .1, color = "grey20") +
  labs(title = "Incremental cost-effectiveness", subtitle = "Probabilistic sensitivity analysis",
       x = "Incremental effectiveness", y = "Incremental cost") +
  xlim(-1,1) +
  ylim(-2800,2800) +
  theme_bw()

#dev.off()



## CE Acceptability Curve (CEAC) ####

wtp <- seq(0,10500, by = 300)
n <- length(wtp)
i <- 1
pce <- vector("numeric", length = n)

df <- data.frame(wtp,pce)


for(i in 1:n){
  #print(wtp[i])
  data$model = wtp[i] * data$incr_effectiveness
  data$model_true = data$model - data$incr_cost
  data$CE = ifelse(test = data$model_true > 0, yes = 1, no = 0)
  #print(mean(data$CE))
  df[i, "pce"] <- mean(data$CE)
}

df$pce_inv <- 1-df$pce
df <- df %>% gather("grp", "pce", 2:3)


#pdf("ceac.pdf", width = 7, height = 4.5)

ggplot(df, aes(x = wtp, y = pce, color = grp, shape = grp)) +
  geom_line(size = 0.5) +
  geom_point(size = 2.5) +
  labs(x = "Willingness to pay",
       y = "Probability of intervention is cost-effective",
       title = "Cost-effectiveness acceptability curve") +
  scale_color_discrete(name = "Group", labels = c("HA", "Silver")) +
  scale_shape_discrete(name = "Group", labels = c("HA", "Silver")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = seq(0,1, by = 0.1))+
  scale_x_continuous(expand = c(0,0), limits = c(0,10500))+
  theme_bw() +
  theme(legend.position = "bottom")

#dev.off()








