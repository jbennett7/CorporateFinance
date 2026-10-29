# 53. Growing Annuities
# Tom Adams has received a job offer from a large investment bank as a clerk to
# an associate banker. His base salary will be $55,000. He will receive his
# first annual salary payment one year from the day he begins to work. In
# addition, he will get an immediate $10,000 bonus for joining the company. His
# salary will grow at 3.5 percent each year. Each year he will receive a bonus
# equal to 10 percent of his salary. Mr. Adams is expected to work for 25
# years. What is the present value of the offer if the discount rate is 9
# percent?

pvia <- function(r, g=0, t) (1 - ((1 + g) / (1 + r))^t) / (r - g)

10e3 + 55e3*(1.1)*pvia(r=.09, g=.035, t=25)
