
test_conf_1 <- matrix(c(10,2,3,15), nrow = 2, byrow = TRUE)
test_conf_1

test_conf_2 <- matrix(c(8,1,5,16), nrow = 2, byrow = TRUE)
test_conf_2

cost_matrix <- matrix(c(0,100,2, 0), nrow = 2, byrow = TRUE)
cost_matrix

cost_conf_1 <- test_conf_1 * cost_matrix
cost_conf_1
sum(cost_conf_1)

cost_conf_2 <- test_conf_2 * cost_matrix
cost_conf_2
sum(cost_conf_2)