library(dplyr)
library(lpSolve)
library(ggplot2)  

data <- read.csv("supply_chain_data.csv", stringsAsFactors = FALSE)

# Select relevant columns
transport_data <- data %>%
  select(
    SKU,
    Product_type = Product.type,
    Order_quantities = Order.quantities,
    Routes,
    Transportation_modes = Transportation.modes,
    Shipping_costs = Shipping.costs
  )

# Get unique routes and transportation modes
unique_routes <- unique(transport_data$Routes)
unique_modes <- unique(transport_data$Transportation_modes)

# Calculate average shipping costs by route and transportation mode
cost_summary <- transport_data %>%
  group_by(Routes, Transportation_modes) %>%
  summarize(
    Avg_Cost = mean(Shipping_costs),
    Total_Order_Qty = sum(Order_quantities),
    .groups = "drop"
  )

# Calculate current total cost
current_cost_data <- transport_data %>%
  mutate(Total_Cost = Order_quantities * Shipping_costs) %>%
  group_by(Routes, Transportation_modes) %>%
  summarize(
    Order_Quantity = sum(Order_quantities),
    Current_Cost = sum(Total_Cost),
    .groups = "drop"
  )

# Calculate total current cost before optimisation
cat("Total Cost Before optimisation:", sum(current_cost_data$Current_Cost))

# Current distribution by mode (before optimisation
current_mode_distribution <- current_cost_data %>%
  group_by(Transportation_modes) %>%
  reframe(  # Using reframe instead of summarize to avoid the warning
    Total_Quantity = sum(Order_Quantity),
    Total_Cost = sum(Current_Cost)
  )

print(current_mode_distribution)

# Current distribution by route (before optimisation)
current_route_distribution <- current_cost_data %>%
  group_by(Routes) %>%
  reframe(  # Using reframe instead of summarize
    Total_Quantity = sum(Order_Quantity),
    Total_Cost = sum(Current_Cost),
    Avg_Unit_Cost = Total_Cost / Total_Quantity
  )

cat("\nCurrent Route Distribution (Before optimisation):\n")
print(current_route_distribution)

# Create a matrix to store shipping costs by route and mode
cost_matrix <- matrix(0, nrow = length(unique_routes), ncol = length(unique_modes))
rownames(cost_matrix) <- unique_routes
colnames(cost_matrix) <- unique_modes

# Fill the cost matrix with average shipping costs for each route-mode combination
for(route in unique_routes) {
  for(mode in unique_modes) {
    subset_data <- cost_summary %>% 
      filter(Routes == route, Transportation_modes == mode)
    
    if(nrow(subset_data) > 0) {
      cost_matrix[route, mode] <- subset_data$Avg_Cost
    } else {
      # If no data for this combination, set a high cost
      cost_matrix[route, mode] <- 1000
    }
  }
}

# Calculate total order quantities for each route
route_demand <- transport_data %>%
  group_by(Routes) %>%
  summarize(total_demand = sum(Order_quantities))

# Calculate total demand
total_demand <- sum(route_demand$total_demand)

# Set mode capacities
mode_capacity <- c(
  "Air" = total_demand * 0.4,
  "Rail" = total_demand * 0.35,
  "Road" = total_demand * 0.4,
  "Sea" = total_demand * 0.25
)

# Flatten the cost matrix for LP solver
cost_vector <- as.vector(cost_matrix)

# Number of decision variables = routes × modes
num_routes <- length(unique_routes)
num_modes <- length(unique_modes)
num_vars <- num_routes * num_modes

# Route constraints (= demand)
route_constraints <- matrix(0, nrow = num_routes, ncol = num_vars)
for(i in 1:num_routes) {
  for(j in 1:num_modes) {
    route_constraints[i, (i-1)*num_modes + j] <- 1
  }
}

# Mode constraints (<= capacity)
mode_constraints <- matrix(0, nrow = num_modes, ncol = num_vars)
for(i in 1:num_routes) {
  for(j in 1:num_modes) {
    mode_constraints[j, (i-1)*num_modes + j] <- 1
  }
}

# Combine constraints
constraint_matrix <- rbind(route_constraints, mode_constraints)

# Constraint directions
constraint_dir <- c(rep("=", num_routes), rep("<=", num_modes))

# Right-hand side values for constraints
rhs_values <- c(route_demand$total_demand, mode_capacity)

# Solve the linear programming problem
lp_solution <- lp(
  direction = "min",
  objective.in = cost_vector,
  const.mat = constraint_matrix,
  const.dir = constraint_dir,
  const.rhs = rhs_values,
  all.int = TRUE  
)

# Extract solution
solution_matrix <- matrix(lp_solution$solution, nrow = num_routes, ncol = num_modes)
rownames(solution_matrix) <- unique_routes
colnames(solution_matrix) <- unique_modes

# Calculate total cost
total_cost <- sum(solution_matrix * cost_matrix)

cat("\n----- optimisation RESULTS -----\n")
cat("Optimal Transportation Plan:\n")
print(solution_matrix)
cat("\nTotal Minimum Shipping Cost:", total_cost, "\n")
cat("Cost Reduction:", total_current_cost - total_cost, "\n")
cat("Percentage Savings:", round((1 - total_cost/total_current_cost) * 100, 2), "%\n")

# Create a detailed solution table
solution_table <- data.frame()
for(i in 1:num_routes) {
  for(j in 1:num_modes) {
    if(solution_matrix[i,j] > 0) {
      route <- unique_routes[i]
      mode <- unique_modes[j]
      quantity <- solution_matrix[i,j]
      cost <- cost_matrix[i,j] * quantity
      
      solution_table <- rbind(solution_table, data.frame(
        Route = route,
        Mode = mode,
        Assigned_Quantity = quantity,
        Unit_Cost = cost_matrix[i,j],
        Total_Cost = cost
      ))
    }
  }
}

cat("\nDetailed Solution:\n")
print(solution_table)

# Create a summary by route
route_summary <- solution_table %>%
  group_by(Route) %>%
  reframe(  # Using reframe instead of summarize
    Total_Quantity = sum(Assigned_Quantity),
    Total_Cost = sum(Total_Cost),
    Avg_Unit_Cost = Total_Cost / Total_Quantity
  )

cat("\nSummary by Route:\n")
print(route_summary)

# Create a summary by transportation mode
mode_summary <- solution_table %>%
  group_by(Mode) %>%
  reframe(  # Using reframe instead of summarize
    Total_Quantity = sum(Assigned_Quantity),
    Total_Cost = sum(Total_Cost),
    Utilization = Total_Quantity / mode_capacity[Mode] * 100
  )

cat("\nSummary by Transportation Mode:\n")
print(mode_summary)

# ----- VISUALIZATIONS -----

# Cost comparison before and after optimisation
cost_comparison <- data.frame(
  Scenario = c("Before optimisation", "After optimisation"),
  Cost = c(total_current_cost, total_cost)
)

# Create cost comparison bar chart
cost_plot <- ggplot(cost_comparison, aes(x = Scenario, y = Cost, fill = Scenario)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Cost, 2)), vjust = -0.5) +
  labs(title = "Cost Comparison Before vs After optimisation",
       y = "Total Cost", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

print(cost_plot)

# Compare mode distribution before and after optimisation
before_mode <- current_mode_distribution %>%
  select(Mode = Transportation_modes, Quantity = Total_Quantity) %>%
  mutate(Scenario = "Before optimisation")

after_mode <- mode_summary %>%
  select(Mode, Quantity = Total_Quantity) %>%
  mutate(Scenario = "After optimisation")

mode_comparison <- rbind(before_mode, after_mode)

# Create mode distribution comparison
mode_plot <- ggplot(mode_comparison, aes(x = Mode, y = Quantity, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Transportation Mode Usage Before vs After optimisation",
       y = "Quantity", x = "Transportation Mode") +
  theme_minimal()

print(mode_plot)

# Compare route costs before and after optimisation
before_route <- current_route_distribution %>%
  select(Route = Routes, Cost = Total_Cost) %>%
  mutate(Scenario = "Before optimisation")

after_route <- route_summary %>%
  select(Route, Cost = Total_Cost) %>%
  mutate(Scenario = "After optimisation")

route_comparison <- rbind(before_route, after_route)

# Create route cost comparison
route_plot <- ggplot(route_comparison, aes(x = Route, y = Cost, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Cost, 0)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Route Cost Before vs After optimisation",
       y = "Total Cost", x = "Route") +
  theme_minimal()

print(route_plot)

# Create a summary of changes
summary_df <- data.frame(
  Metric = c("Total Cost", "Cost Savings", "Savings Percentage"),
  Value = c(
    total_cost,
    total_current_cost - total_cost,
    round((1 - total_cost/total_current_cost) * 100, 2)
  ),
  Unit = c("$", "$", "%")
)

cat("\noptimisation Summary:\n")
print(summary_df)