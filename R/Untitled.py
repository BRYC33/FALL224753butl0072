import numpy as np
import matplotlib.pyplot as plt

# Define the error function er(u) based on the assignment's given formula
def error_function(u, X, y):
    return np.sum((np.dot(X, u) - y) ** 2)  # Standard least squares

# Local search algorithm
def local_search(u_init, X, y, max_rounds=100):
    u = np.array(u_init)
    n = len(u)
    best_u = u.copy()
    best_error = error_function(best_u, X, y)
    
    errors = [best_error]  # Track errors over rounds

    for round in range(max_rounds):
        improved = False
        
        # Check all neighbors (flip one element at a time)
        for i in range(n):
            new_u = best_u.copy()
            new_u[i] = -new_u[i]  # Flip element i
            new_error = error_function(new_u, X, y)
            
            if new_error < best_error:
                best_error = new_error
                best_u = new_u
                improved = True
        
        errors.append(best_error)

        if not improved:
            break  # Exit if no improvement

    return best_u, best_error, errors

# Plotting the results
def plot_results(errors):
    plt.plot(errors)
    plt.xlabel("Search Rounds")
    plt.ylabel("Error er(u)")
    plt.title("Local Search: Error vs Search Rounds")
    plt.grid(True)
    plt.show()

# Example usage
if __name__ == "__main__":
    X = np.array([[1, 0, 1], [1, 1, 0], [0, 0, 1]])  # Replace with your actual dataset
    y = np.array([1, 0, 1])  # Replace with actual application approval results
    u_initial = [-1, 1, 1]  # Example initial solution (size should match the columns in X)
    
    optimal_u, optimal_error, error_history = local_search(u_initial, X, y, max_rounds=50)

    print("Optimal u:", optimal_u)
    print("Optimal error:", optimal_error)

    # Plot the error history
    plot_results(error_history)

