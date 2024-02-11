#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <cmath>
#include <stdexcept>
#include <string>
#include <random>
#include <thread>
#include <mutex>
#include <functional>



#ifndef M_PI
    #define M_PI 3.14159265358979323846
#endif


class Option {
public:
    enum class Type { Call, Put };
    Type type; // Call or Put
    double strike; // Strike price
    double timeToExpiry; // Time to expiry in years
    double position; // Number of contracts

    Option(Type type, double strike, double timeToExpiry, double position)
        : type(type), strike(strike), timeToExpiry(timeToExpiry), position(position) {}

    double payoff(double S) const {
        if (type == Type::Call) {
            return std::max(S - strike, 0.0);
        } else {
            return std::max(strike - S, 0.0);
        }
    }

    bool operator==(const Option &other) const {
        return type == other.type && strike == other.strike && timeToExpiry == other.timeToExpiry && position == other.position;
    }
};

namespace std {
    template <>
    struct hash<Option> {
        std::size_t operator()(const Option &option) const {
            return hash<int>()(static_cast<int>(option.type)) ^ hash<double>()(option.strike) ^ hash<double>()(option.timeToExpiry) ^ hash<double>()(option.position);
        }
    };
}

#include <cmath>

/**
 * @brief Calculates the cumulative distribution function of the standard normal distribution.
 * @param value The value at which to evaluate the CDF.
 * @return The cumulative distribution function evaluated at the given value.
 */

double normcdf(double value) {
    return 0.5 * erfc(-value * std::sqrt(0.5));
}

/**
 * @brief Calculates the Black-Scholes delta of an option.
 * @details The delta is the rate of change of the option price with respect to the price of the underlying asset.
 * 
 * @param option The option for which to calculate delta.
 * @param S The current price of the underlying asset.
 * @param sigma The volatility of the underlying asset.
 * @param r The risk-free interest rate.
 * @return The Black-Scholes delta of the option.
 */

double blackScholesDelta(const Option& option, double S, double sigma, double r) {
    double T = option.timeToExpiry;
    double K = option.strike;
    double d1 = (std::log(S / K) + (r + 0.5 * sigma * sigma) * T) / (sigma * std::sqrt(T));
    if (option.type == Option::Type::Call) {
        return normcdf(d1);
    } else {
        return -normcdf(-d1);
    }
}

/**
 * @brief Performs a Monte Carlo simulation on a single thread.
 * @details Simulates the payoff of an option at expiration using a geometric Brownian motion model of stock prices.
 * 
 * @param option The option for which to simulate payoffs.
 * @param S0 The initial price of the underlying asset.
 * @param sigma The volatility of the underlying asset.
 * @param r The risk-free interest rate.
 * @param numSimulations The number of simulations to perform.
 * @param results A reference to a vector where the results will be stored.
 * @param resultsMutex A mutex to ensure thread safety when writing to the results vector.
 */

void monteCarloThread(const Option& option, double S0, double sigma, double r, int numSimulations, std::vector<double>& results, std::mutex& resultsMutex) {
    std::vector<double> threadResults;
    threadResults.reserve(numSimulations);
    std::default_random_engine generator(std::random_device{}());
    std::normal_distribution<double> distribution(0.0, 1.0);

    for (int i = 0; i < numSimulations; ++i) {
        double Z = distribution(generator);
        double ST = S0 * exp((r - 0.5 * sigma * sigma) * option.timeToExpiry + sigma * sqrt(option.timeToExpiry) * Z);
        double payoff = option.payoff(ST);
        threadResults.push_back(payoff);
    }

    std::lock_guard<std::mutex> lock(resultsMutex);
    results.insert(results.end(), threadResults.begin(), threadResults.end());
}

/**
 * @brief Runs Monte Carlo simulations using multiple threads.
 * @details This function splits the workload across multiple threads to speed up the simulation process.
 * 
 * @param option The option for which to simulate payoffs.
 * @param S0 The initial price of the underlying asset.
 * @param sigma The volatility of the underlying asset.
 * @param r The risk-free interest rate.
 * @param numSimulations The total number of simulations to run.
 * @param numThreads The number of threads to use for the simulations.
 * @return A vector containing the simulated payoffs for the option.
 */

std::vector<double> monteCarloSimulation(const Option& option, double S0, double sigma, double r, int numSimulations, int numThreads) {
    std::vector<double> simulatedPayoffs;
    std::vector<std::thread> threads;
    std::mutex resultsMutex;

    int simulationsPerThread = numSimulations / numThreads;

    for (int i = 0; i < numThreads; ++i) {
        int start = i * simulationsPerThread;
        int end = (i == numThreads - 1) ? numSimulations : start + simulationsPerThread;
        int simulationsForThread = end - start;

        threads.emplace_back(monteCarloThread, std::cref(option), S0, sigma, r, simulationsForThread, std::ref(simulatedPayoffs), std::ref(resultsMutex));
    }

    for (auto& t : threads) {
        t.join();
    }

    return simulatedPayoffs;
}

/**
 * @brief Calculates statistics from a set of data.
 * @details Computes the mean, variance, and standard deviation of a vector of double values.
 * 
 * @param data A vector of double values to calculate statistics from.
 * @param mean A reference to a double where the mean will be stored.
 * @param variance A reference to a double where the variance will be stored.
 * @param stdDev A reference to a double where the standard deviation will be stored.
 */

void calculateStatistics(const std::vector<double>& data, double& mean, double& variance, double& stdDev) {
    double sum = 0.0;
    double sqSum = 0.0;
    int n = data.size();

    for (double value : data) {
        sum += value;
        sqSum += value * value;
    }
    mean = sum / n;
    variance = (sqSum - sum * sum / n) / (n - 1);
    stdDev = sqrt(variance);
}

/**
 * @class VarianceCalculator
 * @brief This class is used to calculate and manage the variance of options in a portfolio.
 *
 * It allows for adding options, calculating the variance of individual options,
 * and getting statistics on the portfolio payoff.
 */

class VarianceCalculator {

 private:
    std::vector<Option> portfolio; ///< Stores the portfolio of options.
    double portfolioVariance; ///< The cached value of the total portfolio variance.
    std::unordered_map<Option, double> varianceCache; ///< Cache for individual option variances
    const double underlyingValue = 5000; ///< Fixed underlying asset value.
    const double underlyingStdDev = 0.2; ///< Fixed standard deviation of the underlying asset.
    const double annualReturn = 0.05; ///< Fixed annual return rate.

    /**
     * @brief Internal method to calculate the variance of a single option.
     * @param option The option to calculate the variance for.
     * @return The calculated variance of the option.
     */
    
    double calculateOptionVariance(const Option &option) {
        auto cachedVariance = varianceCache.find(option);
        if (cachedVariance != varianceCache.end()) {
            return cachedVariance->second;
        }
        double delta = blackScholesDelta(option, underlyingValue, underlyingStdDev, annualReturn);
        double optionVariance = pow(delta, 2) * pow(underlyingStdDev, 2);
        varianceCache[option] = optionVariance;
        return optionVariance;
    }

   
public:
    /**
     * @brief Constructs a new VarianceCalculator instance.
     */

    VarianceCalculator() : portfolioVariance(0) {}
    /**
     * @brief Adds an option to the portfolio and updates the total variance.
     * @param option The Option object to add, which contains type, strike price, time to expiry, and position.
     */

    void addOption(const Option &option)
    
    /**
     * @brief Retrieves an option from the portfolio by index.
     * @param index The index of the option in the portfolio to retrieve.
     * @return The Option object at the specified index.
     * @throws std::out_of_range If the index is invalid (e.g., negative or beyond the size of the portfolio).
     */
     {
        portfolio.push_back(option);
        portfolioVariance += pow(option.position, 2) * calculateOptionVariance(option);
    }

    Option getOptionByIndex(int index)
        /**
     * @brief Checks if the portfolio is empty.
     * @return True if the portfolio is empty, false otherwise.
     */
     {
        if (index < 0 || index >= static_cast<int>(portfolio.size())) {
            std::ostringstream msg;
            msg << "Index " << index << " is out of range. Valid range is 0 to " << (portfolio.size() - 1);
            throw std::out_of_range(msg.str());
        }
        return portfolio[index];
    }

    bool isEmpty() const {
        return portfolio.empty();
    }

    /**
     * @struct VarianceInfo
     * @brief Holds variance-related information for an option.
     */

 struct VarianceInfo {
        double currentVariance; ///< The current variance of the portfolio.
        double newVariance; ///< The new variance of the portfolio if the option is purchased.
        double mean; ///< The mean of the simulated payoffs.
        double variance; ///< The variance of the simulated payoffs.
        double stdDev; ///< The standard deviation of the simulated payoffs.
    };

    /**
     * @brief Calculates the impact on portfolio variance if a given option is purchased.
     * @param option The Option object for which the variance is calculated.
     * @return A VarianceInfo struct containing the current and new variance, along with statistics of the simulated payoffs.
     */

    VarianceInfo getVarianceIfPurchased(const Option &option) {
        VarianceInfo info;
        info.currentVariance = portfolioVariance;
        double newOptionVariance = calculateOptionVariance(option);
        info.newVariance = portfolioVariance + pow(option.position, 2) * newOptionVariance;

        auto simulatedPayoffs = monteCarloSimulation(option, underlyingValue, underlyingStdDev, annualReturn, 10000, 100);
        calculateStatistics(simulatedPayoffs, info.mean, info.variance, info.stdDev);

        return info;
    }

     /**
     * @brief Calculates portfolio payoff statistics after adding each option.
     * @param numSimulations The number of Monte Carlo simulations to run.
     * @param numThreads The number of threads to use for the Monte Carlo simulation.
     * @return A tuple containing the mean, variance, and standard deviation of the payoffs.
     */

    std::tuple<double, double, double> calculatePortfolioPayoffStatistics(int numSimulations, int numThreads) {
        std::vector<double> allPayoffs;
        for (const auto& option : portfolio) {
            auto payoffs = monteCarloSimulation(option, underlyingValue, underlyingStdDev, annualReturn, numSimulations, numThreads);
            allPayoffs.insert(allPayoffs.end(), payoffs.begin(), payoffs.end());
        }
        double mean, variance, stdDev;
        calculateStatistics(allPayoffs, mean, variance, stdDev);
        return {mean, variance, stdDev};
    }
};

void readOptionsFromCSV(const std::string& filename, VarianceCalculator& calculator, std::ofstream& outputFile) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file " + filename);
    }
    std::string line;
    int optionCount = 0;
    while (std::getline(file, line)) {
        std::istringstream s(line);
        std::string field;
        std::vector<std::string> fields;
        while (getline(s, field, ',')) {
            fields.push_back(field);
        }
        if (fields.size() == 5) {
            Option::Type type = (fields[0] == "C") ? Option::Type::Call : Option::Type::Put;
            double strike = std::stod(fields[2]);
            double timeToExpiry = std::stod(fields[3]);
            double position = std::stod(fields[4]);
            Option option(type, strike, timeToExpiry, position);
            calculator.addOption(option);
            optionCount++;
            
            // Calculate and write statistics after adding each option
            auto [mean, variance, stdDev] = calculator.calculatePortfolioPayoffStatistics(10000, 100); // Adjust simulation parameters as needed
            outputFile << "After adding option " << optionCount << ":\n";
            outputFile << "Portfolio Payoff Statistics:\n";
            outputFile << "Mean Payoff: " << mean << "\n";
            outputFile << "Variance: " << variance << "\n";
            outputFile << "Standard Deviation: " << stdDev << "\n\n";
        }
    }
}

/**
 * @file main.cpp
 * @brief Entry point for the Option Portfolio Variance Calculation program.
 * 
 * This program reads option data from a CSV file, adds them to a portfolio,
 * calculates the variance for the portfolio, and writes statistics to an output file.
 * It utilizes classes and functions defined for option pricing and variance calculation.
 */

int main() {
    // Initialize the variance calculator.
    VarianceCalculator calculator;
    
    // Define file paths for the CSV input and the output text file.
    std::string filePath = "D:\\Documents\\GitHub\\C-Final-Project\\sample_large.csv"; // Update this path
    std::string outputPath = "D:\\Documents\\GitHub\\C-Final-Project\\output_large2.txt"; // Update this path
    
    // Open the output file in append mode.
    std::ofstream outputFile(outputPath, std::ios::app); 
    if (!outputFile.is_open()) {
        std::cerr << "Failed to open output file at: " << outputPath << std::endl;
        return 1; // Error opening the output file
    }

    // Read options from a CSV file and calculate portfolio statistics.
      try {
        readOptionsFromCSV(filePath, calculator, outputFile);
        std::cout << "Finished processing options from CSV." << std::endl;
    } catch (const std::exception& e) {
        std::cerr << "An error occurred: " << e.what() << std::endl;
        outputFile.close();
        return 1; // Indicates an error occurred
    }

    // Close the output file and complete the program
    outputFile.close();
    std::cout << "Program completed successfully. Output written to " << outputPath << std::endl;
    return 0; // Indicates successful execution
}
