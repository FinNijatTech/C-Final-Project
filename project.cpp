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

// Option class and other definitions remain the same...


class Option {
public:
    enum class Type { Call, Put };
    Type type;
    double strike;
    double timeToExpiry;
    double position;

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

double normcdf(double value) {
    return 0.5 * erfc(-value * std::sqrt(0.5));
}

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

// Utility function to perform Monte Carlo simulations in a thread
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

class VarianceCalculator {
    // Existing private members...
 private:
    std::vector<Option> portfolio;
    double portfolioVariance;
    std::unordered_map<Option, double> varianceCache;
    const double underlyingValue = 5000;
    const double underlyingStdDev = 0.2;
    const double annualReturn = 0.05;


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
    // Constructor and other public members remain the same...
    VarianceCalculator() : portfolioVariance(0) {}

    void addOption(const Option &option) {
        portfolio.push_back(option);
        portfolioVariance += pow(option.position, 2) * calculateOptionVariance(option);
    }

    Option getOptionByIndex(int index) {
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


 struct VarianceInfo {
        double currentVariance;
        double newVariance;
        double mean;
        double variance;
        double stdDev;
    };

    VarianceInfo getVarianceIfPurchased(const Option &option) {
        VarianceInfo info;
        info.currentVariance = portfolioVariance;
        double newOptionVariance = calculateOptionVariance(option);
        info.newVariance = portfolioVariance + pow(option.position, 2) * newOptionVariance;

        auto simulatedPayoffs = monteCarloSimulation(option, underlyingValue, underlyingStdDev, annualReturn, 10000, 100);
        calculateStatistics(simulatedPayoffs, info.mean, info.variance, info.stdDev);

        return info;
    }
    // This method calculates portfolio payoff statistics after adding each option
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

int main() {
    VarianceCalculator calculator;
    std::string filePath = "D:\\Documents\\GitHub\\C-Final-Project\\sample_large.csv"; // Update this path
    std::string outputPath = "D:\\Documents\\GitHub\\C-Final-Project\\output.txt"; // Update this path
    std::ofstream outputFile(outputPath, std::ios::app); // Open in append mode
    if (!outputFile.is_open()) {
        std::cerr << "Failed to open output file at: " << outputPath << std::endl;
        return 1; // Error opening the output file
    }

    try {
        readOptionsFromCSV(filePath, calculator, outputFile);
        std::cout << "Finished processing options from CSV." << std::endl;
    } catch (const std::exception& e) {
        std::cerr << "An error occurred: " << e.what() << std::endl;
        outputFile.close();
        return 1; // Indicates an error occurred
    }

    outputFile.close();
    std::cout << "Program completed successfully. Output written to " << outputPath << std::endl;
    return 0; // Indicates successful execution
}
