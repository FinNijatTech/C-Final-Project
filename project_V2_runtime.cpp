#include <sstream>
#include <iomanip>
#include <stdexcept>
#include <algorithm>
#include <cctype>
#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <random>
#include <chrono>
#include <thread>

/**
 * @brief Represents an option process
 * @details The OptionProcess class is responsible for storing the details of an option, such as the type, direction, strike price, time to maturity, and volume
 */

class OptionProcess {
public:
    std::string LongShortDirection; // Direction of the option (Long/Short)
    std::string CallPutType; // Type of the option (Call/Put)
    double StrikePrice; // Strike price of the option
    double timeToMaturity; // Time to maturity of the option
    int volume; // Quantity of options


    /**
    * @brief Constructor for OptionProcess
    * @param type Type of the option (Call/Put)
    * @param direction Direction of the option (Long/Short)
    * @param strike Strike price of the option
    * @param maturity Time to maturity of the option
    * @param quantity Volume of options
    */

    OptionProcess(const std::string &type, const std::string &direction, double strike, double maturity, int quantity)
        : CallPutType(type), LongShortDirection(direction), StrikePrice(strike), timeToMaturity(maturity), volume(quantity) {}
};


/**
 * @brief Represents the financial model for options pricing
 * @details The Model class is responsible for generating asset prices and calculating the payouts for a given set of options
 */


class Model {
private:
    std::default_random_engine generator;
    std::normal_distribution<double> distribution;
    std::vector<OptionProcess> optionsList;
    std::string outputDirectory;

    double sigma;
    double maturity;
    double spot;
    double mean;

public:

/**
 *  @brief Constructor for the Model class
 *  @param spot Initial spot price
 *  @param mean Expected mean return
 *  @param stddev Standard deviation (volatility)
 *  @param filename Path to the CSV file containing options data
*/

    Model(double spot, double mean, double stddev, const std::string &filename)
        : spot(spot), mean(mean), sigma(stddev) {
        if (!loadOptionsFromCSV(filename)) {
            throw std::runtime_error("Failed to open file: " + filename);
        }

        double meanLog = std::log(spot) + (mean - 0.5 * sigma * sigma) * maturity;
        double sigmaLog = std::sqrt(sigma * sigma * maturity);
        distribution = std::normal_distribution<double>(meanLog, sigmaLog);
        std::random_device rd;
        generator.seed(rd());
    }

    /**
    * @brief Splits a string by a given delimiter
    * @param s The string to split
    *  @param delimiter The delimiter character
    *  @return A vector of tokens
    **/

    std::vector<std::string> splitString(const std::string& s, char delimiter) {
        std::vector<std::string> tokens;
        std::string token;
        std::istringstream tokenStream(s);
        while (std::getline(tokenStream, token, delimiter)) {
            tokens.push_back(token);
        }
        return tokens;
    }

    /**
     *  @brief Loads options data from a CSV file
     * @param filename Path to the CSV file
     * @return True if successful, False otherwise
     **/

    bool loadOptionsFromCSV(const std::string& filename) {
        std::ifstream inputFile(filename);
        if (!inputFile.is_open()) {
            std::cerr << "Failed to open file: " << filename << std::endl;
            return false;
        }

        std::string line;
        int lineNumber = 0;
        while (std::getline(inputFile, line)) {
            ++lineNumber;
            std::vector<std::string> optionData = splitString(line, ',');
            if (optionData.size() == 5) {
                if (lineNumber == 1 && !optionData[0].empty()) {
                    optionData[0] = optionData[0].back();
                    maturity = std::stod(optionData[3]) / 365;
                }
                OptionProcess option(optionData[0], optionData[1], std::stod(optionData[2]),
                            std::stod(optionData[3]), std::stoi(optionData[4]));
                optionsList.push_back(option);
            } else {
                std::cerr << "Invalid data in line " << lineNumber << ": " << line << std::endl;
            }
        }
        inputFile.close();
        return true;
    }

    /**
     * @brief Generates a simulated price based on the log-normal distribution
     * @return A simulated price
     **/


    double generatePrice() {
        return distribution(generator);
    }
    
    /**
     * @brief Generates a list of simulated prices
     * @param numPrices Number of prices to generate
     * @return A vector of simulated prices
     * **/


    std::vector<double> generatePrices(int numPrices) {
        std::vector<double> sampledPrices;
        sampledPrices.reserve(numPrices);
        for (int i = 0; i < numPrices; ++i) {
            double sampledPrice = std::exp(generatePrice());
            sampledPrices.push_back(sampledPrice);
        }
        return sampledPrices;
    }

    /**
     * @brief Calculates the payouts for each option given a list of prices
     * @param prices A vector of simulated asset prices
     * @param numOptions The number of options to consider for payout calculations
     * @return A 2D vector containing the payouts for each option at each simulated price
     * **/

    std::vector<std::vector<double>> calculateOptionPayouts(const std::vector<double>& prices, size_t numOptions) const {
        std::vector<std::vector<double>> payouts;
        payouts.reserve(prices.size());

        for (size_t i = 0; i < prices.size(); ++i) {
            std::vector<double> rowPayouts(numOptions, 0.0);

            for (size_t j = 0; j < numOptions; ++j) {
                const auto& option = optionsList[j];
                double optionPayout = 0.0;

                if (option.CallPutType == "C") {
                    optionPayout = std::max(0.0, prices[i] - option.StrikePrice);
                } else if (option.CallPutType == "P") {
                    optionPayout = std::max(0.0, option.StrikePrice - prices[i]);
                }

                if (option.LongShortDirection == "S") {
                    optionPayout *= -1;
                }

                optionPayout *= option.volume;
                rowPayouts[j] = optionPayout;
            }

            payouts.push_back(rowPayouts);
        }

        return payouts;
    }

    /**
     * @brief Calculates the sum of payouts for each price scenario
     * @param payouts A 2D vector containing the payouts for each option at each simulated price
     * @return A vector containing the sum of payouts for each price scenario
     **/

    std::vector<double> calculateRowSum(const std::vector<std::vector<double>>& payouts) const {
        std::vector<double> rowSums;
        rowSums.reserve(payouts.size());

        for (const auto& row : payouts) {
            double rowSum = 0.0;
            for (const auto& value : row) {
                rowSum += value;
            }
            rowSums.push_back(rowSum);
        }

        return rowSums;
    }

    /**
     * @brief Runs the model simulation and calculates portfolio statistics
     * @param numPrices The number of prices to simulate
     * @param spot The initial spot price
     * @param varianceCutoff The cutoff for variance beyond which processing stops
     * **/

    void runModel(int numPrices, double spot, double varianceCutoff) {
        std::vector<double> priceList = generatePrices(numPrices);
        calculatePortfolioStatistics(optionsList.size(), priceList, varianceCutoff);
    }
    
    /**
     * @brief Calculates portfolio statistics for each option and stops processing if variance exceeds a specified cutoff
     * @param numOptions The number of options to consider
     * @param priceList A vector of simulated asset prices
     * @param varianceCutoff The cutoff for variance beyond which processing stops
     * @return A 2D vector containing statistics (number of options, mean, variance, standard deviation) for each option set
     * **/


    std::vector<std::vector<double>> calculatePortfolioStatistics(size_t numOptions, const std::vector<double>& priceList, double varianceCutoff) {
        std::vector<std::vector<double>> output_matrix;

        std::vector<OptionProcess> partialOptions;
        std::vector<std::vector<double>> optionPayouts;
        double mean, variance, stdDeviation;

        for (size_t i = 0; i < numOptions; ++i) {
            partialOptions.push_back(optionsList[i]);
            optionPayouts = calculateOptionPayouts(priceList, partialOptions.size());

            std::vector<double> portfolioPayoffs(priceList.size(), 0.0);
            for (size_t j = 0; j <= i; ++j) {
                for (size_t k = 0; k < priceList.size(); ++k) {
                    portfolioPayoffs[k] += optionPayouts[k][j];
                }
            }

            mean = 0.0;
            for (size_t k = 0; k < priceList.size(); ++k) {
                mean += portfolioPayoffs[k] / priceList.size();
            }

            double varianceSum = 0.0;
            for (size_t k = 0; k < priceList.size(); ++k) {
                double sum = portfolioPayoffs[k];
                varianceSum += (sum - mean) * (sum - mean);
            }
            variance = varianceSum / priceList.size();
            stdDeviation = std::sqrt(variance);

            if (variance > varianceCutoff) {
                std::cout << "Variance cutoff exceeded. Data saved and processing stopped." << std::endl;
                break; // Stop further processing if variance cutoff exceeded
            }

            output_matrix.push_back({static_cast<double>(partialOptions.size()), mean, variance, stdDeviation});

            std::cout << "Option Processed: " << partialOptions.size()
                      << ", Mean: " << mean // Output statistics for each option set
                      << ", Variance: " << variance     // Output statistics for each option set
                      << ", Standard Deviation: " << stdDeviation << std::endl;
        }

        writeStats(output_matrix, "output_matrix_.csv"); // Write statistics to a CSV file
        return output_matrix;
    }
    /**
     * @brief Writes statistical results (mean, variance, standard deviation) to a CSV file
     * @param output_matrix A 2D vector containing the statistical results
     * @param filename The name of the output CSV file
     * **/

// Write statistical results to a CSV file

    void writeStats(const std::vector<std::vector<double>>& output_matrix, const std::string& filename) {
        std::ofstream outputFile(outputDirectory + "/" + filename);
        if (outputFile.is_open()) {
            outputFile << "OptionCount,Mean,Variance,StdDeviation\n";
            for (size_t i = 0; i < output_matrix.size(); ++i) {
                for (size_t j = 0; j < output_matrix[i].size(); ++j) {
                    outputFile << std::fixed << std::setprecision(6) << output_matrix[i][j];
                    if (j < output_matrix[i].size() - 1) {
                        outputFile << ',';
                    }
                }
                outputFile << "\n";
            }

            outputFile.close();

            std::cout << "Result matrix has been saved to: " << filename << std::endl;
        } else {
            std::cerr << "Failed to open file: " << filename << std::endl;
        }
    }

// Set the output directory for saving files

    void setOutputDirectory(const std::string& directory) {
        outputDirectory = directory;
    }

    std::string getOutputDirectory() const {
        return outputDirectory;
    }
};

int main() {
    try {
        std::string inputFilename;
        std::cout << "Enter the name of the input CSV file (include extension): ";
        std::cin >> inputFilename;

        std::string outputDirectory;
        std::cout << "Enter the directory for output files: ";
        std::cin >> outputDirectory;

        double mean = 0.05;
        double stdDev = 0.2;
        double spot = 5000;
        double varianceCutoff;

        std::cout << "Enter the variance cutoff: ";
        std::cin >> varianceCutoff;

        auto start = std::chrono::high_resolution_clock::now();

        Model financialModel(spot, mean, stdDev, inputFilename);
        financialModel.setOutputDirectory(outputDirectory);
        financialModel.runModel(50000, spot, varianceCutoff);

        auto end = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
        std::cout << "Running time: " << duration.count() << " milliseconds" << std::endl;

    } catch (const std::exception &e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
