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

class OptionProcess {
public:
    std::string LSdirection;
    std::string CPtype;
    double STR;
    double timeToMaturity;
    int quantity;

    OptionProcess(const std::string &t, const std::string &d, double s, double ttm, int q)
        : CPtype(t), LSdirection(d), STR(s), timeToMaturity(ttm), quantity(q) {}
};

class Model {
private:
    std::default_random_engine generator;
    std::normal_distribution<double> distribution;
    std::vector<OptionProcess> optionsList;
    std::string outputDirectory;

    double sigma;
    double ttm;
    double spot;
    double mean;

public:
    Model(double spotPrice, double meanReturn, double stddev, const std::string &filename)
        : spot(spotPrice), mean(meanReturn), sigma(stddev) {
        if (!loadOptionsFromCSV(filename)) {
            throw std::runtime_error("Failed to open file: " + filename);
        }

        double meanLog = std::log(spot) + (mean - 0.5 * sigma * sigma) * ttm;
        double sigmaLog = std::sqrt(sigma * sigma * ttm);
        distribution = std::normal_distribution<double>(meanLog, sigmaLog);
        std::random_device rd;
        generator.seed(rd());
    }

    // Function to split string by delimiter
    std::vector<std::string> splitString(const std::string& s, char delimiter) {
        std::vector<std::string> tokens;
        std::string token;
        std::istringstream tokenStream(s);
        while (std::getline(tokenStream, token, delimiter)) {
            tokens.push_back(token);
        }
        return tokens;
    }

    // Function to load options from CSV file
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
                    ttm = std::stod(optionData[3]) / 365;
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

    // Function to generate a random price
    double generatePrice() {
        return distribution(generator);
    }

    // Function to generate prices
    std::vector<double> generatePrices(int numPrices) {
        std::vector<double> sampledPrices;
        sampledPrices.reserve(numPrices);
        for (int i = 0; i < numPrices; ++i) {
            double sampledPrice = std::exp(generatePrice());
            sampledPrices.push_back(sampledPrice);
        }
        return sampledPrices;
    }

    // Function to calculate option payouts
    std::vector<std::vector<double>> calculateOptionPayouts(const std::vector<double>& prices, size_t numOptions) const {
        std::vector<std::vector<double>> payouts;
        payouts.reserve(prices.size());

        for (size_t i = 0; i < prices.size(); ++i) {
            std::vector<double> rowPayouts(numOptions, 0.0);

            for (size_t j = 0; j < numOptions; ++j) {
                const auto& option = optionsList[j];
                double optionPayout = 0.0;

                if (option.CPtype == "C") {
                    optionPayout = std::max(0.0, prices[i] - option.STR);
                } else if (option.CPtype == "P") {
                    optionPayout = std::max(0.0, option.STR - prices[i]);
                }

                if (option.LSdirection == "S") {
                    optionPayout *= -1;
                }

                optionPayout *= option.quantity;
                rowPayouts[j] = optionPayout;
            }

            payouts.push_back(rowPayouts);
        }

        return payouts;
    }

    // Function to calculate row sums
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

    // Function to write combined data to a CSV file
    void writeCombined(const std::vector<double>& assetPrices,
                   const std::vector<std::vector<double>>& optionPayouts,
                   const std::vector<OptionProcess>& optionsList,
                   const std::string& filename, size_t numOptions) {
        std::ofstream outputFile(outputDirectory + "/" + filename);
        if (outputFile.is_open()) {
            outputFile << "Asset_Prices";
            for (size_t j = 0; j < numOptions; ++j) {
                const auto& option = optionsList[j];
                outputFile << ',' << option.CPtype << '|' << option.LSdirection << '|' << option.STR
                           << '|' << option.timeToMaturity << '|' << option.quantity;
            }
            outputFile << "\n";

            for (size_t i = 0; i < assetPrices.size(); ++i) {
                outputFile << std::fixed << std::setprecision(2) << assetPrices[i];
                for (size_t j = 0; j < numOptions; ++j) {
                    outputFile << ',' << optionPayouts[i][j];
                }
                outputFile << "\n";
            }

            outputFile.close();
            std::cout << "Data has been saved in: " << filename << std::endl;
        } else {
            std::cerr << "Failed to open file: " << filename << std::endl;
        }
    }

    // Function to run the model
    void runModel(int numPrices, double spotPrice, double varianceCutoff) {
        std::vector<double> priceList = generatePrices(numPrices);
        calculatePortfolioStatistics(optionsList.size(), priceList, varianceCutoff);
    }

    // Function to calculate portfolio statistics
    std::vector<std::vector<double>> calculatePortfolioStatistics(size_t numOptions, const std::vector<double>& priceList, double varianceCutoff) {
        std::vector<std::vector<double>> output_matrix;

        std::vector<OptionProcess> partialOptions;
        std::vector<std::vector<double>> optionPayouts;
        double mean, variance, stdDeviation;

        bool skip = false;
        for (size_t i = 0; i < numOptions; ++i) {
            if (skip) {
                skip = false;
                continue;
            }

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
                skip = true;
                if (i == numOptions - 1) {
                    writeCombined(priceList, optionPayouts, partialOptions, "merged_table1.csv", partialOptions.size());
                    std::cout << "Variance cutoff exceeded. Data saved." << std::endl;
                }
                continue;
            }

            output_matrix.push_back({static_cast<double>(partialOptions.size()), mean, variance, stdDeviation});

            std::cout << "Option Processed: " << partialOptions.size()
                    << ", Mean: " << mean
                    << ", Variance: " << variance
                    << ", Standard Deviation: " << stdDeviation << std::endl;

            if (i == numOptions - 1) {
                writeCombined(priceList, optionPayouts, partialOptions, "merged_table1.csv", partialOptions.size());
                std::cout << "All options processed. Data saved." << std::endl;
            }
        }

        writeStats(output_matrix, "output_matrix1.csv");
        return output_matrix;
    }

    // Function to write statistics to a CSV file
    void writeStats(const std::vector<std::vector<double>>& output_matrix, const std::string& filename) {
        std::ofstream outputFile(outputDirectory + "\\" + filename); //output path \\ for windows
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

        double meanReturn = 0.05;
        double stdDev = 0.2;
        double spotPrice = 5000;
        double varianceCutoff;

        std::cout << "Enter the variance cutoff: ";
        std::cin >> varianceCutoff;

        Model financialModel(spotPrice, meanReturn, stdDev, inputFilename);
        financialModel.setOutputDirectory(outputDirectory);
        financialModel.runModel(10000, spotPrice, varianceCutoff);
        
    } catch (const std::exception &e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
