#include <iostream>
#include <fstream>
#include <chrono>
#include <vector>
#include <cmath>
#include <random>
#include <sstream>
#include <iomanip>
#include <stdexcept> // Include for std::runtime_error
#include <algorithm> // Include for std::remove_if
#include <cctype>    // Include for std::isspace

class Option
{
public:
    std::string type;      // Call or Put
    std::string direction; // Long or Short
    double strike;
    double timeToMaturity;
    int quantity;

    Option(const std::string &t, const std::string &d, double s, double ttm, int q)
        : type(t), direction(d), strike(s), timeToMaturity(ttm), quantity(q) {}
};

class FinancialModel
{
private:
    double spot;
    double mu; // Annual mean return
    double sigma; // Annual standard deviation
    std::default_random_engine generator;
    std::normal_distribution<double> distribution;
    std::vector<Option> optionsList; // List of options
    double ttm; // Time to maturity

public:
    FinancialModel(double spotPrice, double meanReturn, double stddev, const std::string &filename)
        : spot(spotPrice), mu(meanReturn), sigma(stddev)
    {
        // Load options from input csv
        if (!loadOptionsFromCSV(filename)) {
            throw std::runtime_error("Unable to open file: " + filename);
        }

        // Calculate mu and sigma sq of log price for the normal distribution
        double muLog = std::log(spot) + (mu - 0.5 * sigma * sigma) * ttm;
        double sigmaLog = std::sqrt(sigma * sigma * ttm);

        // Set up the normal distribution
        distribution = std::normal_distribution<double>(muLog, sigmaLog);

        // Seed the random number generator
        std::random_device rd;
        generator.seed(rd());
    }

    // Function to split a string based on a delimiter
    std::vector<std::string> splitString(const std::string& s, char delimiter) {
        std::vector<std::string> tokens;
        std::string token;
        std::istringstream tokenStream(s);
        while (std::getline(tokenStream, token, delimiter)) {
            tokens.push_back(token);
        }
        return tokens;
    }

    // Function to load options from a CSV file
    bool loadOptionsFromCSV(const std::string& filename) {
        std::ifstream inputFile(filename);
        if (!inputFile.is_open()) {
            std::cerr << "Unable to open file: " << filename << std::endl;
            return false;
        }

        std::string line;
        int lineNumber = 0;
        while (std::getline(inputFile, line)) {
            ++lineNumber;

            // Add this line for additional debugging
            std::cout << "Processing line " << lineNumber << ": " << line << std::endl;

            std::vector<std::string> optionData = splitString(line, ',');
            if (optionData.size() == 5) {
                // Modify the first element of the first option
                if (lineNumber == 1 && !optionData[0].empty()) {
                    optionData[0] = optionData[0].back();
                    ttm = std::stod(optionData[3]) / 365;
                    std::cout << "Time to maturity: " << ttm << std::endl;
                }

                Option option(optionData[0], optionData[1], std::stod(optionData[2]),
                            std::stod(optionData[3]), std::stoi(optionData[4]));
                optionsList.push_back(option);
            } else {
                std::cerr << "Error reading line " << lineNumber << ": " << line << std::endl;
            }
        }
        inputFile.close();

        // Debugging statement
        std::cout << "Number of options loaded: " << optionsList.size() << std::endl;

        return true;
    }

    // Function to generate price
    double generatePrice() {
        return distribution(generator);
    }

    // Function to generate a list of prices
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
                const auto& option = optionsList[j]; // Consider only the first 'numOptions' options
                double optionPayout = 0.0;

                if (option.type == "C") {
                    optionPayout = std::max(0.0, prices[i] - option.strike);
                } else if (option.type == "P") {
                    optionPayout = std::max(0.0, option.strike - prices[i]);
                }

                if (option.direction == "S") {
                    optionPayout *= -1;
                }

                optionPayout *= option.quantity;
                rowPayouts[j] = optionPayout;
            }

            // Add rowPayouts to the payouts vector
            payouts.push_back(rowPayouts);
        }

        return payouts;
    }

    // Function to calculate the sum of values per price (row)
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

    // Function to save data to a CSV file
    void writeCombined(const std::vector<double>& assetPrices,
                const std::vector<std::vector<double>>& optionPayouts,
                const std::vector<Option>& optionsList,
                const std::string& filename, size_t numOptions) {
        std::ofstream outputFile(filename);

        if (outputFile.is_open()) {
            // Write column headers
            outputFile << "asset_prices";

            // Write option_payouts headers for the relevant options
            for (size_t j = 0; j < numOptions; ++j) {
                const auto& option = optionsList[j];
                outputFile << ',' << option.type << '|' << option.direction << '|' << option.strike
                        << '|' << option.timeToMaturity << '|' << option.quantity;
            }
            outputFile << "\n";

            // Write data rows
            for (size_t i = 0; i < assetPrices.size(); ++i) {
                // Write asset_prices
                outputFile << std::fixed << std::setprecision(2) << assetPrices[i];

                // Write option_payouts columns for the relevant options
                for (size_t j = 0; j < numOptions; ++j) {
                    outputFile << ',' << optionPayouts[i][j];
                }

                outputFile << "\n";
            }

            outputFile.close();
            std::cout << "Data has been saved to: " << filename << std::endl;
        } else {
            std::cerr << "Unable to open file: " << filename << std::endl;
        }
    }

    // Function to save result matrix to a CSV file
    void writeStats(const std::vector<std::vector<double>> &resultMatrix, const std::string &filename)
    {
        std::ofstream outputFile(filename);

        if (outputFile.is_open())
        {
            // Write column headers
            outputFile << "OptionCount,Mean,Variance,StdDeviation\n";

            // Write data rows
            for (size_t i = 0; i < resultMatrix.size(); ++i)
            {
                for (size_t j = 0; j < resultMatrix[i].size(); ++j)
                {
                    outputFile << std::fixed << std::setprecision(6) << resultMatrix[i][j];
                    if (j < resultMatrix[i].size() - 1)
                    {
                        outputFile << ',';
                    }
                }
                outputFile << "\n";
            }

            outputFile.close();
            std::cout << "Result matrix has been saved to: " << filename << std::endl;
        }
        else
        {
            std::cerr << "Unable to open file: " << filename << std::endl;
        }
    }

    void runModel(int numPrices, double spotPrice)
    {
        // Generate prices (moved outside the loop)
        std::vector<double> priceList = generatePrices(numPrices);

        // Calculate and store option count, mean, variance, and standard deviation
        std::vector<std::vector<double>> resultMatrix = calculatePortfolioStatistics(optionsList.size(), priceList);
    }

    std::vector<std::vector<double>> resultMatrixAddZeroes(std::vector<std::vector<double>> resultMatrixInput) {
        std::vector<double> initialRow{0, 0, 0, 0};
        resultMatrixInput.push_back(initialRow);

        return resultMatrixInput;
    }

private:
    // Function to calculate and store option count, mean, variance, and standard deviation
    std::vector<std::vector<double>> calculatePortfolioStatistics(size_t numOptions, const std::vector<double> &priceList) {
        std::vector<std::vector<double>> resultMatrix;

        // Process 'sequential' run
        std::string runType = "sequential";

        double varianceCutoff = 0.0;
        bool resetTrigger = false;  

        resultMatrix = resultMatrixAddZeroes(resultMatrix);

        // Print the initial row of statistics
        std::cout << "Option Count: " << 0
                << ", Mean: " << 0
                << ", Variance: " << 0
                << ", Standard Deviation: " << 0 << std::endl;

        for (size_t i = 0; i < numOptions; ++i) {
            if (resetTrigger == true) {
                i = 0;
                resultMatrix.clear();
                resultMatrix = resultMatrixAddZeroes(resultMatrix);
                resetTrigger = false;
            }

            // Check if the run is 'sequential' and prompt the user to continue
            if (runType == "sequential") {
                if (i == 0) {
                    std::cout << "Please input variance cutoff: ";
                    std::cin >> varianceCutoff;
                }

                char continueOption;
                std::cout << "Do you want to continue with the next option? (y/n/r): ";
                std::cin >> continueOption;

                if (continueOption == 'r') {
                    resetTrigger = true;
                }
                else if (continueOption == 'n') {
                    // Break the loop if the user chooses not to continue
                    std::vector<Option> partialOptionsOut(optionsList.begin(), optionsList.begin() + i);
                    std::vector<std::vector<double>> optionPayoutsOut = calculateOptionPayouts(priceList, numOptions);
                    writeCombined(priceList, optionPayoutsOut, partialOptionsOut, "combined_table.csv", partialOptionsOut.size());
                    break;
                }
            }

            if (resetTrigger == false ) {
                std::vector<Option> partialOptions(optionsList.begin(), optionsList.begin() + i + 1);

                // Calculate option payouts with the specified number of options
                std::vector<std::vector<double>> optionPayouts = calculateOptionPayouts(priceList, numOptions);

                // Accumulate payoffs across options up to and including the current option
                std::vector<double> portfolioPayoffs(priceList.size(), 0.0);
                for (size_t j = 0; j <= i; ++j) {
                    for (size_t k = 0; k < priceList.size(); ++k) {
                        portfolioPayoffs[k] += optionPayouts[k][j]; // Accumulate payoffs for the current option
                    }
                }

                // Calculate mean
                double mean = 0.0;
                for (size_t k = 0; k < priceList.size(); ++k) {
                    mean += portfolioPayoffs[k] / priceList.size();
                }

                // Calculate variance
                double varianceSum = 0.0;
                for (size_t k = 0; k < priceList.size(); ++k) {
                    double sum = portfolioPayoffs[k];
                    varianceSum += (sum - mean) * (sum - mean);
                }
                double variance = varianceSum / priceList.size();

                // Calculate standard deviation
                double stdDeviation = std::sqrt(variance);

                // Store the results in a row
                std::vector<double> resultRow{static_cast<double>(partialOptions.size()), mean, variance, stdDeviation};
                resultMatrix.push_back(resultRow);

                // Print the row of statistics
                std::cout << "Option Count: " << partialOptions.size()
                        << ", Mean: " << mean
                        << ", Variance: " << variance
                        << ", Standard Deviation: " << stdDeviation << std::endl;

                if (runType == "sequential" && variance > varianceCutoff){
                    writeCombined(priceList, optionPayouts, partialOptions, "combined_table.csv", partialOptions.size());
                    std::cout << "Variance cutoff exceed, ending program" << std::endl;
                    break;
                }

                // Save combined data to CSV in the last iteration or if user chooses not to continue
                if (i == numOptions - 1) {
                    writeCombined(priceList, optionPayouts, partialOptions, "combined_table.csv", partialOptions.size());
                }
            }
        }

        // Save results to CSV
        writeStats(resultMatrix, "result_matrix.csv");

        return resultMatrix;
    }
};

int main()
{
    try {
        // Get the input filename from the user
        std::string inputFilename;
        std::cout << "Enter the name of the input CSV file: ";
        std::cin >> inputFilename;

        // Set asset parameters
        double meanReturn = 0.05; // 5% annual mean return
        double stdDev = 0.2;      // 20% annual standard deviation
        double spotPrice = 5000;  // typical S&P500 spot price

        // Start measuring time
        auto start = std::chrono::high_resolution_clock::now();

        // Create FinancialModel object with user-provided input filename
        FinancialModel financialModel(spotPrice, meanReturn, stdDev, inputFilename);

        // Run the financial model
        financialModel.runModel(10000, spotPrice);

        // End measuring time
        auto end = std::chrono::high_resolution_clock::now();

        // Calculate the elapsed time
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);

        // Output the running time
        std::cout << "Running time: " << duration.count() << " milliseconds" << std::endl;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1; // Terminate the program with an error code
    }

    return 0;
}
