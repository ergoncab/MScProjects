import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import time
import multiprocessing as mp


# Define a function to apply a pandas method so we can parallelize it
def find_matches(dframe, pattern):
    return dframe.sequence.str.count(pattern)


# Line to get the parallelization to work in Windows
if __name__ == "__main__":
    # Choose the number of cores to use
    noCores = 6
    # Load the data
    df = pd.read_csv("proteins.csv", sep=';')
    # Some rows are empty so we change them to - string for easier handling
    df = df.fillna('-')
    # Introduce the pattern to search
    patt = input('Introduce the pattern to search: ')
    # Change to capital letters
    patt = patt.upper()
    # Print the chosen pattern
    print("Chosen pattern: " + patt)

    # Start timing
    start_time = time.time()

    # Divide the data in 6 chunks
    df_chunks = np.array_split(df, noCores)
    # Create a pool with the number of cores chosen
    myPool = mp.Pool(noCores)
    freq = pd.concat(myPool.apply(find_matches, args=(x, patt)) for x in df_chunks)
    # Close the pool
    myPool.close()
    # Delete entries with 0 matches
    freq = freq[freq > 0]

    # Stop timing
    stop_time = time.time()

    if freq.size == 0:
        print('No matches found')
        # Print time
        print("Parallel (multiprocessing) execution time: " + str(stop_time - start_time))
    else:
        # Proteins with max matches
        max_patternmatches = max(freq.values)
        protein_maxmatches = list(freq).count(max_patternmatches)
        matches = freq.nlargest(protein_maxmatches)
        # Print proteins with max matches
        print("Found " + str(protein_maxmatches) + " proteins with " + str(max_patternmatches) + " maximum matches")
        print("")
        for n in range(0, protein_maxmatches):
            print("Protein " + str(matches.index[n]) + " with maximum occurrences: " + df.iloc[n, 1])

        # Print time
        print("")
        print("Parallel (multiprocessing) execution time: " + str(stop_time - start_time))

        # Plot the result
        freq.plot.bar()
        tics_position = np.arange(freq.size)
        plt.xticks(tics_position, freq.index, rotation='vertical', fontsize=6)
        plt.title('Histogram of occurrences')
        plt.xlabel('Proteins id')
        plt.ylabel('Number of occurrences')
        plt.show()