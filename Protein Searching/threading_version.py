import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import time
import threading as th


# Define a function to apply a pandas method so we can parallelize it
def find_matches(dframe, pattern, myList, myIndex):
    myList[myIndex] = dframe.sequence.str.count(pattern)
    return None


# Line to get the parallelization to work in Windows
if __name__ == "__main__":
    # Choose the number of threads to use
    noThreads = 8
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

    # Divide the data in 4 chunks
    df_chunks = np.array_split(df, noThreads)
    # Create a list to store the threads calculations results
    split_freq = [None] * noThreads
    # Call the function in each thread
    for num_thread in range(noThreads):
        thr = th.Thread(name='th%s' % num_thread,
                        target=find_matches, args=(df_chunks[num_thread], patt, split_freq, num_thread))
        thr.start()
    # Make sure that the threads have finished
    for num_thread in range(noThreads):
        thr.join()
    # Put all the results in the same list
    freq = pd.concat([split_freq[i] for i in range (noThreads)])
    # Delete entries with 0 matches
    freq = freq[freq > 0]

    # Stop timing
    stop_time = time.time()

    if freq.size == 0:
        print('No matches found')
        # Print time
        print("Parallel (threading) execution time: " + str(stop_time - start_time))
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
        print("Parallel (threads) execution time: " + str(stop_time - start_time))

        # Plot the result
        freq.plot.bar()
        tics_position = np.arange(freq.size)
        plt.xticks(tics_position, freq.index, rotation='vertical', fontsize=6)
        plt.title('Histogram of occurrences')
        plt.xlabel('Proteins id')
        plt.ylabel('Number of occurrences')
        plt.show()