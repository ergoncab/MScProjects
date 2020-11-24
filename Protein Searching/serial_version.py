import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import time

# Load the data
df = pd.read_csv("proteins.csv", sep=';')
#  Some rows are empty so we change them to - string for easier handling
df = df.fillna('-')
# Introduce the pattern to search
patt = input('Introduce the pattern to search: ')
# Change to capital letters
patt = patt.upper()
# Print the chosen pattern
print("Chosen pattern: " + patt)

# Start timing
start_time = time.time()

# Count matches
freq = df.sequence.str.count(patt)
# Delete entries with 0 matches
freq = freq[freq > 0]

# Stop timing
stop_time = time.time()

if freq.size == 0:
    print('No matches found')
    # Print time
    print("Serial execution time: " + str(stop_time - start_time))
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
    print("Serial execution time: " + str(stop_time - start_time))

    # Plot the result
    freq.plot.bar()
    tics_position = np.arange(freq.size)
    plt.xticks(tics_position, freq.index, rotation='vertical', fontsize=6)
    plt.title('Histogram of occurrences')
    plt.xlabel('Proteins id')
    plt.ylabel('Number of occurrences')
    plt.show()