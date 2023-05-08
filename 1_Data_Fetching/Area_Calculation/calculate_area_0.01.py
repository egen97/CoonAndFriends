import cv2
import os
import csv
import numpy as np


#target_color = (176, 180, 245)  # OpenCV uses BGR channel ordring, and not RGB.
target_color = (177,181,246)
color_range = 20 #50  # range of 20 looks fine (at least for the first image).

# Define the folder containing the images
folder = "Data/pictures/test/"

# Define the path to the output CSV file
output_file = "Data/pictures/area.csv"

# Open the CSV file for writing
with open(output_file, 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)

    # Write the header row
    writer.writerow(["Filename", "Percentage"])

    # Loop over every file in the folder
    for filename in os.listdir(folder):

        # Make sure the file is an image
        if filename.endswith(".jpg") or filename.endswith(".jpeg") or filename.endswith(".png"):

            # Construct the full path to the image
            filepath = os.path.join(folder, filename)

            # Load the image
            img = cv2.imread(filepath)

            # Calculate the percentage of the image that is within the color range of the target 
            lower_bound = np.clip(np.array(target_color) - color_range, 0, 255)
            upper_bound = np.clip(np.array(target_color) + color_range, 0, 255)
            mask = cv2.inRange(img, lower_bound, upper_bound)
            cv2.imwrite('mask.png', mask)  # Save mask for testing
            num_pixels = img.shape[0] * img.shape[1]
            target_pixels = cv2.countNonZero(mask)
            percentage = (target_pixels / num_pixels) * 100
            print(num_pixels, "num")
            print(target_pixels, "tar")
            print(percentage, "per")

            # Write the filename and percentage to the CSV file
            writer.writerow([filename, percentage])
