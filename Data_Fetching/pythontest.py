#Based on https://stackoverflow.com/questions/66757199/color-percentage-in-image-for-python-using-opencv
import cv2
import numpy as np

# Read image
imagePath = "Data/pictures/march5jpeg.jpeg"
img = cv2.imread(imagePath)

# Here, you define your target color as
# a tuple of three values: RGB
red = [233,150,122]
#Over 255 > out of bounds
#red = [250,184,185]
# [233,150,122] - Russian controlled Ukrainian terriritory before 24 feb 

# 250,184,185,255

# You define an interval that covers the values
# in the tuple and are below and above them by diff
diff = 70

# Be aware that opencv loads image in BGR format,
# that's why the color values have been adjusted here:
boundaries = [([red[2], red[1]-diff, red[0]-diff],
           [red[2]+diff, red[1]+diff, red[0]+diff])]
           
           
# Clip the values in boundaries to be between 0 and 255 
boundaries = np.clip(boundaries, 0, 255)           

# Scale your BIG image into a small one:
scalePercent = 0.3

# Calculate the new dimensions
width = int(img.shape[1] * scalePercent)
height = int(img.shape[0] * scalePercent)
newSize = (width, height)

# Resize the image:
img = cv2.resize(img, newSize, None, None, None, cv2.INTER_AREA)

# check out the image resized:
cv2.imshow("img resized", img)
cv2.waitKey(0)


# for each range in your boundary list:
for (lower, upper) in boundaries:

    # You get the lower and upper part of the interval:
    lower = np.array(lower, dtype=np.uint8)
    upper = np.array(upper, dtype=np.uint8)

    # cv2.inRange is used to binarize (i.e., render in white/black) an image
    # All the pixels that fall inside your interval [lower, uipper] will be white
    # All the pixels that do not fall inside this interval will
    # be rendered in black, for all three channels:
    mask = cv2.inRange(img, lower, upper)

    # Check out the binary mask:
    cv2.imshow("binary mask", mask)
    cv2.waitKey(0)

    # Now, you AND the mask and the input image
    # All the pixels that are white in the mask will
    # survive the AND operation, all the black pixels
    # will remain black
    output = cv2.bitwise_and(img, img, mask=mask)

    # Check out the ANDed mask:
    cv2.imshow("ANDed mask", output)
    cv2.waitKey(0)

    # You can use the mask to count the number of white pixels.
    # Remember that the white pixels in the mask are those that
    # fall in your defined range, that is, every white pixel corresponds
    # to a red pixel. Divide by the image size and you got the
    # percentage of red pixels in the original image:
    ratio_red = cv2.countNonZero(mask)/(img.size/3)

    # This is the color percent calculation, considering the resize I did earlier.
    colorPercent = (ratio_red * 100) / scalePercent

    # Print the color percent, use 2 figures past the decimal point
    print('red pixel percentage:', np.round(colorPercent, 15))

    # numpy's hstack is used to stack two images horizontally,
    # so you see the various images generated in one figure:
    cv2.imshow("images", np.hstack([img, output]))
    cv2.waitKey(0)

