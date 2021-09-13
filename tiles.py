from PIL import Image

img0 = Image.open("0.gif")
img1 = Image.open("1.gif")
img2 = Image.open("2.gif")
img3 = Image.open("3.gif")
img4 = Image.open("4.gif")
img5 = Image.open("5.gif")
img6 = Image.open("6.gif")
img7 = Image.open("7.gif")
img8 = Image.open("8.gif")
img9 = Image.open("9.gif")
img10 = Image.open("10.gif")
img11 = Image.open("11.gif")
img12 = Image.open("12.gif")
img13 = Image.open("13.gif")
img14 = Image.open("14.gif")
img15 = Image.open("15.gif")
img135 = Image.open("135.gif")

n = 3 # rows
m = 3 # cols
seed = 15
px = 32
height = n * px
width = m * px

#tiles = [15,8,0,1,3,10,12,2,4,6,9,135,5,7,14,11]
#tiles = [5,2,15,5,2,15,5,2,15]
#tiles = [5,5,5,15,15,15,11,11,11]
tiles = [15,13,5,15,15,11,11,13,0]
# creating a new image and pasting
# the images
img_tiles = Image.new("RGB", (width, height), "white")
rows = 0
cols = 0
x = 0
y = 0
for tile in tiles:
    if tile == 0:
        img_tile = img0
    elif tile == 1:
        img_tile = img1
    elif tile == 2:
        img_tile = img2
    elif tile == 3:
        img_tile = img3
    elif tile == 4:
        img_tile = img4
    elif tile == 5:
        img_tile = img5
    elif tile == 6:
        img_tile = img6
    elif tile == 7:
        img_tile = img7
    elif tile == 8:
        img_tile = img8
    elif tile == 9:
        img_tile = img9
    elif tile == 10:
        img_tile = img10
    elif tile == 11:
        img_tile = img11
    elif tile == 12:
        img_tile = img12
    elif tile == 13:
        img_tile = img13
    elif tile == 14:
        img_tile = img14
    elif tile == 15:
        img_tile = img15
    elif tile == 135:
        img_tile = img135
    img_tiles.paste(img_tile, (x, y))
    cols += 1
    rows += 1
    x += px

    if cols % m == 0:
        y += px
        x = 0

img_tiles.show()
img_tiles.save("test.gif")