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

def init_tile_img(tile):
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
    return img_tile

# tiles_list [n, m, tile_l, file_name]
tiles_list = [
    [4,4,[15,8,0,1,3,10,12,2,4,6,9,135,5,7,14,11],'sin_edge_4x4_15.gif'],
    [2,4,[15,8,0,4,1,2,10,9],'edge_0_2x4_15.gif'],
    [3,4,[15,9,1,3,11,8,0,2,12,4,6,10],'edge_1_3x4_15.gif'],
]
px = 32
for tile_list in tiles_list:
        n = tile_list[0] # rows
        m = tile_list[1] # cols
        tile_l = tile_list[2]
        file_name = tile_list[3]
        height = n * px
        width = m * px
        # creating a new image and pasting
        # the images
        img_tiles = Image.new("RGB", (width, height), "white")
        rows = 0
        cols = 0
        x = 0
        y = 0
        for tile in tile_l:
            img_tile = init_tile_img(tile)
            img_tiles.paste(img_tile, (x, y))
            cols += 1
            rows += 1
            x += px
            if cols % m == 0:
                y += px
                x = 0
        # si tengo solo un array lo muestro porque sino da error
        if len(tiles_list) == 1:
            img_tiles.show()
        img_tiles.save(file_name)