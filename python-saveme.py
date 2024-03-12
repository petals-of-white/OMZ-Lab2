import numpy as np
import glfw
from OpenGL.GL import *

import pydicom

# Global variables
n = 256  # Number of rows
m = 256  # Number of columns
file_path = 'C:\\Users\\maxle\\Обробка_медичних_зображень\\Lab2\\Lab2\\DICOM_Image_16b.dcm'  # Replace with the actual path to your binary file


def read_binary_file(file_path):
    with open(file_path, 'rb') as file:
        binary_data = file.read()
    return np.frombuffer(binary_data, dtype=np.uint16)

def load_texture(data):
    texture_id = glGenTextures(1)
    glBindTexture(GL_TEXTURE_2D, texture_id)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, m, n, 0, GL_RED, GL_UNSIGNED_SHORT, data)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    return texture_id

def display():
    glClear(GL_COLOR_BUFFER_BIT)
    glEnable(GL_TEXTURE_2D)
    glBindTexture(GL_TEXTURE_2D, texture_id)

    glBegin(GL_TRIANGLES)
    glTexCoord2f(0.0, 0.0)
    glVertex2f(-1.0, -1.0)
    
    glTexCoord2f(1.0, 0.0)
    glVertex2f(1.0, -1.0)
    
    glTexCoord2f(1.0, 1.0)
    glVertex2f(1.0, 1.0)
    
    glTexCoord2f(0.0, 1.0)
    glVertex2f(-1.0, 1.0)
    glEnd()

    glDisable(GL_TEXTURE_2D)
    

def main():
    if not glfw.init():
        return

    window = glfw.create_window(m, n, "Binary Image Viewer", None, None)
    if not window:
        glfw.terminate()
        return

    glfw.make_context_current(window)

    # Load binary data
    ds = pydicom.read_file(file_path)

    # Initialize OpenGL
    glClearColor(0.0, 0.0, 0.0, 0.0)
    # glMatrixMode(GL_PROJECTION)
    # glLoadIdentity()
    # glOrtho(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0)
    
    # Load texture
    global texture_id
    texture_id = load_texture(ds.PixelData)
    print('texture id is ',texture_id)
    # Set display function
    glfw.set_key_callback(window, key_callback)
    while not glfw.window_should_close(window):
        display()
        glfw.swap_buffers(window)
        glfw.poll_events()

    glfw.terminate()

def key_callback(window, key, scancode, action, mods):
    if key == glfw.KEY_ESCAPE and action == glfw.PRESS:
        glfw.set_window_should_close(window, True)

if __name__ == "__main__":
    main()