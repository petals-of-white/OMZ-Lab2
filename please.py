import numpy as np
import glfw
from OpenGL.GL import *
from OpenGL.GL.shaders import compileProgram, compileShader
from PIL import Image
import pydicom

# Define the dimensions of the image (n rows, m columns)
n, m = 256, 256

file_path = 'C:\\Users\\maxle\\Обробка_медичних_зображень\\Lab2\\Lab2\\DICOM_Image_16b.dcm'  # Replace with the actual path to your binary file


# # Read binary file (assuming it contains uint16 data)
# with open("image_data.bin", "rb") as f:
#     image_data = np.fromfile(f, dtype=np.uint16).reshape((n, m))

ds = pydicom.read_file(file_path)
print(ds)
image_data = ds.pixel_array

# print(image_data.tolist())

# Initialize GLFW
if not glfw.init():
    raise Exception("GLFW initialization failed")

# Create a window
window = glfw.create_window(800, 800, "OpenGL Texture Display", None, None)
if not window:
    glfw.terminate()
    raise Exception("Window creation failed")

# Make the window's context current
glfw.make_context_current(window)

# Compile shaders
vertex_shader = """
#version 330 core
layout (location = 0) in vec2 position;
out vec2 tex_coords;
void main()
{
    gl_Position = vec4(position.x, position.y, 0.0, 1.0);
    tex_coords = position * 0.5 + 0.5;
}
"""

fragment_shader = """
#version 330 core
in vec2 tex_coords;
out vec4 frag_color;
uniform sampler2D tex;
void main()
{
    frag_color = texture(tex, tex_coords);
}
"""

shader_program = compileProgram(compileShader(vertex_shader, GL_VERTEX_SHADER),
                                compileShader(fragment_shader, GL_FRAGMENT_SHADER))

# Create a texture
texture_id = glGenTextures(1)
glBindTexture(GL_TEXTURE_2D, texture_id)
glTexImage2D(GL_TEXTURE_2D, 0, GL_R16, m, n, 0, GL_RED, GL_UNSIGNED_SHORT, image_data)
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)

# Create vertex buffer
vertices = np.array([-1.0, -1.0, 1.0, -1.0, -1.0, 1.0, 1.0, 1.0], dtype=np.float32)
vbo = glGenBuffers(1)
glBindBuffer(GL_ARRAY_BUFFER, vbo)
glBufferData(GL_ARRAY_BUFFER, vertices.nbytes, vertices, GL_STATIC_DRAW)

# Create vertex array object
vao = glGenVertexArrays(1)
glBindVertexArray(vao)
glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, None)
glEnableVertexAttribArray(0)

# Main loop
while not glfw.window_should_close(window):
    glClear(GL_COLOR_BUFFER_BIT)
    glUseProgram(shader_program)
    glBindVertexArray(vao)
    glBindTexture(GL_TEXTURE_2D, texture_id)
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4)
    glfw.swap_buffers(window)
    glfw.poll_events()

# Clean up
glDeleteTextures(1, [texture_id])
glDeleteBuffers(1, [vbo])
glDeleteVertexArrays(1, [vao])
glfw.terminate()