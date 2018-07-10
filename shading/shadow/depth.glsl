#version 330 core

#ifdef VERTEX_SHADER

layout(location = 0) in vec3 vPosition;

uniform mat4 MVP;

void main () {
  gl_Position = MVP * vec4(vPosition, 1.0);
}
#endif

#ifdef FRAGMENT_SHADER

void main() {
}

#endif
