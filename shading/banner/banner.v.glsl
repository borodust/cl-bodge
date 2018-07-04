#version 330 core

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec2 vTexCoord;

out v_PerVertex {
  vec2 texCoord;
};

uniform mat4 MVP;

void main () {
  gl_Position = MVP * vec4(vPosition, 1.0);
  texCoord = vTexCoord;
}
