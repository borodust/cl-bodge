#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vTexCoord;

out v_PerVertex {
  vec4 position;
  vec4 texCoord;
};

void main () {
  position = vPosition;
  texCoord = vTexCoord;
}
