#version 330 core

out vec4 fColor;

in vec3 texCoord;

uniform samplerCube banner;

void main() {
  fColor = texture(banner, texCoord);
}
