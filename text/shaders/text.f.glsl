#version 330 core

#include <bodge/text>

out vec4 fColor;

in g_PerVertex {
  vec2 sdfCoord;
};

uniform sampler2D atlas;
uniform vec4 baseColor;

void main() {
  fColor = sdfTest(baseColor, sdfCoord, atlas);
}
