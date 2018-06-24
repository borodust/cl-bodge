#version 330 core

#include <bodge/text>

layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

in v_PerVertex {
  vec4 box;
  vec4 sdfCoord;
} v_in[1];

out gl_PerVertex {
  vec4 gl_Position;
};

out g_PerVertex {
  vec2 sdfCoord;
};

uniform mat4 proj;
uniform float scale;

void main () {
  GlyphVertex[] vertices = makeGlyphVertices(Glyph(v_in[0].box, v_in[0].sdfCoord));
  for (int i = 0; i < 4; ++i) {
    gl_Position = proj * vec4(vertices[i].position * scale, 0.0, 1.0);
    sdfCoord = vertices[i].texCoord;
    EmitVertex();
  }

  EndPrimitive();
}
