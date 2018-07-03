#version 330 core

layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

in v_PerVertex {
  vec4 position;
  vec4 texCoord;
} v_in[1];

uniform mat4 MVP;

out gl_PerVertex {
  vec4 gl_Position;
};

out g_PerVertex {
  vec2 texCoord;
};

vec4 transform(float x, float y) {
  vec4 transformed = MVP * vec4(x, y, 0.0, 1.0);
  return vec4(transformed.xy - 1.0 * transformed.w, transformed.z, transformed.w);
}

void main () {
  float x = v_in[0].position.x;
  float y = v_in[0].position.y;
  float width = v_in[0].position.z;
  float height = v_in[0].position.w;

  float tex_x = v_in[0].texCoord.x;
  float tex_y = v_in[0].texCoord.y;
  float tex_width = v_in[0].texCoord.z;
  float tex_height = v_in[0].texCoord.w;

  gl_Position = transform(x + width, y);
  texCoord = vec2(tex_x + tex_width, tex_y);
  EmitVertex();

  gl_Position = transform(x + width, y + height);
  texCoord = vec2(tex_x + tex_width, tex_y + tex_height);
  EmitVertex();

  gl_Position = transform(x, y);
  texCoord = vec2(tex_x, tex_y);
  EmitVertex();

  gl_Position = transform(x, y + height);
  texCoord = vec2(tex_x, tex_y + tex_height);
  EmitVertex();

  EndPrimitive();
}
