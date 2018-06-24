#version 330 core

#include <bodge/text>

#ifdef FRAGMENT_SHADER

vec4 sdfTest(vec4 baseColor, vec2 sdfCoords, sampler2D atlas) {
  float d = texture(atlas, sdfCoords).r;
  // float width = fwidth(d);
  float width = length(vec2(dFdx(d), dFdy(d))) * 0.70710678118654757;
  float a = smoothstep(0.5 - width, 0.5 + width, d);

  if (a == 0.0) discard;
  return vec4(baseColor.rgb, a);
}

#endif // FRAGMENT_SHADER code


#ifdef GEOMETRY_SHADER

GlyphVertex[4] makeGlyphVertices (Glyph glyph) {
  return GlyphVertex[4](GlyphVertex(glyph.box.zy,
                                    glyph.texCoords.pt),
                        GlyphVertex(glyph.box.zw,
                                    glyph.texCoords.pq),
                        GlyphVertex(glyph.box.xy,
                                    glyph.texCoords.st),
                        GlyphVertex(glyph.box.xw,
                                    glyph.texCoords.sq));
}

#endif // GEOMETRY_SHADER code
