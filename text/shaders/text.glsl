#version 410

#include <text>

#ifdef FRAGMENT_SHADER

vec4 sdfTest(vec4 baseColor, vec2 sdfCoords, SDFMap map) {
  float d = texture(map.sdf, sdfCoords).r;
  float width = fwidth(d);
  float a = smoothstep(0.5 - width, 0.5 + width, d);

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
