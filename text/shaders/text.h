#ifndef _TEXT_H
#define _TEXT_H

#ifdef FRAGMENT_SHADER

struct SDFMap {
  sampler2D sdf;
  float threshold;
};

vec4 sdfTest(vec4 baseColor, vec2 sdfCoords, SDFMap map);

#endif // FRAGMENT_SHADER


#ifdef GEOMETRY_SHADER

struct Glyph {
  vec4 box;
  vec4 texCoords;
};

struct GlyphVertex {
  vec2 position;
  vec2 texCoord;
};

GlyphVertex[4] makeGlyphVertices (Glyph glyph);

#endif // GEOMETRY_SHADER


#endif