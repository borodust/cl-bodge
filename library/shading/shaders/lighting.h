#ifndef _LIGHTING_H
#define _LIGHTING_H

struct DirectionalLight {
  vec4 ambient;
  vec4 diffuse;
  vec3 direction;
};

vec4 computeLight(vec4 base, vec3 normal, DirectionalLight lightSource);

#endif
