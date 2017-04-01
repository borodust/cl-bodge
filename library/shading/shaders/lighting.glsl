#version 410
#include <lighting>


vec4 computeLight(vec4 base, vec3 normal, DirectionalLight lightSource) {
  float f = max(-dot(lightSource.direction, normal), 0);
  return (lightSource.ambient + lightSource.diffuse * f) * base;
}
