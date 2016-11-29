#version 410

struct DirectionalLight {
  vec3 ambient;
  vec3 diffuse;
  vec3 direction;
};

uniform DirectionalLight dLight;

vec3 computeLight(vec3 base, vec3 norm) {
  float f = max(-dot(dLight.direction, norm), 0);
  return dLight.ambient * base + dLight.diffuse * f;
}
