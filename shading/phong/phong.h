#ifndef _PHONG_H
#define _PHONG_H

const float GAMMA = 2.2;

struct Light {
  vec3 position;
  vec3 color;
  vec3 ambient;
  float falloff;
  float radius;
};

struct Material {
  float specularScale;
  float shininess;
  float roughness;
  float albedo;
};

vec3 calcPhongReflection(Light light,
                         Material material,
                         vec3 normal,
                         vec3 viewPosition,
                         vec3 diffuseColor,
                         float specularStrength,
                         mat4 view);

#endif
