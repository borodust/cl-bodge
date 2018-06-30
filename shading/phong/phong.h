#ifndef _PHONG_H
#define _PHONG_H

struct PhongPointLight {
  vec3 position;
  vec3 color;
  vec3 ambient;
  float falloff;
  float radius;
};

struct PhongMaterial {
  float specularScale;
  float shininess;
  float roughness;
  float albedo;
};

vec3 calcPhongReflection(PhongPointLight light,
                         PhongMaterial material,
                         vec3 normal,
                         vec3 viewPosition,
                         vec3 diffuseColor,
                         float specularStrength,
                         mat4 view);

#endif
