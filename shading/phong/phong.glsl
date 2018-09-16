#version 330 core

#pragma bodge: import bodge/phong
#pragma bodge: import bodge/math

#ifdef FRAGMENT_SHADER

// adapted from https://github.com/stackgl/glsl-lighting-walkthrough
float attenuation(float r, float f, float d) {
  float denom = d / r + 1.0;
  float attenuation = 1.0 / (denom * denom);
  float t = (attenuation - f) / (1.0 - f);
  return max(t, 0.0);
}

// Oren-Nayar
float computeDiffuse(vec3 lightDirection,
                     vec3 viewDirection,
                     vec3 surfaceNormal,
                     float roughness,
                     float albedo) {
  float LdotV = dot(lightDirection, viewDirection);
  float NdotL = dot(surfaceNormal, lightDirection);
  float NdotV = dot(surfaceNormal, viewDirection);

  float s = LdotV - NdotL * NdotV;
  float t = mix(1.0, max(NdotL, NdotV), step(0.0, s));

  float sigma2 = roughness * roughness;
  float A = 1.0 + sigma2 * (albedo / (sigma2 + 0.13) + 0.5 / (sigma2 + 0.33));
  float B = 0.45 * sigma2 / (sigma2 + 0.09);

  return albedo * max(0.0, NdotL) * (A + B * s / t) / PI;
}

// Phong
float computeSpecular(vec3 lightDirection,
                      vec3 viewDirection,
                      vec3 surfaceNormal,
                      float shininess) {
  //Calculate Phong power
  vec3 R = -reflect(lightDirection, surfaceNormal);
  return pow(max(0.0, dot(viewDirection, R)), shininess);
}

vec3 calcPhongReflection(PhongPointLight light,
                         PhongMaterial material,
                         vec3 position,
                         vec3 normal,
                         vec3 diffuseColor,
                         float specularStrength,
                         vec3 eyeDirection) {
  //determine surface to light direction
  vec3 lightVector = light.position - position;
  vec3 color = vec3(0.0);

  //calculate attenuation
  float lightDistance = length(lightVector);
  float falloff = attenuation(light.radius, light.falloff, lightDistance);

  vec3 L = normalize(lightVector);              //light direction
  vec3 V = eyeDirection;                        //eye direction

  //compute our diffuse & specular terms
  float specular = specularStrength *
    computeSpecular(L, -V, normal, material.shininess) *
    material.specularScale *
    falloff;
  vec3 diffuse = light.color *
    computeDiffuse(L, -V, normal, material.roughness, material.albedo) *
    falloff;
  vec3 ambient = light.ambient;

  //add the lighting
  color += diffuseColor * (diffuse + ambient) + specular;

  return color;
}

#endif
