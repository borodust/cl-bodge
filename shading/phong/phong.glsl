#version 330 core

#include <bodge/phong>
#include <bodge/math>

#ifdef FRAGMENT_SHADER

// adapted from https://github.com/stackgl/glsl-lighting-walkthrough
float attenuation(float r, float f, float d) {
  float denom = d / r + 1.0;
  float attenuation = 1.0 / (denom * denom);
  float t = (attenuation - f) / (1.0 - f);
  return max(t, 0.0);
}

mat3 cotangent(vec3 N, vec3 p, vec2 uv) {
  // get edge vectors of the pixel triangle
  vec3 dp1 = dFdx(p);
  vec3 dp2 = dFdy(p);
  vec2 duv1 = dFdx(uv);
  vec2 duv2 = dFdy(uv);

  // solve the linear system
  vec3 dp2perp = cross(dp2, N);
  vec3 dp1perp = cross(N, dp1);
  vec3 T = dp2perp * duv1.x + dp1perp * duv2.x;
  vec3 B = dp2perp * duv1.y + dp1perp * duv2.y;

  // construct a scale-invariant frame
  float invmax = 1.0 / sqrt(max(dot(T,T), dot(B,B)));
  return mat3(T * invmax, B * invmax, N);
}

vec3 perturb(vec3 map, vec3 N, vec3 V, vec2 texcoord) {
  mat3 TBN = cotangent(N, -V, texcoord);
  return normalize(TBN * map);
}

// Oren-Nayar
float computeDiffuse(vec3 lightDirection,
                     vec3 viewDirection,
                     vec3 surfaceNormal,
                     float roughness,
                     float albedo) {
  float LdotV = dot(lightDirection, viewDirection);
  float NdotL = dot(lightDirection, surfaceNormal);
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
                         vec3 normal,
                         vec3 viewPosition,
                         vec3 diffuseColor,
                         float specularStrength,
                         mat4 view) {
  //determine surface to light direction
  vec4 lightPosition = view * vec4(light.position, 1.0);
  vec3 lightVector = lightPosition.xyz - viewPosition;
  vec3 color = vec3(0.0);
  vec4(normal, 1.0);


  //calculate attenuation
  float lightDistance = length(lightVector);
  float falloff = attenuation(light.radius, light.falloff, lightDistance);

  vec3 L = normalize(lightVector);              //light direction
  vec3 V = normalize(viewPosition);            //eye direction

  //compute our diffuse & specular terms
  float specular = specularStrength *
    computeSpecular(L, V, normal, material.shininess) *
    material.specularScale * falloff;
  vec3 diffuse = light.color *
    computeDiffuse(L, V, normal, material.roughness, material.albedo) *
    falloff;
  vec3 ambient = light.ambient;

  //add the lighting
  color += diffuseColor * (diffuse + ambient) + specular;

  return color;
}

#endif
