#version 330 core

// https://stackoverflow.com/questions/10786951/omnidirectional-shadow-mapping-with-depth-cubemap
// http://en.wikipedia.org/wiki/Z-buffering#Mathematics
float vectorToDepthValue(vec3 vec, float near, float far) {
    vec3 absVec = abs(vec);
    float localZComp = max(absVec.x, max(absVec.y, absVec.z));

    float normZComp = (far + near) / (far - near) - (2 * far * near)/(far - near) / localZComp;
    return (normZComp + 1.0) * 0.5;
}


bool inShadow(vec3 lightDirection, samplerCube shadowMap,
              float near, float far, float epsilon) {
  float lightDistance = length(lightDirection);
  float fragmentDistance = vectorToDepthValue(lightDirection, near, far);

  float referenceDistance = texture(shadowMap, lightDirection).r;

  if (referenceDistance + epsilon < fragmentDistance) {
    return true;
  } else {
    return false;
  }
}
