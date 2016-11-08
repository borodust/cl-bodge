#version 410


struct Bone {
  mat4 transform;
};

uniform Bone bones[64];

mat4 weightedTransform(ivec4 boneIds, vec4 weights) {
  mat4 result = mat4(1.0);
  for (int i = 0; i < 4; ++i) {
    int boneId = boneIds[i];
    if (boneId > 0) {
      result += bones[boneId - 1].transform * weights[i];
    }
  }
  return result;
}
