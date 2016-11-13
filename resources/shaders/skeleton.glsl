#version 410


struct Bone {
  mat4 transform;
};

uniform Bone bones[64];

const ivec4 ZERO_VEC = ivec4(0);

mat4 weightedTransform(ivec4 boneIds, vec4 weights) {
  if (all(equal(boneIds, ZERO_VEC))) {
    return mat4(1.0);
  } else {
    mat4 result = mat4(0.0);
    for (int i = 0; i < 4; ++i) {
      int boneId = boneIds[i];
      if (boneId > 0) {
        result += bones[boneId - 1].transform * weights[i];
      }
    }
    return result;
  }
}
