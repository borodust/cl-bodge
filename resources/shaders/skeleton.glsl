#version 410


struct Bone {
  mat4 transform;
};

uniform Bone bones[2];

mat4 weightedTransform(float w0, float w1) {
  return (bones[0].transform * w0 + bones[1].transform * w1);
}
