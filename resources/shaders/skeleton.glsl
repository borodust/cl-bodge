#version 410


struct Bone {
  mat4 transform;
};

uniform Bone bones[2];

vec4 weightedPosition(vec4 pos, float w0, float w1) {
  return (bones[0].transform * w0 + bones[1].transform * w1) * pos;
}
