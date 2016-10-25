#version 410


struct Bone {
  int id;
  mat3 rotation;
};

uniform Bone bones[2];

vec3 skinMesh(vec3 position, float weight0, float weight1) {
  return position;
}
