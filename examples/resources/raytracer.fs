#version 330

#define KIND_DIFFUSE 0
#define KIND_GLASS 1
#define KIND_EMISSIVE 2

struct Sphere {
    vec3 center;
    vec4 color;
    float radius;
    float intensity;
    float ior;
    float rough;
    int kind;
};

struct Triangle {
    vec3 v0, v1, v2;
    vec3 n0, n1, n2;
    int objIndex;
};

struct Object {
    vec4 color;
    float intensity;
    float ior;
    float rough;
    int kind;
};

struct BVHNode {
    vec3 min;
    vec3 max;
    int left;
    int right;
    int start;
    int end;
};

struct Camera {
    vec3 position;
    vec3 forward;
    vec3 right;
    vec3 up;
    float fov;
    float ar;
};

struct Ray {
    vec3 position;
    vec3 direction;
};

struct Plane {
    vec3 point;
    vec3 normal;
    vec4 color;
};

struct Hit {
    bool did_hit;
    float t;
    int kind;
    vec4 color;
    vec3 point;
    vec3 normal;
    float intensity;
    float ior;
    float rough;
};

in vec2 fragTexCoord;
out vec4 finalColor;

#define PI 3.141592
#define MAX_ENTITY 16

uniform Sphere spheres[MAX_ENTITY];
uniform int spheres_size;

uniform samplerBuffer triangles;

uniform int triangles_size;
uniform int bvh_size;
uniform int bvh_start_index;

uniform Object objects[MAX_ENTITY];
uniform int objects_size;

uniform Camera camera;
uniform Plane plane;

uniform int depth;
uniform int samples;

uniform int frameIndex;

uniform sampler2D history;

float rand(vec2 co) {
    return fract(sin(dot(co, vec2(12.9898, 78.233))) * 43758.5453);
}

vec3 rand_dir(vec2 seed) {
    float a = rand(seed * 12.34) * 2 * PI;
    float z = rand(seed * 56.78) * 2.0 - 1.0;
    float r = sqrt(max(0.0, 1.0 - z * z));
    return vec3(r * cos(a), r * sin(a), z);
}

vec4 get_vertex(int i, int offset) {
    return texelFetch(triangles, i * 6 + offset);
}

Triangle get_triangle(int i) {
    vec4 v0 = get_vertex(i, 0);
    vec4 v1 = get_vertex(i, 1);
    vec4 v2 = get_vertex(i, 2);

    vec4 n0 = get_vertex(i, 3);
    vec4 n1 = get_vertex(i, 4);
    vec4 n2 = get_vertex(i, 5);

    return Triangle(v0.rgb, v1.rgb, v2.rgb, n0.rgb, n1.rgb, n2.rgb, int(v0.w));
}

BVHNode get_bvh_node(int i) {
    vec4 a = texelFetch(triangles, i * 3 + 0 + bvh_start_index);
    vec4 b = texelFetch(triangles, i * 3 + 1 + bvh_start_index);
    vec4 c = texelFetch(triangles, i * 3 + 2 + bvh_start_index);

    vec3 min = a.xyz;
    vec3 max = b.xyz;
    int left  = int(c.x);
    int right = int(c.y);

    int start = int(c.z);
    int end   = int(c.w);

    // return BVHNode(vec3(0), vec3(0), 0, 0, 0, 0);
    return BVHNode(min, max, left, right, start, end);
}

vec3 checkerboard_color(vec3 point) {
    int checkX = int(floor(point.x));
    int checkZ = int(floor(point.z));

    return plane.color.rgb * ((checkX + checkZ) % 2 == 0 ? 1 : 0.25);
}

Ray get_ray(Camera camera, vec2 uv, vec2 jitter) {
    uv += (jitter - 0.5) * (vec2(1.0) / vec2(textureSize(history, 0)));
    uv = uv * 2.0 - 1.0;
    uv.x *= camera.ar;

    float px = uv.x * tan(radians(camera.fov) * 0.5);
    float py = uv.y * tan(radians(camera.fov) * 0.5);

    return Ray(camera.position, normalize(camera.forward + px * camera.right + py * camera.up));
}

float intersect_sphere(Ray ray, Sphere sphere) {
    vec3 oc = ray.position - sphere.center;
    float a = dot(ray.direction, ray.direction);
    float b = 2.0 * dot(oc, ray.direction);
    float c = dot(oc, oc) - sphere.radius * sphere.radius;
    float discriminant = b * b - 4.0 * a * c;

    if (discriminant < 0.0) return -1.0;

    float sqrtDisc = sqrt(discriminant);
    float t0 = (-b - sqrtDisc) / (2.0 * a);
    float t1 = (-b + sqrtDisc) / (2.0 * a);

    if (t0 > 1.0e-3) return t0;
    if (t1 > 1.0e-3) return t1;

    return -1.0;
}

float intersect_plane(Ray ray) {
    float denom = dot(plane.normal, ray.direction);
    if (abs(denom) < 1e-6) return -1.0;

    float t = dot(plane.point - ray.position, plane.normal) / denom;
    if (t > 1e-3) return t;
    return -1.0;
}

vec3 intersect_triangle(Ray ray, Triangle triangle) {
    vec3 edge1 = triangle.v1 - triangle.v0;
    vec3 edge2 = triangle.v2 - triangle.v0;
    vec3 pvec = cross(ray.direction, edge2);

    float det = dot(edge1, pvec);
    if (abs(det) < 1e-6) return vec3(-1.0, 0, 0);

    float invDet = 1.0 / det;
    vec3 tvec = ray.position - triangle.v0;

    float u = dot(tvec, pvec) * invDet;
    if (u < 0.0 || u > 1.0) return vec3(-1.0, u, 0);

    vec3 qvec = cross(tvec, edge1);
    float v = dot(ray.direction, qvec) * invDet;
    if (v < 0.0 || (u + v) > 1.0) return vec3(-1.0, u, v);

    float t = dot(edge2, qvec) * invDet;
    if (t > 1.0e-3) return vec3(t, u, v);

    return vec3(-1.0, u, v);
}

bool intersect_aabb(Ray ray, vec3 _min, vec3 _max) {
    vec3 inv = 1.0 / (ray.direction + 1.0e-4);
    vec3 t0 = (_min - ray.position) * inv;
    vec3 t1 = (_max - ray.position) * inv;

    vec3 tminv = min(t0, t1);
    vec3 tmaxv = max(t0, t1);

    float tmin = max(max(tminv.x, tminv.y), tminv.z);
    float tmax = min(min(tmaxv.x, tmaxv.y), tmaxv.z);

    return tmax >= max(tmin, 0.0);
}

// The schlick approxmiation and glass scattering was adapted from:
// https://raytracing.github.io/books/RayTracingInOneWeekend.html
float schlick(float cos_theta, float ior) {
    float r0 = (1.0 - ior) / (1.0 + ior);
    r0 *= r0;
    return r0 + (1.0 - r0) * pow(1.0 - cos_theta, 5.0);
}

vec3 glass_scatter(vec3 ray_dir, vec3 hit_point, vec3 normal, float eta, vec2 seed) {
    vec3 unit_dir = normalize(ray_dir);
    float cos_theta = min(dot(-unit_dir, normal), 1.0);
    float sin_theta = sqrt(1.0 - cos_theta * cos_theta);

    bool cannot_refract = eta * sin_theta > 1.0;

    if (cannot_refract || schlick(cos_theta, eta) > rand(seed)) {
        return reflect(unit_dir, normal);
    } else {
        return refract(unit_dir, normal, eta);
    }
}

Hit find_hit(Ray ray) {
    Hit hit;
    hit.did_hit = false;
    float t = 1e20;

    for (int i = 0; i < spheres_size; ++i) {
        float ts = intersect_sphere(ray, spheres[i]);

        if (ts > 0.0 && ts < t) {
            vec3 point = ray.position + ray.direction * (t = ts);

            hit.did_hit = true;
            hit.kind = spheres[i].kind;
            hit.color = spheres[i].color;
            hit.point = point;
            hit.normal = normalize(point - spheres[i].center);
            hit.rough = spheres[i].rough;
            hit.ior = spheres[i].ior;
            hit.intensity = spheres[i].intensity;
        }
    }

    // for (int i = 0; i < triangles_size; ++i) {
    //     Triangle tri = get_triangle(i);
    //     Object obj = objects[tri.objIndex];
    //     float ts = intersect_triangle(ray, tri);

    //     if (ts > 0.0 && ts < t) {
    //         vec3 point = ray.position + ray.direction * (t = ts);

    //         hit.did_hit = true;
    //         hit.point = point;
    //         hit.normal = normalize(cross(tri.v1 - tri.v0, tri.v2 - tri.v0));
    //         hit.color = obj.color;
    //         hit.intensity = obj.intensity;
    //         hit.ior = obj.ior;
    //         hit.rough = obj.rough;
    //         hit.kind = obj.kind;
    //     }
    // }

    const int MAX_STACK = 64;
    int stack[MAX_STACK];
    int sp = 0;
    stack[sp++] = 0;

    while (sp > 0) {
        int node_index = stack[--sp];
        if (node_index < 0 || node_index >= bvh_size) continue;
        BVHNode node = get_bvh_node(node_index);

        if (!intersect_aabb(ray, node.min, node.max)) continue;

        if (node.left < 0 && node.right < 0) {
            for (int ti = node.start; ti < node.end; ++ti) {
                if (ti < 0 || ti > triangles_size) continue;
                Triangle tri = get_triangle(ti);
                Object obj = objects[tri.objIndex];

                vec3 ints = intersect_triangle(ray, tri);
                float ts = ints.x;
                float u = ints.y;
                float v = ints.z;

                if (ts > 0.0 && ts < t) {
                    vec3 point = ray.position + ray.direction * (t = ts);

                    float w = 1.0 - u - v;
                    vec3 normal = normalize(tri.n0 * w + tri.n1 * u + tri.n2);

                    // vec3 normal = normalize(cross(tri.v1 - tri.v0, tri.v2 - tri.v0));
                    // finalColor = vec4((normal + 1) / 2, 1);

                    hit.did_hit = true;
                    hit.point = point;
                    hit.normal = normal;
                    hit.color = obj.color;
                    hit.intensity = obj.intensity;
                    hit.ior = obj.ior;
                    hit.rough = obj.rough;
                    hit.kind = obj.kind;
                }
            }
        } else {
            if (node.left >= 0 && sp < MAX_STACK) {
                stack[sp++] = node.left;
            }

            if (node.right >= 0 && sp < MAX_STACK) {
                stack[sp++] = node.right;
            }
        }
    }

    float tp = intersect_plane(ray);
    if (tp > 0.0 && tp < t) {
        vec3 point = ray.position + ray.direction * (t = tp);

        hit.did_hit = true;
        hit.kind = KIND_DIFFUSE;
        hit.color = vec4(checkerboard_color(point), 1.0);
        hit.point = point;
        hit.normal = plane.normal;
        hit.rough = 1.0;
        hit.ior = 1.0;
        hit.intensity = 0.0;
    }

    return hit;
}

vec4 trace_ray(Ray ray, vec2 seed) {
    vec4 color = vec4(1.0);
    Ray current = ray;

    for (int bounce = 0; bounce < depth; ++bounce) {
        Hit hit = find_hit(current);

        // return vec4(0);

        if (!hit.did_hit) return color * vec4(0.85, 0.82, 1.0, 1.0);
        if (hit.kind == KIND_GLASS) hit.color = mix(vec4(1.0), hit.color, 0.3);
        if (hit.kind == KIND_EMISSIVE) return color * hit.color * hit.intensity;
        color *= hit.color;

        vec3 n = hit.normal;
        float eta = 1.0 / hit.ior;
        if (dot(current.direction, n) > 0.0) n = -n, eta = 1.0 / eta;

        vec3 new_dir;

        if (hit.kind == KIND_GLASS) {
            new_dir = glass_scatter(current.direction, hit.point, n, eta, seed);
        } else {
            new_dir = reflect(current.direction, n);
        }

        vec3 seed_dir = rand_dir(seed += 0.1);
        new_dir = normalize(mix(new_dir, seed_dir, hit.rough));
        current = Ray(hit.point + n * 1e-4, new_dir);
    }

    return color;
}

void main() {
    // BVHNode node = get_bvh_node(0);
    // vec4 x = texelFetch(triangles, 0);
    // return;
    vec4 sum = vec4(0.0);
    vec4 prev = texture(history, fragTexCoord);

    for (int i = 0; i < samples; ++i) {
        vec2 seed = fragTexCoord + vec2(float(frameIndex), float(i)) * 12.9898;
        float n1 = rand(seed);
        float n2 = rand(seed + 78.233);

        Ray ray = get_ray(camera, fragTexCoord, vec2(n1, n2));
        sum += trace_ray(ray, vec2(n1, n2));
    }

    vec4 current = sum / float(samples);

    if (frameIndex == 1) {
        finalColor = current;
    } else {
        finalColor = mix(prev, current, 1.0 / float(frameIndex));
    }
}
