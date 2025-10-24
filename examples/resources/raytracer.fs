#version 330

struct Sphere {
    vec3 center;
    vec4 color;
    float intensity;
    float radius;
    float ior;
    float rough;
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

in vec2 fragTexCoord;
out vec4 finalColor;

#define PI 3.141592
#define MAX_ENTITY 16

uniform Sphere spheres[MAX_ENTITY];
uniform int spheres_size;

uniform Camera camera;
uniform Plane plane;

uniform int depth;
uniform int samples;

float rand(vec2 co) {
    return fract(sin(dot(co, vec2(12.9898, 78.233))) * 43758.5453);
}

vec3 rand_dir(vec2 seed) {
    float a = rand(seed * 12.34) * 2 * PI;
    float z = rand(seed * 56.78) * 2.0 - 1.0;
    float r = sqrt(max(0.0, 1.0 - z * z));
    return vec3(r * cos(a), r * sin(a), z);
}

vec3 checkerboard_color(vec3 point) {
    int checkX = int(floor(point.x));
    int checkZ = int(floor(point.z));

    return plane.color.rgb * ((checkX + checkZ) % 2 == 0 ? 1 : 0.25);
}

Ray get_ray(Camera camera, vec2 uv, vec2 jitter) {
    uv.x += (jitter.x - 0.5) * 0.002;
    uv.y += (jitter.y - 0.5) * 0.002;

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

vec4 trace_ray(Ray ray, vec2 seed) {
    vec4 color = vec4(1.0);
    Ray current = ray;

    for (int bounce = 0; bounce < depth; ++bounce) {
        float closest = 1e20;
        int hit_index = -1;
        bool hit_plane = false;

        for (int i = 0; i < spheres_size; ++i) {
            float t = intersect_sphere(current, spheres[i]);
            if (t > 0.0 && t < closest) {
                closest = t;
                hit_index = i;
                hit_plane = false;
            }
        }

        float t_plane = intersect_plane(current);

        if (t_plane > 0.0 && t_plane < closest) {
            closest = t_plane;
            hit_index = -2;
            hit_plane = true;
        }

        if (closest > 1e19) return color * vec4(0.85, 0.82, 1.0, 1.0);

        vec3 point = current.position + current.direction * closest;
        vec3 normal = hit_plane ? plane.normal : normalize(point - spheres[hit_index].center);

        if (spheres[hit_index].intensity > 0.0) {
            color *= 1 + spheres[hit_index].color * spheres[hit_index].intensity;
        }

        if (hit_plane) {
            color *= vec4(checkerboard_color(point), 1.0);
        } else {
            color *= mix(vec4(1.0), spheres[hit_index].color, 0.3);
        }

        vec3 n = normal;
        float eta = 1.0 / spheres[hit_index].ior;

        if (dot(current.direction, normal) > 0.0) n *= -1, eta = 1 / eta;

        vec3 new_dir;
        vec3 blur = normalize(rand_dir(seed));

        if (hit_plane) {
            new_dir = normalize(normal + rand_dir(seed));
        } else if (spheres[hit_index].ior == 1.0) {
            new_dir = reflect(current.direction, normal);
        } else {
            new_dir = glass_scatter(current.direction, point, n, eta, seed);
        }

        if (!hit_plane) new_dir = normalize(mix(new_dir, blur, spheres[hit_index].rough));

        current = Ray(point + n * 1e-4, new_dir);
        seed += 0.1;
    }

    return color;
}

void main() {
    vec4 sum = vec4(0.0);

    for (int i = 0; i < samples; ++i) {
        float n1 = rand(fragTexCoord + float(i));
        float n2 = rand(fragTexCoord + float(i) * PI);

        Ray ray = get_ray(camera, fragTexCoord, vec2(n1, n2));
        sum += trace_ray(ray, vec2(n1, n2));
    }

    finalColor = sum / float(samples);
}
