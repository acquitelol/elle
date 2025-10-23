#version 330

struct Light {
    vec3 position;
    vec4 color;
    float intensity;
};

struct Sphere {
    vec3 center;
    vec4 color;
    float radius;
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
#define LIGHT_RADIUS 2

uniform Light lights[MAX_ENTITY];
uniform int lights_size;

uniform Sphere spheres[MAX_ENTITY];
uniform int spheres_size;

uniform Camera camera;
uniform Plane plane;

float rand(vec2 co) {
    return fract(sin(dot(co, vec2(12.9898, 78.233))) * 43758.5453);
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

bool hit_light(Ray ray, out vec4 light_color) {
    for (int i = 0; i < lights_size; ++i) {
        float t = intersect_sphere(Ray(ray.position, normalize(lights[i].position - ray.position)), Sphere(lights[i].position, lights[i].color, LIGHT_RADIUS));

        if (t > 0.0) {
            light_color = lights[i].color * lights[i].intensity;
            return true;
        }
    }

    return false;
}

vec4 trace_ray(Ray ray, vec2 seed) {
    vec4 color = vec4(1.0);
    Ray current = ray;
    int depth = 128;

    for (int bounce = 0; bounce < depth; ++bounce) {
        float closest = 1e20;
        int hit_index = -1;

        for (int i = 0; i < spheres_size; ++i) {
            float t = intersect_sphere(current, spheres[i]);
            if (t > 0.0 && t < closest) {
                closest = t;
                hit_index = i;
            }
        }

        float t_plane = intersect_plane(current);

        if (t_plane > 0.0 && t_plane < closest) {
            closest = t_plane;
            hit_index = -2;
        }

        if (closest > 1e19) return color * vec4(0.85, 0.82, 1, 1);
        // if (closest > 1e19) return vec4(0.01);
        vec3 point = current.position + current.direction * closest;
        vec3 normal = hit_index >= 0 ? normalize(point - spheres[hit_index].center) : plane.normal;
        color *= hit_index >= 0 ? spheres[hit_index].color : plane.color;

        vec4 light_color;
        vec3 reflected = reflect(current.direction, normal);

        if (hit_light(Ray(point, current.direction), light_color)) {
            color *= light_color;
        }

        vec3 random_dir = normalize(normal + vec3(rand(seed), rand(seed + 1), rand(seed + 2)) - 0.5);

        current = Ray(point + normal * 1e-4, normalize(mix(reflected, random_dir, 0.3))); // roughness
        seed += 0.1;

    }

    return color;
}

void main() {
    int samples = 128;
    vec4 sum = vec4(0.0);

    for (int i = 0; i < samples; ++i) {
        float n1 = rand(fragTexCoord + float(i));
        float n2 = rand(fragTexCoord + float(i) * PI);

        Ray ray = get_ray(camera, fragTexCoord, vec2(n1, n2));
        sum += trace_ray(ray, vec2(n1, n2));
    }

    finalColor = sum / float(samples);
}
