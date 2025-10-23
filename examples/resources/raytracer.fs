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

const int MAX_ENTITY = 16;

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

vec3 sample_light_position(Light light, vec2 seed) {
    float radius = 0.5;
    float theta = 2.0 * 3.141592 * seed.x;
    float r = radius * sqrt(seed.y);

    vec3 offset = vec3(r * cos(theta), r * sin(theta), 0.0);
    return light.position + offset;
}

vec4 calculate_light(Light light, vec3 base_color, vec3 point, vec3 normal, vec3 view_dir, float visibility) {
    float ambient = 0.2;
    vec3 light_dir = normalize(light.position - point);
    float diff = max(dot(normal, light_dir), 0.0);

    float distance = length(light.position - point);
    float attenuation = 1.0 / (1.0 + 0.01 * distance);

    vec3 reflect_dir = reflect(-light_dir, normal);
    float spec = pow(max(dot(view_dir, reflect_dir), 0.0), 25.0);

    vec3 diffuse = base_color * light.color.rgb * (ambient + diff * light.intensity * attenuation) * visibility;
    vec3 specular = vec3(spec) * light.color.rgb * light.intensity * attenuation * visibility;

    return vec4(diffuse + specular, 1.0);
}

float calculate_visibility(Light light, vec3 point, vec3 normal, vec2 seed) {
    float visibility = 0.0;
    int samples = 4;

    for (int i = 0; i < samples; ++i) {
        vec3 position = sample_light_position(light, seed);
        vec3 direction = normalize(position - point);
        float distance = length(position - point);

        Ray ray = Ray(point + normal * 1e-4, direction);
        bool blocked = false;

        for (int j = 0; j < spheres_size; ++j) {
            float t = intersect_sphere(ray, spheres[j]);

            if (t > 0.0 && t < distance) {
                blocked = true;
                break;
            }
        }

        if (!blocked) visibility += 1.0;
    }

    visibility /= float(samples);
    return visibility;
}

vec4 trace_ray(Ray ray, vec2 seed) {
    vec4 color = vec4(0.0);
    Ray current = ray;
    float factor = 1;
    int depth = 4;

    for (int bounce = 0; bounce < depth; ++bounce) {
        float closest_hit = 1e14;
        int closest_sphere = -1;

        for (int i = 0; i < spheres_size; ++i) {
            float hit = intersect_sphere(ray, spheres[i]);
            if (hit > 0 && hit < closest_hit) {
                closest_hit = hit;
                closest_sphere = i;
            }
        }

        float plane_hit = intersect_plane(current);

        if (closest_sphere == -1 && plane_hit < 0.0) {
            color += vec4(102.0/255.0,191.0/255.0,255.0/255.0,1.0); // sky
            break;
        }

        bool hit_plane = false;

        if (plane_hit > 0.0 && (plane_hit < closest_hit || closest_sphere == -1)) {
            closest_hit = plane_hit;
            hit_plane = true;
        }

        vec3 point = current.position + current.direction * closest_hit;
        vec3 normal = hit_plane ? plane.normal : normalize(point - spheres[closest_sphere].center);
        vec3 base_color = hit_plane ? plane.color.rgb : spheres[closest_sphere].color.rgb;
        vec3 view_dir = normalize(-current.direction);

        for (int i = 0; i < lights_size; ++i) {
            float visibility = calculate_visibility(lights[i], point, normal, seed);
            color += calculate_light(lights[i], base_color, point, normal, view_dir, visibility) * factor;
        }

        if (hit_plane) break;
        vec3 reflected = reflect(current.direction, normal);
        vec3 random_dir = normalize(normal + vec3(seed.x, seed.y, rand(fragTexCoord + seed.x + seed.y)) - 0.5);

        current = Ray(point + normal * 1e-4, normalize(mix(reflected, random_dir, 0.4))); // roughness
        factor /= 2;
    }

    return color;
}

void main() {
    int samples = 16;
    vec4 sum = vec4(0.0);

    for (int i = 0; i < samples; ++i) {
        float n1 = rand(fragTexCoord + float(i));
        float n2 = rand(fragTexCoord + float(i) * 3.141592);

        Ray ray = get_ray(camera, fragTexCoord, vec2(n1, n2));
        sum += trace_ray(ray, vec2(n1, n2));
    }

    finalColor = sum / float(samples);
}
