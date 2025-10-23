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
    vec3 target;
    vec3 up;
    float fov;
    float ar;
};

struct Ray {
    vec3 position;
    vec3 direction;
};

in vec2 fragTexCoord;
out vec4 finalColor;

const int MAX_ENTITY = 16;

uniform Light lights[MAX_ENTITY];
uniform int lights_size;

uniform Sphere spheres[MAX_ENTITY];
uniform int spheres_size;

uniform Camera camera;

Ray get_ray(Camera camera) {
    vec2 uv = fragTexCoord * 2.0 - 1.0;
    uv.x *= camera.ar;

    vec3 forward = normalize(camera.target - camera.position);
    vec3 right = normalize(cross(forward, camera.up));
    vec3 up = normalize(cross(right, forward));

    float px = uv.x * tan(radians(camera.fov) / 2.0);
    float py = uv.y * tan(radians(camera.fov) / 2.0);
    vec3 rayDir = normalize(forward + px * right + py * up);

    return Ray(camera.position, rayDir);
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

vec4 calculate_light(Light light, Sphere sphere, vec3 point, vec3 normal, vec3 view_dir) {
    float ambient = 0.1;
    vec3 light_dir = normalize(light.position - point);
    float diff = max(dot(normal, light_dir), ambient);

    vec3 reflect_dir = reflect(-light_dir, normal);
    float spec = pow(max(dot(view_dir, reflect_dir), 0.0), 64.0);

    float distance = length(light.position - point);
    float attenuation = 1.0 / (1 + 0.01 * distance);

    vec3 diffuseColor = light.color.rgb * (ambient + diff * light.intensity * attenuation);
    vec3 specularColor = vec3(spec) * light.color.rgb * light.intensity * attenuation;
    vec3 color = sphere.color.rgb * diffuseColor + specularColor;
    return vec4(color, 1.0);
}

vec4 trace_ray(Ray ray) {
    vec4 color = vec4(0.0);
    Ray current = ray;
    float factor = 1;
    int depth = 10;

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

        if (closest_sphere == -1) {
            color = vec4(102.0 / 255.0, 191.0 / 255.0, 255.0 / 255.0, 1.0); // SKYBLUE
            break;
        }

        vec3 point = current.position + current.direction * closest_hit;
        vec3 normal = normalize(point - spheres[closest_sphere].center);
        vec3 view_dir = normalize(-current.direction);

        for (int i = 0; i < lights_size; ++i) {
            color += calculate_light(lights[i], spheres[closest_sphere], point, normal, view_dir) * factor;
        }

        current = Ray(point + normal * 1e-4, reflect(current.direction, normal));
        factor /= 1.8;
    }

    return color;
}


void main() {
    Ray ray = get_ray(camera);
    finalColor = trace_ray(ray);
}
