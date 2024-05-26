#pragma once

#include <string>

inline static constexpr auto ROUNDED_SHADER_FUNC = [](const std::string colorVarName) -> std::string {
    return R"#(

    // branchless baby!
    highp vec2 pixCoord = vec2(gl_FragCoord);
    pixCoord -= topLeft + fullSize * 0.5;
    pixCoord *= vec2(lessThan(pixCoord, vec2(0.0))) * -2.0 + 1.0;
    pixCoord -= fullSize * 0.5 - radius;
    pixCoord += vec2(1.0, 1.0) / fullSize; // center the pix dont make it top-left

    if (pixCoord.x + pixCoord.y > radius) {

	    float dist = length(pixCoord);

	    if (dist > radius)
	        discard;

	    if (dist > radius - 1.0) {
	        float dist = length(pixCoord);

            float normalized = 1.0 - smoothstep(0.0, 1.0, dist - radius + 0.5);

	        )#" +
        colorVarName + R"#( = )#" + colorVarName + R"#( * normalized;
        }

    }
)#";
};

inline const std::string QUADVERTSRC = R"#(
uniform mat3 proj;
uniform vec4 color;
attribute vec2 pos;
attribute vec2 texcoord;
attribute vec2 texcoordMatte;
varying vec4 v_color;
varying vec2 v_texcoord;
varying vec2 v_texcoordMatte;

void main() {
    gl_Position = vec4(proj * vec3(pos, 1.0), 1.0);
    v_color = color;
    v_texcoord = texcoord;
    v_texcoordMatte = texcoordMatte;
})#";

inline const std::string QUADFRAGSRC = R"#(
precision highp float;
varying vec4 v_color;

uniform vec2 topLeft;
uniform vec2 fullSize;
uniform float radius;

void main() {

    vec4 pixColor = v_color;

    if (radius > 0.0) {
	)#" +
    ROUNDED_SHADER_FUNC("pixColor") + R"#(
    }

    gl_FragColor = pixColor;
})#";

inline const std::string TEXVERTSRC = R"#(
uniform mat3 proj;
attribute vec2 pos;
attribute vec2 texcoord;
varying vec2 v_texcoord;

void main() {
    gl_Position = vec4(proj * vec3(pos, 1.0), 1.0);
    v_texcoord = texcoord;
})#";

inline const std::string TEXVERTSRC320 = R"#(#version 320 es
uniform mat3 proj;
in vec2 pos;
in vec2 texcoord;
out vec2 v_texcoord;

void main() {
    gl_Position = vec4(proj * vec3(pos, 1.0), 1.0);
    v_texcoord = texcoord;
})#";

inline const std::string TEXFRAGSRCRGBA = R"#(
precision highp float;
varying vec2 v_texcoord; // is in 0-1
uniform sampler2D tex;
uniform float alpha;

uniform vec2 topLeft;
uniform vec2 fullSize;
uniform float radius;

uniform int discardOpaque;
uniform int discardAlpha;
uniform float discardAlphaValue;

uniform int applyTint;
uniform vec3 tint;

void main() {

    vec4 pixColor = texture2D(tex, v_texcoord);

    if (discardOpaque == 1 && pixColor[3] * alpha == 1.0)
	    discard;

    if (discardAlpha == 1 && pixColor[3] <= discardAlphaValue)
        discard;

    if (applyTint == 1) {
	    pixColor[0] = pixColor[0] * tint[0];
	    pixColor[1] = pixColor[1] * tint[1];
	    pixColor[2] = pixColor[2] * tint[2];
    }

    if (radius > 0.0) {
    )#" +
    ROUNDED_SHADER_FUNC("pixColor") + R"#(
    }

    gl_FragColor = pixColor * alpha;
})#";

inline const std::string TEXFRAGSRCRGBAPASSTHRU = R"#(
precision highp float;
varying vec2 v_texcoord; // is in 0-1
uniform sampler2D tex;

void main() {
    gl_FragColor = texture2D(tex, v_texcoord);
})#";

inline const std::string TEXFRAGSRCRGBAMATTE = R"#(
precision highp float;
varying vec2 v_texcoord; // is in 0-1
uniform sampler2D tex;
uniform sampler2D texMatte;

void main() {
    gl_FragColor = texture2D(tex, v_texcoord) * texture2D(texMatte, v_texcoord)[0]; // I know it only uses R, but matte should be black/white anyways.
})#";

inline const std::string TEXFRAGSRCRGBX = R"#(
precision highp float;
varying vec2 v_texcoord;
uniform sampler2D tex;
uniform float alpha;

uniform vec2 topLeft;
uniform vec2 fullSize;
uniform float radius;

uniform int discardOpaque;
uniform int discardAlpha;
uniform int discardAlphaValue;

uniform int applyTint;
uniform vec3 tint;

void main() {

    if (discardOpaque == 1 && alpha == 1.0)
	discard;

    vec4 pixColor = vec4(texture2D(tex, v_texcoord).rgb, 1.0);

    if (applyTint == 1) {
	pixColor[0] = pixColor[0] * tint[0];
	pixColor[1] = pixColor[1] * tint[1];
	pixColor[2] = pixColor[2] * tint[2];
    }

    if (radius > 0.0) {
    )#" +
    ROUNDED_SHADER_FUNC("pixColor") + R"#(
    }

    gl_FragColor = pixColor * alpha;
})#";

inline const std::string FRAGBLUR1 = R"#(
#version 320 es
precision            highp float;
in vec2              v_texcoord; // is in 0-1
out vec4             fragColor;
uniform sampler2D    tex;

uniform float        radius;
uniform vec2         halfpixel;
uniform int          passes;
uniform float        vibrancy;
uniform float        vibrancy_darkness;

// see http://alienryderflex.com/hsp.html
const float Pr = 0.299;
const float Pg = 0.587;
const float Pb = 0.114;

// Y is "v" ( brightness ). X is "s" ( saturation )
// see https://www.desmos.com/3d/a88652b9a4
// Determines if high brightness or high saturation is more important
const float a = 0.93;
const float b = 0.11;
const float c = 0.66; //  Determines the smoothness of the transition of unboosted to boosted colors

// Sample points
const vec2 samples[40] = vec2[40](vec2(0.0, 0.0), vec2(-0.1165882565225246, 0.10680439336018908), vec2(0.019548986344930585, -0.22275061645904756), vec2(0.1666278445104364, 0.21733651656775446), vec2(-0.31139374562795236, -0.055081169048905435), vec2(0.29831254531195034, -0.1897620228325615), vec2(-0.10054431494872178, 0.37401984002388466), vec2(-0.19281124176321465, -0.3712463131799792), vec2(0.4200772542587903, 0.15341153950859004), vec2(-0.4384555953675745, 0.18098809599215576), vec2(0.21192299752395535, -0.4528671362778068), vec2(0.15694578257216948, 0.5003678859927058), vec2(-0.4738956965881778, -0.27463224274510395), vec2(0.5567908576902676, -0.12240890814208018), vec2(-0.34025115883992346, 0.4839722604737684), vec2(-0.07869640412515617, -0.6072947192078736), vec2(0.4836064872401676, 0.4075840594272863), vec2(-0.6513635358433012, 0.02693592719236743), vec2(0.4754972264501394, -0.4731832495325937), vec2(-0.031835257138575386, 0.688466786710093), vec2(-0.453049781172062, -0.5429050522696827), vec2(0.7180980120770482, 0.09661907188020213), vec2(-0.6087648356726265, 0.42356271654677086), vec2(0.16642998852852006, -0.7397979852083922), vec2(0.3851146497586841, 0.6720764142128826), vec2(-0.7531697715832165, -0.24028169962210072), vec2(0.7318846100577492, -0.3381492533787653), vec2(-0.3171815449473513, 0.7578890865719149), vec2(-0.28316949312590045, -0.7872833277561647), vec2(0.7537116468288211, 0.3961297179391954), vec2(-0.8374205892135652, 0.2207413798117724), vec2(0.4761215505571531, -0.7404784055562001), vec2(0.1514946541949552, 0.881504038419763), vec2(-0.7181191606719344, -0.5561518417445334), vec2(0.9188065931338664, -0.07612124810943162), vec2(-0.6352205632151753, 0.6866548158052892), vec2(0.004627940068221585, -0.9486720097961807), vec2(0.6461984615666002, 0.7123394894774254), vec2(-0.9705201855577097, -0.08994759265832905), vec2(0.7865394272549175, -0.5969553830677015));

// Gaussian weights for each sample point
const float weights[40] = float[40](0.031570946536891895, 0.03117876594041362, 0.03079145708954723, 0.03040895946649678, 0.030031213305230484, 0.02965815958214211, 0.029289740006828435, 0.02892589701298127, 0.028566573749392577, 0.028211714071071377, 0.027861262530470957, 0.027515164368825097, 0.027173365507591907, 0.02683581254000392, 0.026502452722723193, 0.02617323396760005, 0.0258481048335342, 0.02552701451843697, 0.025209912851293363, 0.024896750284322734, 0.024587477885236863, 0.024282047329594172, 0.023980410893248937, 0.023682521444894303, 0.023388332438697895, 0.023097797907028966, 0.022810872453275843, 0.022527511244752624, 0.02224767000569398, 0.021971305010336957, 0.02169837307608877, 0.0214288315567794, 0.02116263833599808, 0.02089975182051248, 0.02064013093376971, 0.02038373510947801, 0.020130524285268205, 0.019880458896433837, 0.019633499869749146, 0.019389608617363767);

// http://www.flong.com/archive/texts/code/shapers_circ/
float doubleCircleSigmoid(float x, float a) {
    a = clamp(a, 0.0, 1.0);

    float y = .0;
    if (x <= a) {
        y = a - sqrt(a * a - x * x);
    } else {
        y = a + sqrt(pow(1. - a, 2.) - pow(x - 1., 2.));
    }
    return y;
}

vec3 rgb2hsl(vec3 col) {
    float red   = col.r;
    float green = col.g;
    float blue  = col.b;

    float minc  = min(col.r, min(col.g, col.b));
    float maxc  = max(col.r, max(col.g, col.b));
    float delta = maxc - minc;

    float lum = (minc + maxc) * 0.5;
    float sat = 0.0;
    float hue = 0.0;

    if (lum > 0.0 && lum < 1.0) {
        float mul = (lum < 0.5) ? (lum) : (1.0 - lum);
        sat       = delta / (mul * 2.0);
    }

    if (delta > 0.0) {
        vec3  maxcVec = vec3(maxc);
        vec3  masks = vec3(equal(maxcVec, col)) * vec3(notEqual(maxcVec, vec3(green, blue, red)));
        vec3  adds = vec3(0.0, 2.0, 4.0) + vec3(green - blue, blue - red, red - green) / delta;

        hue += dot(adds, masks);
        hue /= 6.0;

        if (hue < 0.0)
            hue += 1.0;
    }

    return vec3(hue, sat, lum);
}

vec3 hsl2rgb(vec3 col) {
    const float onethird = 1.0 / 3.0;
    const float twothird = 2.0 / 3.0;
    const float rcpsixth = 6.0;

    float       hue = col.x;
    float       sat = col.y;
    float       lum = col.z;

    vec3        xt = vec3(0.0);

    if (hue < onethird) {
        xt.r = rcpsixth * (onethird - hue);
        xt.g = rcpsixth * hue;
        xt.b = 0.0;
    } else if (hue < twothird) {
        xt.r = 0.0;
        xt.g = rcpsixth * (twothird - hue);
        xt.b = rcpsixth * (hue - onethird);
    } else
        xt = vec3(rcpsixth * (hue - twothird), 0.0, rcpsixth * (1.0 - hue));

    xt = min(xt, 1.0);

    float sat2   = 2.0 * sat;
    float satinv = 1.0 - sat;
    float luminv = 1.0 - lum;
    float lum2m1 = (2.0 * lum) - 1.0;
    vec3  ct     = (sat2 * xt) + satinv;

    vec3  rgb;
    if (lum >= 0.5)
        rgb = (luminv * ct) + lum2m1;
    else
        rgb = lum * ct;

    return rgb;
}

void main() {
    vec2 uv = v_texcoord * 2.0;
    vec2 offset = halfpixel * radius;

    // Horizontal Gaussian blur
    vec4 color = vec4(0, 0, 0, 0);
    for (int i = 0; i < 40; ++i) {
        color += texture(tex, uv + vec2(offset.x * samples[i].x, offset.y * samples[i].y)) * weights[i];
    }

    if (vibrancy == 0.0) {
        fragColor = color;
    } else {
        // Invert it so that it correctly maps to the config setting
        float vibrancy_darkness1 = 1.0 - vibrancy_darkness;

        // Decrease the RGB components based on their perceived brightness, to prevent visually dark colors from overblowing the rest.
        vec3 hsl = rgb2hsl(color.rgb);
        // Calculate perceived brightness, as not boost visually dark colors like deep blue as much as equally saturated yellow
        float perceivedBrightness = doubleCircleSigmoid(sqrt(color.r * color.r * Pr + color.g * color.g * Pg + color.b * color.b * Pb), 0.8 * vibrancy_darkness1);

        float b1        = b * vibrancy_darkness1;
        float boostBase = hsl[1] > 0.0 ? smoothstep(b1 - c * 0.5, b1 + c * 0.5, 1.0 - (pow(1.0 - hsl[1] * cos(a), 2.0) + pow(1.0 - perceivedBrightness * sin(a), 2.0))) : 0.0;

        float saturation = clamp(hsl[1] + (boostBase * vibrancy) / float(passes), 0.0, 1.0);

        vec3  newColor = hsl2rgb(vec3(hsl[0], saturation, hsl[2]));

        fragColor = vec4(newColor, color[3]);
    }
}
)#";

inline const std::string FRAGBLUR2 = R"#(
#version 100
precision highp float;
varying highp vec2 v_texcoord; // is in 0-1
uniform sampler2D tex;

uniform float radius;
uniform vec2 halfpixel;

void main() {
    vec2 uv = v_texcoord / 2.0;

    vec4 sum = texture2D(tex, uv + vec2(-halfpixel.x * 2.0, 0.0) * radius);

    sum += texture2D(tex, uv + vec2(-halfpixel.x, halfpixel.y) * radius) * 2.0;
    sum += texture2D(tex, uv + vec2(0.0, halfpixel.y * 2.0) * radius);
    sum += texture2D(tex, uv + vec2(halfpixel.x, halfpixel.y) * radius) * 2.0;
    sum += texture2D(tex, uv + vec2(halfpixel.x * 2.0, 0.0) * radius);
    sum += texture2D(tex, uv + vec2(halfpixel.x, -halfpixel.y) * radius) * 2.0;
    sum += texture2D(tex, uv + vec2(0.0, -halfpixel.y * 2.0) * radius);
    sum += texture2D(tex, uv + vec2(-halfpixel.x, -halfpixel.y) * radius) * 2.0;

    gl_FragColor = sum / 12.0;
}
)#";

inline const std::string FRAGBLURPREPARE = R"#(
precision         highp float;
varying vec2      v_texcoord; // is in 0-1
uniform sampler2D tex;

uniform float     contrast;
uniform float     brightness;

float gain(float x, float k) {
    float a = 0.5 * pow(2.0 * ((x < 0.5) ? x : 1.0 - x), k);
    return (x < 0.5) ? a : 1.0 - a;
}

void main() {
    vec4 pixColor = texture2D(tex, v_texcoord);

    // contrast
    if (contrast != 1.0) {
        pixColor.r = gain(pixColor.r, contrast);
        pixColor.g = gain(pixColor.g, contrast);
        pixColor.b = gain(pixColor.b, contrast);
    }

    // brightness
    if (brightness > 1.0) {
        pixColor.rgb *= brightness;
    }

    gl_FragColor = pixColor;
}
)#";

inline const std::string FRAGBLURFINISH = R"#(
precision         highp float;
varying vec2      v_texcoord; // is in 0-1
uniform sampler2D tex;

uniform float     noise;
uniform float     brightness;

float hash(vec2 p) {
    vec3 p3 = fract(vec3(p.xyx) * 1689.1984);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

void main() {
    vec4 pixColor = texture2D(tex, v_texcoord);

    // noise
    float noiseHash   = hash(v_texcoord);
    float noiseAmount = (mod(noiseHash, 1.0) - 0.5);
    pixColor.rgb += noiseAmount * noise;

    // brightness
    if (brightness < 1.0) {
        pixColor.rgb *= brightness;
    }

    gl_FragColor = pixColor;
}
)#";

inline const std::string TEXFRAGSRCEXT = R"#(
#extension GL_OES_EGL_image_external : require

precision highp float;
varying vec2 v_texcoord;
uniform samplerExternalOES texture0;
uniform float alpha;

uniform vec2 topLeft;
uniform vec2 fullSize;
uniform float radius;

uniform int discardOpaque;
uniform int discardAlpha;
uniform int discardAlphaValue;

uniform int applyTint;
uniform vec3 tint;

void main() {

    vec4 pixColor = texture2D(texture0, v_texcoord);

    if (discardOpaque == 1 && pixColor[3] * alpha == 1.0)
	discard;

    if (applyTint == 1) {
	pixColor[0] = pixColor[0] * tint[0];
	pixColor[1] = pixColor[1] * tint[1];
	pixColor[2] = pixColor[2] * tint[2];
    }

    if (radius > 0.0) {
    )#" +
    ROUNDED_SHADER_FUNC("pixColor") + R"#(
    }

    gl_FragColor = pixColor * alpha;
}
)#";

static const std::string FRAGGLITCH = R"#(
precision highp float;
varying vec2 v_texcoord;
uniform sampler2D tex;
uniform float time; // quirk: time is set to 0 at the beginning, should be around 10 when crash.
uniform float distort;
uniform vec2 screenSize;

float rand(float co) {
    return fract(sin(dot(vec2(co, co), vec2(12.9898, 78.233))) * 43758.5453);
}

float rand(vec2 co) {
    return fract(sin(dot(co, vec2(12.9898, 78.233))) * 43758.5453);
}

float noise(vec2 point) {
    vec2 floored = floor(point);
    vec2 fractal = fract(point);
    fractal = fractal * fractal * (3.0 - 2.0 * fractal);

    float mixed = mix(
        mix(rand(floored), rand(floored + vec2(1.0, 0.0)), fractal.x),
        mix(rand(floored + vec2(0.0,1.0)), rand(floored + vec2(1.0,1.0)), fractal.x), fractal.y);
    return mixed * mixed;
}

void main() {
    float ABERR_OFFSET = 4.0 * (distort / 5.5) * time;
    float TEAR_AMOUNT = 9000.0 * (1.0 - (distort / 5.5));
    float TEAR_BANDS = 108.0 / 2.0 * (distort / 5.5) * 2.0;
    float MELT_AMOUNT = (distort * 8.0) / screenSize.y;

    float NOISE = abs(mod(noise(v_texcoord) * distort * time * 2.771, 1.0)) * time / 10.0;
    if (time < 2.0)
        NOISE = 0.0;

    float offset = (mod(rand(floor(v_texcoord.y * TEAR_BANDS)) * 318.772 * time, 20.0) - 10.0) / TEAR_AMOUNT;

    vec2 blockOffset = vec2(((abs(mod(rand(floor(v_texcoord.x * 37.162)) * 721.43, 100.0))) - 50.0) / 200000.0 * pow(time, 3.0),
                            ((abs(mod(rand(floor(v_texcoord.y * 45.882)) * 733.923, 100.0))) - 50.0) / 200000.0 * pow(time, 3.0));
    if (time < 3.0)
        blockOffset = vec2(0,0);

    float meltSeed = abs(mod(rand(floor(v_texcoord.x * screenSize.x * 17.719)) * 281.882, 1.0));
    if (meltSeed < 0.8) {
        meltSeed = 0.0;
    } else {
        meltSeed *= 25.0 * NOISE;
    }
    float meltAmount = MELT_AMOUNT * meltSeed;

    vec2 pixCoord = vec2(v_texcoord.x + offset + NOISE * 3.0 / screenSize.x + blockOffset.x, v_texcoord.y - meltAmount + 0.02 * NOISE / screenSize.x + NOISE * 3.0 / screenSize.y  + blockOffset.y);

    vec4 pixColor = texture2D(tex, pixCoord);
    vec4 pixColorLeft = texture2D(tex, pixCoord + vec2(ABERR_OFFSET / screenSize.x, 0));
    vec4 pixColorRight = texture2D(tex, pixCoord + vec2(-ABERR_OFFSET / screenSize.x, 0));

    pixColor[0] = pixColorLeft[0];
    pixColor[2] = pixColorRight[2];

    pixColor[0] += distort / 90.0;

    gl_FragColor = pixColor;
}
)#";
